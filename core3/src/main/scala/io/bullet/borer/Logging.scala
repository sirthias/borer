/*
 * Copyright (c) 2019-2021 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import java.lang.{StringBuilder => JStringBuilder}
import java.nio.charset.StandardCharsets.UTF_8
import java.util
import io.bullet.borer
import io.bullet.borer.internal.Util

import scala.annotation.tailrec

/**
 * Facilities for on-the-side logging of either encoding or decoding progress,
 * which can be useful for debugging problems with the input or custom codec logic.
 *
 * Logging can be done either before or after the input validation step, depending on your needs.
 * If unsure, go for after-validation logging, at least initially. (This is also the default.)
 * For example, to log decoding progress to the console you can say:
 *
 * {{{
 * Cbor.decode(inputBytes).withPrintLogging().to[MyType]
 * }}}
 */
object Logging:

  def transformer[Config](createLogger: LevelInfo => Logger): borer.Receiver.Transformer[Config] =
    (target, _) => new Receiver(target, createLogger)

  trait LevelInfo:

    /**
     * The zero-based nesting level of the current item.
     */
    def level: Int

    /**
     * The one-based index number of the current item within the current level.
     */
    def levelCount: Long

    /**
     * The number of total items on the current level.
     * -1 if the current level is unbounded.
     */
    def levelSize: Long

    /**
     * The type of the current item.
     */
    def elementType: ElementType

    final def isUnbounded: Boolean = levelSize < 0

  sealed trait ElementType

  object ElementType:
    case object ArrayElement               extends ElementType
    case object UnboundedByteStringElement extends ElementType
    case object UnboundedTextStringElement extends ElementType

    sealed trait MapEntry extends ElementType
    case object MapKey    extends MapEntry
    case object MapValue  extends MapEntry

  trait Logger:
    def onNull(): Unit
    def onUndefined(): Unit
    def onBool(value: Boolean): Unit
    def onInt(value: Int): Unit
    def onLong(value: Long): Unit
    def onOverLong(negative: Boolean, value: Long): Unit
    def onFloat16(value: Float): Unit
    def onFloat(value: Float): Unit
    def onDouble(value: Double): Unit
    def onDecimal(integer: Long, fraction: Int): Unit
    def onNumberString(value: String): Unit
    def onBytes[Bytes: ByteAccess](value: Bytes): Unit
    def onBytesStart(): Unit
    def onString(value: String): Unit
    def onChars(buffer: Array[Char], length: Int): Unit
    def onText[Bytes: ByteAccess](value: Bytes): Unit
    def onTextStart(): Unit
    def onArrayHeader(length: Long): Unit
    def onArrayStart(): Unit
    def onMapHeader(length: Long): Unit
    def onMapStart(): Unit
    def onTag(value: Tag): Unit
    def onSimpleValue(value: Int): Unit
    def onLevelExited(levelType: ElementType, break: Boolean): Unit
    def onEndOfInput(): Unit

  /**
   * A [[Logger]] which formats each incoming element to it's own log line.
   */
  abstract class LineFormatLogger extends Logger:
    protected var gutterWidth: Int = _

    def info: LevelInfo

    def maxShownByteArrayPrefixLen: Int
    def maxShownStringPrefixLen: Int
    def maxShownArrayElems: Int
    def maxShownMapEntries: Int
    def initialGutterWidth: Int
    def renderLevelCount: Boolean
    def renderEndOfInput: Boolean
    def renderCommas: Boolean
    def indentation: String
    def mapKeySep: String

    def renderLine(line: String): Unit

    def onNull(): Unit                                   = render("null")
    def onUndefined(): Unit                              = render("undefined")
    def onBool(value: Boolean): Unit                     = render(value.toString)
    def onInt(value: Int): Unit                          = render(value.toString)
    def onLong(value: Long): Unit                        = render(s"${value}L")
    def onOverLong(negative: Boolean, value: Long): Unit = render(formatOverLong(negative, value))
    def onFloat16(value: Float): Unit                    = render(s"${Util.doubleToString(value.toDouble)}f16")
    def onFloat(value: Float): Unit                      = render(s"${Util.doubleToString(value.toDouble)}f")
    def onDouble(value: Double): Unit                    = render(s"${Util.doubleToString(value)}d")
    def onDecimal(integer: Long, fraction: Int): Unit    = render(s"$integer.${fraction}d")
    def onNumberString(value: String): Unit              = render(s"${value}s")
    def onBytes[Bytes: ByteAccess](value: Bytes): Unit   = render(formatBytes("B[", value))
    def onBytesStart(): Unit                             = render("B*[", levelOpen = true)
    def onString(value: String): Unit                    = render(formatString(value))
    def onChars(buffer: Array[Char], length: Int): Unit  = render(formatString(new String(buffer, 0, length)))
    def onText[Bytes: ByteAccess](value: Bytes): Unit    = render(formatString(value))
    def onTextStart(): Unit                              = render("T*[", levelOpen = true)
    def onArrayStart(): Unit                             = render("*[", levelOpen = true)
    def onMapStart(): Unit                               = render("*{", levelOpen = true)
    def onTag(value: Tag): Unit                          = render(s"@$value")
    def onSimpleValue(value: Int): Unit                  = render(s"Simple($value)")
    def onEndOfInput(): Unit                             = if (renderEndOfInput) render("END")

    def onArrayHeader(length: Long): Unit = if (length > 0) render("[", levelOpen = true) else render("[]")

    def onMapHeader(length: Long): Unit = if (length > 0) render("{", levelOpen = true) else render("{}")

    def onLevelExited(levelType: ElementType, break: Boolean): Unit =
      render(if (levelType.isInstanceOf[ElementType.MapEntry]) "}" else "]", levelClose = true)

    def formatBytes[Bytes](opener: String, value: Bytes)(implicit ba: ByteAccess[Bytes]): String =
      ba.toByteArray(value)
        .take(maxShownByteArrayPrefixLen)
        .map(x => f"${x & 0xFF}%02X")
        .mkString(opener, " ", if (ba.sizeOf(value) > maxShownByteArrayPrefixLen) " ...]" else "]")

    def formatOverLong(negative: Boolean, value: Long): String =
      val x = new java.math.BigInteger(1, Util.toBigEndianBytes(value))
      val y = if (negative) x.not else x
      s"${y.toString}LL"

    def formatString[Bytes](value: Bytes)(implicit ba: ByteAccess[Bytes]): String =
      formatString(new String(ba.toByteArray(value), UTF_8))

    def formatString(value: String): String =
      val sb      = new java.lang.StringBuilder
      val tooLong = value.length > maxShownStringPrefixLen
      val string  = if (tooLong) value.substring(0, maxShownStringPrefixLen) else value
      string.foldLeft(sb.append('"'))(appendChar).append(if (tooLong) "...\"" else "\"").toString

    def appendChar(sb: JStringBuilder, char: Char): JStringBuilder =
      char match
        case '"'                            => sb.append("\\\"")
        case '\t'                           => sb.append("\\t")
        case '\r'                           => sb.append("\\r")
        case '\n'                           => sb.append("\\n")
        case x if Character.isISOControl(x) => sb.append("\\u%04x".format(x.toInt))
        case x                              => sb.append(x)

    def appendIndent(sb: JStringBuilder, level: Int): JStringBuilder =
      @tailrec def rec(remaining: Int, sb: JStringBuilder): JStringBuilder =
        if (remaining > 0) rec(remaining - 1, sb.append(indentation)) else sb
      rec(level, sb)

    def appendLevelCount(sb: JStringBuilder, count: Long, size: Long): JStringBuilder =
      @tailrec def pad(remaining: Int, sb: JStringBuilder): JStringBuilder =
        if (remaining > 0) pad(remaining - 1, sb.append(' ')) else sb
      val countStr = count.toString
      gutterWidth = math.max(gutterWidth, initialGutterWidth)
      if (size >= 0)
        val sizeStr = size.toString
        gutterWidth = math.max(gutterWidth, sizeStr.length * 2 + 1)
        pad(gutterWidth - sizeStr.length - countStr.length - 1, sb).append(countStr)
        sb.append('/').append(sizeStr)
      else
        gutterWidth = math.max(gutterWidth, countStr.length)
        pad(gutterWidth - countStr.length, sb).append(countStr)
      sb.append('|').append(' ')

    def render(item: String, levelOpen: Boolean = false, levelClose: Boolean = false): Unit =
      val inf           = info
      val level         = inf.level
      val levelCount    = inf.levelCount
      val levelSize     = inf.levelSize
      val levelType     = inf.elementType
      val maxShown      = if (levelType.isInstanceOf[ElementType.MapEntry]) maxShownMapEntries else maxShownArrayElems
      val shownHalf     = maxShown >> 1
      val ellipsisCount = levelSize - shownHalf
      if (levelSize <= maxShown || levelCount <= shownHalf + (maxShown & 1) || levelCount > ellipsisCount)
        val sb = new JStringBuilder
        if (renderLevelCount) appendLevelCount(sb, levelCount, levelSize)
        appendIndent(sb, level)
        if (levelType == ElementType.MapValue && !levelClose) sb.append(mapKeySep)
        sb.append(item)
        if (renderCommas && level > 0 && !levelOpen && levelType != ElementType.MapKey &&
          (levelSize < 0 || levelCount < levelSize)) sb.append(',')
        renderLine(sb.toString)
      else if (levelCount == ellipsisCount && levelType != ElementType.MapKey)
        val sb = new JStringBuilder
        appendIndent(sb, level)
        sb.append("...")
        renderLine(sb.toString)

  /**
   * A [[LineFormatLogger]] that simply prints all lines to the console.
   */
  class PrintLogger(
      val info: LevelInfo,
      val maxShownByteArrayPrefixLen: Int,
      val maxShownStringPrefixLen: Int,
      val maxShownArrayElems: Int,
      val maxShownMapEntries: Int,
      val initialGutterWidth: Int,
      val renderLevelCount: Boolean,
      val renderEndOfInput: Boolean,
      val renderCommas: Boolean,
      val indentation: String,
      val mapKeySep: String)
      extends LineFormatLogger:

    def renderLine(line: String): Unit = println(line)

  /**
   * A [[LineFormatLogger]] that appends all lines to a given [[JStringBuilder]].
   */
  class ToStringLogger(
      val info: LevelInfo,
      val stringBuilder: JStringBuilder,
      val maxShownByteArrayPrefixLen: Int,
      val maxShownStringPrefixLen: Int,
      val maxShownArrayElems: Int,
      val maxShownMapEntries: Int,
      val initialGutterWidth: Int,
      val renderLevelCount: Boolean,
      val renderEndOfInput: Boolean,
      val renderCommas: Boolean,
      val indentation: String,
      val mapKeySep: String,
      val lineSep: String,
      val mapValueOnNewLine: Boolean)
      extends LineFormatLogger:

    def renderLine(line: String): Unit =
      stringBuilder.append(line)
      if (mapValueOnNewLine || info.elementType != ElementType.MapKey) stringBuilder.append(lineSep)

    override def appendIndent(sb: JStringBuilder, level: Int) =
      if (mapValueOnNewLine || lineStart) super.appendIndent(sb, level) else sb

    override def appendLevelCount(sb: JStringBuilder, count: Long, size: Long) =
      if (mapValueOnNewLine || lineStart) super.appendLevelCount(sb, count, size) else sb

    private def lineStart: Boolean =
      stringBuilder.length() == 0 || lineSep.isEmpty || stringBuilder.charAt(stringBuilder.length() - 1) == lineSep.last

  /**
   * A [[Receiver]] which forwards all incoming data item to another [[Receiver]] and,
   * on the side, feeds a custom [[Logger]] with logging events.
   */
  final class Receiver(target: borer.Receiver, createLogger: LevelInfo => Logger) extends borer.Receiver with LevelInfo:

    private val logger = createLogger(this)

    private var _level: Int = 0

    // >= 0 if bounded, < 0 if unbounded (the count is then ~x)
    private var _levelCount = new Array[Long](4)

    // if bounded: >= 0 if in array, < 0 if in map (the size is then ~x)
    // if unbounded: 0 if array, 1 if map, 2 if bytes, 3 if text
    private var _levelSize = new Array[Long](4)

    _levelCount(0) = -1

    def level = _level

    def levelCount =
      if (_level >= 0)
        val count = _levelCount(_level)
        val size  = _levelSize(_level)
        val rawCount =
          if (count >= 0)
            if (size >= 0) count          // bounded array
            else count >> 1               // bounded map
          else if (size == 1) ~count >> 1 // unbounded map
          else ~count                     // unbounded array, bytes or text
        rawCount + 1
      else 0

    def levelSize =
      if (_level >= 0)
        val count = _levelCount(_level)
        if (count >= 0)
          val size = _levelSize(_level)
          if (size >= 0) size // bounded array
          else ~size >> 1     // bounded map
        else -1               // unbounded something
      else -1

    def elementType =
      if (_level >= 0)
        val count = _levelCount(_level)
        val size  = _levelSize(_level)
        if (count >= 0)
          if (size >= 0) ElementType.ArrayElement
          else if ((count & 1) == 0) ElementType.MapKey
          else ElementType.MapValue
        else
          size match
            case 0 => ElementType.ArrayElement
            case 1 => if ((count & 1) != 0) ElementType.MapKey else ElementType.MapValue
            case 2 => ElementType.UnboundedByteStringElement
            case 3 => ElementType.UnboundedTextStringElement
            case _ => throw new IllegalStateException
      else ElementType.ArrayElement

    def onNull(): Unit =
      logger.onNull()
      count()
      target.onNull()

    def onUndefined(): Unit =
      logger.onUndefined()
      count()
      target.onUndefined()

    def onBoolean(value: Boolean): Unit =
      logger.onBool(value)
      count()
      target.onBoolean(value)

    def onInt(value: Int): Unit =
      logger.onInt(value)
      count()
      target.onInt(value)

    def onLong(value: Long): Unit =
      logger.onLong(value)
      count()
      target.onLong(value)

    def onOverLong(negative: Boolean, value: Long): Unit =
      logger.onOverLong(negative, value)
      count()
      target.onOverLong(negative, value)

    def onFloat16(value: Float): Unit =
      logger.onFloat16(value)
      count()
      target.onFloat16(value)

    def onFloat(value: Float): Unit =
      logger.onFloat(value)
      count()
      target.onFloat(value)

    def onDouble(value: Double): Unit =
      logger.onDouble(value)
      count()
      target.onDouble(value)

    def onNumberString(value: String): Unit =
      logger.onNumberString(value)
      count()
      target.onNumberString(value)

    def onBytes[Bytes: ByteAccess](value: Bytes): Unit =
      logger.onBytes(value)
      count()
      target.onBytes(value)

    def onBytesStart(): Unit =
      logger.onBytesStart()
      enterLevel(count = -1, size = 2)
      target.onBytesStart()

    def onString(value: String): Unit =
      logger.onString(value)
      count()
      target.onString(value)

    def onChars(buffer: Array[Char], length: Int): Unit =
      logger.onChars(buffer, length)
      count()
      target.onChars(buffer, length)

    def onText[Bytes: ByteAccess](value: Bytes): Unit =
      logger.onText(value)
      count()
      target.onText(value)

    def onTextStart(): Unit =
      logger.onTextStart()
      enterLevel(count = -1, size = 3)
      target.onTextStart()

    def onArrayHeader(length: Long): Unit =
      logger.onArrayHeader(length)
      if (length > 0) enterLevel(count = 0, size = length) else count()
      target.onArrayHeader(length)

    def onArrayStart(): Unit =
      logger.onArrayStart()
      enterLevel(count = -1, size = 0)
      target.onArrayStart()

    def onMapHeader(length: Long): Unit =
      logger.onMapHeader(length)
      if (length > 0) enterLevel(count = 0, size = ~(length << 1)) else count()
      target.onMapHeader(length)

    def onMapStart(): Unit =
      logger.onMapStart()
      enterLevel(count = -1, size = 1)
      target.onMapStart()

    def onBreak(): Unit =
      val exitedLevelType = elementType
      exitLevel()
      logger.onLevelExited(exitedLevelType, break = true)
      count() // level-entering items are only counted when the level is exited, not when they are entered
      target.onBreak()

    def onTag(value: Tag): Unit =
      logger.onTag(value)
      target.onTag(value)

    def onSimpleValue(value: Int): Unit =
      logger.onSimpleValue(value)
      count()
      target.onSimpleValue(value)

    def onEndOfInput(): Unit =
      logger.onEndOfInput()
      target.onEndOfInput()

    @tailrec private def count(): Unit =
      if (_level >= 0)
        val cnt = _levelCount(_level)
        if (cnt >= 0)
          // bounded array or map
          val newCount = cnt + 1
          val rawSize  = _levelSize(_level)
          val size     = if (rawSize >= 0) rawSize else ~rawSize
          if (newCount == size)
            val exitedLevelType = elementType
            exitLevel()
            logger.onLevelExited(exitedLevelType, break = false)
            count() // level-entering items are only counted when the level is exited, not when they are entered
          else _levelCount(_level) = newCount
        else _levelCount(_level) = cnt - 1 // unbounded something

    private def enterLevel(count: Long, size: Long): Unit =
      val newLevel = _level + 1
      if (newLevel == _levelCount.length)
        val l2     = newLevel << 1
        val newLen = if (l2 >= 0) l2 else Int.MaxValue // overflow protection
        _levelCount = util.Arrays.copyOf(_levelCount, newLen)
        _levelSize = util.Arrays.copyOf(_levelSize, newLen)
      _level = newLevel
      _levelCount(newLevel) = count
      _levelSize(newLevel) = size

    private def exitLevel(): Unit = _level -= 1
