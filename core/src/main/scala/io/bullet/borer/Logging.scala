/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import java.lang.{StringBuilder ⇒ JStringBuilder}
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
object Logging {

  def afterValidation(createLogger: LevelInfo ⇒ Logger): Receiver.Applier =
    (creator, target) ⇒ creator(new Receiver(target, createLogger))

  def beforeValidation(createLogger: LevelInfo ⇒ Logger): Receiver.Applier =
    (creator, target) ⇒ new Receiver(creator(target), createLogger)

  trait LevelInfo {
    def level: Int
    def levelCount: Long
    def levelSize: Long
    def levelType: LevelType
    final def isUnbounded: Boolean = levelSize < 0
  }

  sealed trait LevelType
  object LevelType {
    final case object Array               extends LevelType
    final case object UnboundedByteString extends LevelType
    final case object UnboundedTextString extends LevelType

    sealed trait MapEntry      extends LevelType
    final case object MapKey   extends MapEntry
    final case object MapValue extends MapEntry
  }

  trait Logger {
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
    def onChars(length: Int, buffer: Array[Char]): Unit
    def onText[Bytes: ByteAccess](value: Bytes): Unit
    def onTextStart(): Unit
    def onArrayHeader(length: Long): Unit
    def onArrayStart(): Unit
    def onMapHeader(length: Long): Unit
    def onMapStart(): Unit
    def onTag(value: Tag): Unit
    def onSimpleValue(value: Int): Unit
    def onLevelExited(levelType: LevelType, break: Boolean): Unit
    def onEndOfInput(): Unit
  }

  /**
    * A [[Logger]] which formats each incoming element to it's own log line.
    */
  abstract class LineFormatLogger extends Logger {
    import java.lang.Long.toHexString

    def onNull(): Unit                                   = show("null")
    def onUndefined(): Unit                              = show("undefined")
    def onBool(value: Boolean): Unit                     = show(value.toString)
    def onInt(value: Int): Unit                          = show(value.toString)
    def onLong(value: Long): Unit                        = show(s"${value}L")
    def onOverLong(negative: Boolean, value: Long): Unit = show((if (negative) "-0x" else "0x") + toHexString(value))
    def onFloat16(value: Float): Unit                    = show(s"${Util.doubleToString(value.toDouble)}f16")
    def onFloat(value: Float): Unit                      = show(s"${Util.doubleToString(value.toDouble)}f")
    def onDouble(value: Double): Unit                    = show(Util.doubleToString(value))
    def onDecimal(integer: Long, fraction: Int): Unit    = show(s"$integer.${fraction}d")
    def onNumberString(value: String): Unit              = show(value + 's')
    def onBytes[Bytes: ByteAccess](value: Bytes): Unit   = show(formatBytes("BYTES[", value))
    def onBytesStart(): Unit                             = show("BYTES-STREAM[")
    def onString(value: String): Unit                    = show(formatString(value))
    def onChars(length: Int, buffer: Array[Char]): Unit  = show(formatString(new String(buffer, 0, length)))
    def onText[Bytes: ByteAccess](value: Bytes): Unit    = show(formatString(value))
    def onTextStart(): Unit                              = show("TEXT-STREAM[")
    def onArrayHeader(length: Long): Unit                = show(if (length > 0) "[" else "[]")
    def onArrayStart(): Unit                             = show("[")
    def onMapHeader(length: Long): Unit                  = show(if (length > 0) "{" else "{}")
    def onMapStart(): Unit                               = show("{")
    def onTag(value: Tag): Unit                          = show(value.toString)
    def onSimpleValue(value: Int): Unit                  = show(s"SimpleValue($value)")

    def onLevelExited(levelType: LevelType, break: Boolean): Unit =
      show(if (levelType.isInstanceOf[LevelType.MapEntry]) "}" else "]")

    def onEndOfInput(): Unit = show("END")

    def formatBytes[Bytes](opener: String, value: Bytes)(implicit ba: ByteAccess[Bytes]): String =
      ba.toByteArray(value)
        .take(maxShownByteArrayPrefixLen)
        .map(x ⇒ f"${x & 0xFF}%02X")
        .mkString(opener, " ", if (ba.sizeOf(value) > maxShownByteArrayPrefixLen) " ...]" else "]")

    def formatString[Bytes](value: Bytes)(implicit ba: ByteAccess[Bytes]): String =
      formatString(new String(ba.toByteArray(value), UTF_8))

    def formatString(value: String): String =
      if (value.length > maxShownStringPrefixLen) {
        "\"" + value.substring(0, maxShownStringPrefixLen) + "...\""
      } else "\"" + value + '"'

    def show(item: String): Unit = {
      val sb = new java.lang.StringBuilder
      for (_ ← 0 until info.level) sb.append("    ")
      val levelCount = info.levelCount.toString
      val levelSize  = info.levelSize
      if (levelSize >= 0) {
        val s = levelSize.toString
        for (_ ← 0 until (s.length - levelCount.length)) sb.append(' ')
        sb.append(levelCount).append('/').append(s)
      } else sb.append(levelCount)
      sb.append(": ")
      if (info.levelType == LevelType.MapValue && item != "]" && item != "}") sb.append("-> ")
      sb.append(item)
      showLine(sb.toString)
    }

    def info: LevelInfo
    def maxShownByteArrayPrefixLen: Int
    def maxShownStringPrefixLen: Int
    def showLine(line: String): Unit
  }

  def PrintLogger(maxShownByteArrayPrefixLen: Int = 20, maxShownStringPrefixLen: Int = 50): LevelInfo ⇒ PrintLogger =
    new PrintLogger(maxShownByteArrayPrefixLen, maxShownStringPrefixLen, _)

  /**
    * A [[LineFormatLogger]] that simply prints all lines to the console.
    */
  final class PrintLogger(val maxShownByteArrayPrefixLen: Int, val maxShownStringPrefixLen: Int, val info: LevelInfo)
      extends LineFormatLogger {
    def showLine(line: String): Unit = println(line)
  }

  def ToStringLogger(stringBuilder: JStringBuilder,
                     maxShownByteArrayPrefixLen: Int = 20,
                     maxShownStringPrefixLen: Int = 50,
                     lineSeparator: String = System.lineSeparator()): LevelInfo ⇒ ToStringLogger =
    new ToStringLogger(stringBuilder, maxShownByteArrayPrefixLen, maxShownStringPrefixLen, lineSeparator, _)

  /**
    * A [[LineFormatLogger]] that appends all lines to a given [[JStringBuilder]].
    */
  final class ToStringLogger(val stringBuilder: JStringBuilder,
                             val maxShownByteArrayPrefixLen: Int,
                             val maxShownStringPrefixLen: Int,
                             val lineSeparator: String,
                             val info: LevelInfo)
      extends LineFormatLogger {
    def showLine(line: String): Unit = stringBuilder.append(line).append(lineSeparator)
  }

  /**
    * A [[Receiver]] which forwards all incoming data item to another [[Receiver]] and,
    * on the side, feeds a custom [[Logger]] with logging events.
    */
  final class Receiver(private var _target: borer.Receiver, createLogger: LevelInfo ⇒ Logger)
      extends borer.Receiver with LevelInfo with java.lang.Cloneable {

    private var _level: Int = 0

    // >= 0 if bounded, < 0 if unbounded (the count is then ~x)
    private var _levelCount = new Array[Long](4)

    // if bounded: >= 0 if in array, < 0 if in map (the size is then ~x)
    // if unbounded: 0 if array, 1 if map, 2 if bytes, 3 if text
    private var _levelSize = new Array[Long](4)

    _levelCount(0) = -1

    private var logger = createLogger(this)

    override def target = _target

    def level = _level

    def levelCount = {
      val count = _levelCount(_level)
      val size  = _levelSize(_level)
      val rawCount = if (count >= 0) {
        if (size >= 0) count // bounded array
        else count >> 1 // bounded map
      } else {
        if (size == 1) ~count >> 1 // unbounded map
        else ~count // unbounded array, bytes or text
      }
      rawCount + 1
    }

    def levelSize = {
      val count = _levelCount(_level)
      if (count >= 0) {
        val size = _levelSize(_level)
        if (size >= 0) size // bounded array
        else ~size >> 1 // bounded map
      } else -1 // unbounded something
    }

    def levelType = {
      val count = _levelCount(_level)
      val size  = _levelSize(_level)
      if (count >= 0) {
        if (size >= 0) LevelType.Array
        else if ((count & 1) == 0) LevelType.MapKey
        else LevelType.MapValue
      } else
        size match {
          case 0 ⇒ LevelType.Array
          case 1 ⇒ if ((count & 1) != 0) LevelType.MapKey else LevelType.MapValue
          case 2 ⇒ LevelType.UnboundedByteString
          case 3 ⇒ LevelType.UnboundedTextString
        }
    }

    def onNull(): Unit = {
      logger.onNull()
      count()
      target.onNull()
    }

    def onUndefined(): Unit = {
      logger.onUndefined()
      count()
      target.onUndefined()
    }

    def onBool(value: Boolean): Unit = {
      logger.onBool(value)
      count()
      target.onBool(value)
    }

    def onInt(value: Int): Unit = {
      logger.onInt(value)
      count()
      target.onInt(value)
    }

    def onLong(value: Long): Unit = {
      logger.onLong(value)
      count()
      target.onLong(value)
    }

    def onOverLong(negative: Boolean, value: Long): Unit = {
      logger.onOverLong(negative, value)
      count()
      target.onOverLong(negative, value)
    }

    def onFloat16(value: Float): Unit = {
      logger.onFloat16(value)
      count()
      target.onFloat16(value)
    }

    def onFloat(value: Float): Unit = {
      logger.onFloat(value)
      count()
      target.onFloat(value)
    }

    def onDouble(value: Double): Unit = {
      logger.onDouble(value)
      count()
      target.onDouble(value)
    }

    def onDecimal(integer: Long, fraction: Int): Unit = {
      logger.onDecimal(integer, fraction)
      count()
      target.onDecimal(integer, fraction)
    }

    def onNumberString(value: String): Unit = {
      logger.onNumberString(value)
      count()
      target.onNumberString(value)
    }

    def onBytes[Bytes: ByteAccess](value: Bytes): Unit = {
      logger.onBytes(value)
      count()
      target.onBytes(value)
    }

    def onBytesStart(): Unit = {
      logger.onBytesStart()
      enterLevel(count = -1, size = 2)
      target.onBytesStart()
    }

    def onString(value: String): Unit = {
      logger.onString(value)
      count()
      target.onString(value)
    }

    def onChars(length: Int, buffer: Array[Char]): Unit = {
      logger.onChars(length, buffer)
      count()
      target.onChars(length, buffer)
    }

    def onText[Bytes: ByteAccess](value: Bytes): Unit = {
      logger.onText(value)
      count()
      target.onText(value)
    }

    def onTextStart(): Unit = {
      logger.onTextStart()
      enterLevel(count = -1, size = 3)
      target.onTextStart()
    }

    def onArrayHeader(length: Long): Unit = {
      logger.onArrayHeader(length)
      if (length > 0) enterLevel(count = 0, size = length) else count()
      target.onArrayHeader(length)
    }

    def onArrayStart(): Unit = {
      logger.onArrayStart()
      enterLevel(count = -1, size = 0)
      target.onArrayStart()
    }

    def onMapHeader(length: Long): Unit = {
      logger.onMapHeader(length)
      if (length > 0) enterLevel(count = 0, size = ~(length << 1)) else count()
      target.onMapHeader(length)
    }

    def onMapStart(): Unit = {
      logger.onMapStart()
      enterLevel(count = -1, size = 1)
      target.onMapStart()
    }

    def onBreak(): Unit = {
      val exitedLevelType = levelType
      exitLevel()
      logger.onLevelExited(exitedLevelType, break = true)
      count() // level-entering items are only counted when the level is exited, not when they are entered
      target.onBreak()
    }

    def onTag(value: Tag): Unit = {
      logger.onTag(value)
      target.onTag(value)
    }

    def onSimpleValue(value: Int): Unit = {
      logger.onSimpleValue(value)
      count()
      target.onSimpleValue(value)
    }

    def onEndOfInput(): Unit = {
      logger.onEndOfInput()
      target.onEndOfInput()
    }

    override def copy = {
      val clone = super.clone().asInstanceOf[Receiver]
      clone._target = _target.copy
      clone._levelCount = _levelCount.clone()
      clone._levelSize = _levelSize.clone()
      clone.logger = createLogger(clone)
      clone
    }

    @tailrec private def count(): Unit = {
      val cnt = _levelCount(_level)
      if (cnt >= 0) {
        // bounded array or map
        val newCount = cnt + 1
        val rawSize  = _levelSize(_level)
        val size     = if (rawSize >= 0) rawSize else ~rawSize
        if (newCount == size) {
          val exitedLevelType = levelType
          exitLevel()
          logger.onLevelExited(exitedLevelType, break = false)
          count() // level-entering items are only counted when the level is exited, not when they are entered
        } else _levelCount(_level) = newCount
      } else _levelCount(_level) = cnt - 1 // unbounded something
    }

    private def enterLevel(count: Long, size: Long): Unit = {
      val newLevel = _level + 1
      if (newLevel == _levelCount.length) {
        val l2     = newLevel << 1
        val newLen = if (l2 >= 0) l2 else Int.MaxValue // overflow protection
        _levelCount = util.Arrays.copyOf(_levelCount, newLen)
        _levelSize = util.Arrays.copyOf(_levelSize, newLen)
      }
      _level = newLevel
      _levelCount(newLevel) = count
      _levelSize(newLevel) = size
    }

    private def exitLevel(): Unit = _level -= 1
  }
}
