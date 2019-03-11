/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import java.lang.{StringBuilder ⇒ JStringBuilder}
import java.nio.charset.StandardCharsets
import java.util

import io.bullet.borer

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

  def afterValidation[IO](createLogger: LevelInfo ⇒ Logger[IO]): Receiver.Applier[IO] =
    (creator, target) ⇒ creator(new Receiver(target, createLogger))

  def beforeValidation[IO](createLogger: LevelInfo ⇒ Logger[IO]): Receiver.Applier[IO] =
    (creator, target) ⇒ new Receiver(creator(target), createLogger)

  abstract class LevelInfo {
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

  trait Logger[-IO] {
    def onNull(io: IO): Unit
    def onUndefined(io: IO): Unit
    def onBool(io: IO, value: Boolean): Unit
    def onInt(io: IO, value: Int): Unit
    def onLong(io: IO, value: Long): Unit
    def onPosOverLong(io: IO, value: Long): Unit
    def onNegOverLong(io: IO, value: Long): Unit
    def onFloat16(io: IO, value: Float): Unit
    def onFloat(io: IO, value: Float): Unit
    def onDouble(io: IO, value: Double): Unit
    def onBytes[Bytes: ByteAccess](io: IO, value: Bytes): Unit
    def onBytesStart(io: IO): Unit
    def onText[Bytes: ByteAccess](io: IO, value: Bytes): Unit
    def onTextStart(io: IO): Unit
    def onArrayHeader(io: IO, length: Long): Unit
    def onArrayStart(io: IO): Unit
    def onMapHeader(io: IO, length: Long): Unit
    def onMapStart(io: IO): Unit
    def onTag(io: IO, value: Tag): Unit
    def onSimpleValue(io: IO, value: Int): Unit
    def onLevelExited(io: IO, levelType: LevelType, break: Boolean): Unit
    def onEndOfInput(io: IO): Unit
  }

  /**
    * A [[Logger]] which formats each incoming element to it's own log line.
    */
  abstract class LineFormatLogger[-IO] extends Logger[IO] {
    showLine("RESET")

    def onNull(io: IO)                                   = show("null")
    def onUndefined(io: IO)                              = show("undefined")
    def onBool(io: IO, value: Boolean)                   = show(value.toString)
    def onInt(io: IO, value: Int)                        = show(value.toString)
    def onLong(io: IO, value: Long)                      = show(s"${value}L")
    def onPosOverLong(io: IO, value: Long)               = show("0x" + java.lang.Long.toHexString(value))
    def onNegOverLong(io: IO, value: Long)               = show("-0x" + java.lang.Long.toHexString(value))
    def onFloat16(io: IO, value: Float)                  = show(s"${value}f16")
    def onFloat(io: IO, value: Float)                    = show(s"${value}f")
    def onDouble(io: IO, value: Double)                  = show(value.toString)
    def onBytes[Bytes: ByteAccess](io: IO, value: Bytes) = show(formatBytes("BYTES[", value))
    def onBytesStart(io: IO)                             = show("BYTES-STREAM[")
    def onText[Bytes: ByteAccess](io: IO, value: Bytes)  = show(formatString(value))
    def onTextStart(io: IO)                              = show("TEXT-STREAM[")
    def onArrayHeader(io: IO, length: Long)              = show(if (length > 0) "[" else "[]")
    def onArrayStart(io: IO)                             = show("[")
    def onMapHeader(io: IO, length: Long)                = show(if (length > 0) "{" else "{}")
    def onMapStart(io: IO)                               = show("{")
    def onTag(io: IO, value: Tag)                        = show(value.toString)
    def onSimpleValue(io: IO, value: Int)                = show(s"SimpleValue($value)")
    def onLevelExited(io: IO, levelType: LevelType, break: Boolean) =
      show(if (levelType.isInstanceOf[LevelType.MapEntry]) "}" else "]")
    def onEndOfInput(io: IO) = show("END")

    def formatBytes[Bytes](opener: String, value: Bytes)(implicit ba: ByteAccess[Bytes]): String =
      ba.toByteArray(value)
        .take(maxShownByteArrayPrefixLen)
        .map(x ⇒ f"${x & 0xFF}%02X")
        .mkString(opener, " ", if (ba.sizeOf(value) > maxShownByteArrayPrefixLen) " ...]" else "]")

    def formatString[Bytes](value: Bytes)(implicit ba: ByteAccess[Bytes]): String = {
      val s = new String(ba.toByteArray(value), StandardCharsets.UTF_8)
      if (s.length > maxShownStringPrefixLen) {
        "\"" + s.substring(0, maxShownStringPrefixLen) + "...\""
      } else "\"" + s + '"'
    }

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
      extends LineFormatLogger[Any] {
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
      extends LineFormatLogger[Any] {
    def showLine(line: String): Unit = stringBuilder.append(line).append(lineSeparator)
  }

  /**
    * A [[Receiver]] which forwards all incoming data item to another [[Receiver]] and,
    * on the side, feeds a custom [[Logger]] with logging events.
    */
  final class Receiver[IO](private var _target: borer.Receiver[IO], createLogger: LevelInfo ⇒ Logger[IO])
      extends LevelInfo with borer.Receiver[IO] with java.lang.Cloneable {

    private var _level: Int = 0

    // >= 0 if bounded, < 0 if unbounded (the count is then ~x)
    private var _levelCount = new Array[Long](4)

    // if bounded: >= 0 if in array, < 0 if in map (the size is then ~x)
    // if unbounded: 0 if array, 1 if map, 2 if bytes, 3 if text
    private var _levelSize = new Array[Long](4)

    _levelCount(0) = -1

    private var logger = createLogger(this)

    def target = _target

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

    def onNull(io: IO): IO = {
      logger.onNull(io)
      count(io)
      target.onNull(io)
    }

    def onUndefined(io: IO): IO = {
      logger.onUndefined(io)
      count(io)
      target.onUndefined(io)
    }

    def onBool(io: IO, value: Boolean): IO = {
      logger.onBool(io, value)
      count(io)
      target.onBool(io, value)
    }

    def onInt(io: IO, value: Int): IO = {
      logger.onInt(io, value)
      count(io)
      target.onInt(io, value)
    }

    def onLong(io: IO, value: Long): IO = {
      logger.onLong(io, value)
      count(io)
      target.onLong(io, value)
    }

    def onPosOverLong(io: IO, value: Long): IO = {
      logger.onPosOverLong(io, value)
      count(io)
      target.onPosOverLong(io, value)
    }

    def onNegOverLong(io: IO, value: Long): IO = {
      logger.onNegOverLong(io, value)
      count(io)
      target.onNegOverLong(io, value)
    }

    def onFloat16(io: IO, value: Float): IO = {
      logger.onFloat16(io, value)
      count(io)
      target.onFloat16(io, value)
    }

    def onFloat(io: IO, value: Float): IO = {
      logger.onFloat(io, value)
      count(io)
      target.onFloat(io, value)
    }

    def onDouble(io: IO, value: Double): IO = {
      logger.onDouble(io, value)
      count(io)
      target.onDouble(io, value)
    }

    def onBytes[Bytes: ByteAccess](io: IO, value: Bytes): IO = {
      logger.onBytes(io, value)
      count(io)
      target.onBytes(io, value)
    }

    def onBytesStart(io: IO): IO = {
      logger.onBytesStart(io)
      enterLevel(count = -1, size = 2)
      target.onBytesStart(io)
    }

    def onText[Bytes: ByteAccess](io: IO, value: Bytes): IO = {
      logger.onText(io, value)
      count(io)
      target.onText(io, value)
    }

    def onTextStart(io: IO): IO = {
      logger.onTextStart(io)
      enterLevel(count = -1, size = 3)
      target.onTextStart(io)
    }

    def onArrayHeader(io: IO, length: Long): IO = {
      logger.onArrayHeader(io, length)
      if (length > 0) enterLevel(count = 0, size = length) else count(io)
      target.onArrayHeader(io, length)
    }

    def onArrayStart(io: IO): IO = {
      logger.onArrayStart(io)
      enterLevel(count = -1, size = 0)
      target.onArrayStart(io)
    }

    def onMapHeader(io: IO, length: Long): IO = {
      logger.onMapHeader(io, length)
      if (length > 0) enterLevel(count = 0, size = ~(length << 1)) else count(io)
      target.onMapHeader(io, length)
    }

    def onMapStart(io: IO): IO = {
      logger.onMapStart(io)
      enterLevel(count = -1, size = 1)
      target.onMapStart(io)
    }

    def onBreak(io: IO): IO = {
      val exitedLevelType = levelType
      exitLevel()
      logger.onLevelExited(io, exitedLevelType, break = true)
      count(io) // level-entering items are only counted when the level is exited, not when they are entered
      target.onBreak(io)
    }

    def onTag(io: IO, value: Tag): IO = {
      logger.onTag(io, value)
      target.onTag(io, value)
    }

    def onSimpleValue(io: IO, value: Int): IO = {
      logger.onSimpleValue(io, value)
      count(io)
      target.onSimpleValue(io, value)
    }

    def onEndOfInput(io: IO): IO = {
      logger.onEndOfInput(io)
      target.onEndOfInput(io)
    }

    def copy = {
      val clone = super.clone().asInstanceOf[Receiver[IO]]
      clone._target = _target.copy
      clone._levelCount = _levelCount.clone()
      clone._levelSize = _levelSize.clone()
      clone.logger = createLogger(clone)
      clone
    }

    @tailrec private def count(io: IO): Unit = {
      val cnt = _levelCount(_level)
      if (cnt >= 0) {
        // bounded array or map
        val newCount = cnt + 1
        val rawSize  = _levelSize(_level)
        val size     = if (rawSize >= 0) rawSize else ~rawSize
        if (newCount == size) {
          val exitedLevelType = levelType
          exitLevel()
          logger.onLevelExited(io, exitedLevelType, break = false)
          count(io) // level-entering items are only counted when the level is exited, not when they are entered
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
