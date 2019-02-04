/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.core

import java.util

import scala.annotation.tailrec
import io.bullet.borer.core

object Validation {

  /**
    * Validation config settings
    *
    * @param prohibitUnboundedLengths if true an error will be thrown for all occurences of unbounded
    *                                 byte strings, text strings, arrays and maps
    * @param maxArrayLength the maximum array length to accept
    * @param maxMapLength the maximum map length to accept
    * @param maxNestingLevels the maximum number of nesting levels to accept
    */
  final case class Config(prohibitUnboundedLengths: Boolean = false,
                          maxArrayLength: Long = Int.MaxValue,
                          maxMapLength: Long = Int.MaxValue,
                          maxNestingLevels: Int = 1000) {

    Util.requireNonNegative(maxArrayLength, "maxArrayLength")
    Util.requireNonNegative(maxMapLength, "maxMapLength")
    Util.requireNonNegative(maxNestingLevels, "maxNestingLevels")
  }

  def creator[IO[_], Bytes](config: Option[Config]): Receiver.Creator[IO, Bytes] =
    config match {
      case Some(x) ⇒ new Validation.Receiver[IO[Bytes], Bytes](_, x)
      case None    ⇒ identity
    }

  /**
    * A [[Receiver]] wrapping another [[Receiver]].
    * Performs basic structural checks on the incoming data, e.g. ensures that BREAKs only appear where allowed,
    * the input doesn't break off in the middle of an array or map, etc.
    *
    * Throws [[Cbor.Error]] exceptions upon detecting any problem with the input.
    */
  final class Receiver[IO, Bytes](private var _target: core.Receiver[IO, Bytes], config: Config)
      extends core.Receiver[IO, Bytes] with java.lang.Cloneable {

    // compile-time constants
    private final val DEFAULT_MASK = DataItem.AllButBreak
    private final val MAP          = 1 << 30
    private final val UNBOUNDED    = 1 << 31

    private var levelRemaining = new Array[Long](4)
    private var levelMasks     = new Array[Int](4)
    private var level: Int     = -1
    private var mask: Int      = DEFAULT_MASK

    def target = _target

    def onNull(io: IO): IO = {
      checkAllowed(io, DataItem.Null)
      count(io)
      _target.onNull(io)
    }

    def onUndefined(io: IO): IO = {
      checkAllowed(io, DataItem.Undefined)
      count(io)
      _target.onUndefined(io)
    }

    def onBool(io: IO, value: Boolean): IO = {
      checkAllowed(io, DataItem.Bool)
      count(io)
      _target.onBool(io, value)
    }

    def onInt(io: IO, value: Int): IO = {
      checkAllowed(io, DataItem.Int)
      count(io)
      _target.onInt(io, value)
    }

    def onLong(io: IO, value: Long): IO = {
      checkAllowed(io, DataItem.Long)
      count(io)
      _target.onLong(io, value)
    }

    def onPosOverLong(io: IO, value: Long): IO = {
      if (value >= 0)
        throw new Cbor.Error.ValidationFailure(
          io,
          s"Positive OverLong should have most-significant bit set, but was $value")
      checkAllowed(io, DataItem.PosOverLong)
      count(io)
      _target.onPosOverLong(io, value)
    }

    def onNegOverLong(io: IO, value: Long): IO = {
      if (value >= 0)
        throw new Cbor.Error.ValidationFailure(
          io,
          s"Negative OverLong should have most-significant bit set, but was $value")
      checkAllowed(io, DataItem.NegOverLong)
      count(io)
      _target.onNegOverLong(io, value)
    }

    def onFloat16(io: IO, value: Float): IO = {
      checkAllowed(io, DataItem.Float16)
      count(io)
      _target.onFloat16(io, value)
    }

    def onFloat(io: IO, value: Float): IO = {
      checkAllowed(io, DataItem.Float)
      count(io)
      _target.onFloat(io, value)
    }

    def onDouble(io: IO, value: Double): IO = {
      checkAllowed(io, DataItem.Double)
      count(io)
      _target.onDouble(io, value)
    }

    def onBytes(io: IO, value: Bytes): IO = {
      checkAllowed(io, DataItem.Bytes)
      count(io)
      _target.onBytes(io, value)
    }

    def onByteArray(io: IO, value: Array[Byte]): IO = {
      checkAllowed(io, DataItem.Bytes)
      count(io)
      _target.onByteArray(io, value)
    }

    def onBytesStart(io: IO): IO =
      if (!config.prohibitUnboundedLengths) {
        checkAllowed(io, DataItem.BytesStart)
        enterLevel(io, 0, DataItem.Bytes | DataItem.BytesStart | UNBOUNDED)
        _target.onBytesStart(io)
      } else throw new Cbor.Error.Unsupported(io, "Unbounded byte strings disallowed by configuration")

    def onText(io: IO, value: Bytes): IO = {
      checkAllowed(io, DataItem.Text)
      count(io)
      _target.onText(io, value)
    }

    def onTextByteArray(io: IO, value: Array[Byte]): IO = {
      checkAllowed(io, DataItem.Text)
      count(io)
      _target.onTextByteArray(io, value)
    }

    def onTextStart(io: IO): IO =
      if (!config.prohibitUnboundedLengths) {
        checkAllowed(io, DataItem.TextStart)
        enterLevel(io, 0, DataItem.Text | DataItem.TextStart | UNBOUNDED)
        _target.onTextStart(io)
      } else throw new Cbor.Error.Unsupported(io, "Unbounded text strings disallowed by configuration")

    def onArrayHeader(io: IO, length: Long): IO = {
      checkAllowed(io, DataItem.ArrayHeader)
      if (length <= config.maxArrayLength) {
        if (length > 0) {
          if (mask == DataItem.DecimalFrac) {
            if (length == 2) {
              enterLevel(io, 1L, DataItem.Integer | DataItem.BigNum) // mantissa
              enterLevel(io, 1L, DataItem.Integer)                   // exponent
            } else
              throw new Cbor.Error.UnexpectedDataItem(
                io,
                "Array of length 2 for Decimal Fraction / Big Float",
                "Array of length " + length)
          } else enterLevel(io, length, DEFAULT_MASK)
        } else count(io)
        _target.onArrayHeader(io, length)
      } else {
        val msg = s"Array length $length is greater than the configured maximum of ${config.maxArrayLength}"
        throw new Cbor.Error.Unsupported(io, msg)
      }
    }

    def onArrayStart(io: IO): IO =
      if (!config.prohibitUnboundedLengths) {
        checkAllowed(io, DataItem.ArrayStart)
        enterLevel(io, 0, DEFAULT_MASK | UNBOUNDED)
        _target.onArrayStart(io)
      } else throw new Cbor.Error.Unsupported(io, "Unbounded arrays disallowed by configuration")

    def onMapHeader(io: IO, length: Long): IO = {
      checkAllowed(io, DataItem.MapHeader)
      if (length <= config.maxMapLength) {
        if (length > 0) enterLevel(io, length, DEFAULT_MASK | MAP) else count(io)
        _target.onMapHeader(io, length)
      } else {
        val msg = s"Map length $length is greater than the configured maximum of ${config.maxMapLength}"
        throw new Cbor.Error.Unsupported(io, msg)
      }
    }

    def onMapStart(io: IO): IO =
      if (!config.prohibitUnboundedLengths) {
        checkAllowed(io, DataItem.MapStart)
        enterLevel(io, 0, DEFAULT_MASK | MAP | UNBOUNDED)
        _target.onMapStart(io)
      } else throw new Cbor.Error.Unsupported(io, "Unbounded maps disallowed by configuration")

    def onBreak(io: IO): IO =
      if (level >= 0 && isMasked(UNBOUNDED)) {
        if (!isMasked(MAP) || (levelRemaining(level) & 1) == 0) {
          exitLevel()
          count(io) // level-entering items are only counted when the level is exited, not when they are entered
          _target.onBreak(io)
        } else throw new Cbor.Error.UnexpectedDataItem(io, "map entry value data item", "BREAK")
      } else throw new Cbor.Error.UnexpectedDataItem(io, "any data item except for BREAK", "BREAK")

    def onTag(io: IO, value: Tag): IO = {
      value match {
        case Tag.EpochDateTime ⇒
          checkAllowed(io, DataItem.Tag)
          enterLevel(io, 1L, DataItem.Number)

        case Tag.PositiveBigNum | Tag.NegativeBigNum ⇒
          checkAllowed(io, DataItem.BigNum)
          enterLevel(io, 1L, DataItem.Bytes | DataItem.BytesStart)

        case Tag.EmbeddedCBOR ⇒
          checkAllowed(io, DataItem.Tag)
          enterLevel(io, 1L, DataItem.Bytes | DataItem.BytesStart)

        case Tag.DateTimeString | Tag.TextUri | Tag.TextBase64Url | Tag.TextBase64 | Tag.TextRegex | Tag.TextMime ⇒
          checkAllowed(io, DataItem.Tag)
          enterLevel(io, 1L, DataItem.Text)

        case Tag.DecimalFraction | Tag.BigFloat ⇒
          checkAllowed(io, DataItem.Tag)
          enterLevel(io, 1L, DataItem.DecimalFrac)

        case Tag.HintBase64url | Tag.HintBase64 | Tag.HintBase16 | Tag.MagicHeader | Tag.Other(_) ⇒
          checkAllowed(io, DataItem.Tag)
      }
      _target.onTag(io, value)
    }

    def onSimpleValue(io: IO, value: Int): IO = {
      checkAllowed(io, DataItem.SimpleValue)
      count(io)
      _target.onSimpleValue(io, value)
    }

    def onEndOfInput(io: IO) =
      if (level >= 0) throw new Cbor.Error.InsufficientInput(io, 1)
      else _target.onEndOfInput(io)

    def copy = {
      val clone = super.clone().asInstanceOf[Receiver[IO, Bytes]]
      clone._target = _target.copy
      clone.levelRemaining = util.Arrays.copyOf(levelRemaining, levelRemaining.length)
      clone.levelMasks = util.Arrays.copyOf(levelMasks, levelMasks.length)
      clone
    }

    private def checkAllowed(io: IO, dataItem: Int): Unit =
      if (!isMasked(dataItem)) {
        throw new Cbor.Error.UnexpectedDataItem(io, DataItem stringify mask, DataItem stringify dataItem)
      }

    private def isMasked(test: Int): Boolean = (mask & test) != 0

    @tailrec private def count(io: IO): Unit = {
      val l = level
      if (l >= 0) {
        val remaining  = levelRemaining(l) - 1
        def ok(): Unit = levelRemaining(l) = remaining
        def overflow(tpe: String, max: Long): Nothing = {
          val msg = s"Unbounded $tpe length ${-remaining} is greater than the configured maximum of $max"
          throw new Cbor.Error.Overflow(io, msg)
        }
        if (isMasked(UNBOUNDED)) {
          if (isMasked(MAP)) {
            if (remaining < -config.maxMapLength) overflow("map", config.maxMapLength) else ok()
          } else {
            if (remaining < -config.maxArrayLength) overflow("array", config.maxArrayLength) else ok()
          }
        } else {
          if (remaining == 0) {
            exitLevel()
            count(io) // level-entering items are only counted when the level is exited, not when they are entered
          } else ok()
        }
      }
    }

    private def enterLevel(io: IO, remaining: Long, mask: Int): Unit = {
      val l = level + 1
      if (l <= config.maxNestingLevels) {
        if (l == levelMasks.length) {
          val l2     = l << 1
          val newLen = if (l2 >= 0) l2 else Int.MaxValue // overflow protection
          levelRemaining = util.Arrays.copyOf(levelRemaining, newLen)
          levelMasks = util.Arrays.copyOf(levelMasks, newLen)
        }
        level = l
        levelRemaining(l) = remaining
        levelMasks(l) = mask
        this.mask = mask
      } else throw new Cbor.Error.Overflow(io, s"Exceeded ${config.maxNestingLevels} maximum array/map nesting levels")
    }

    private def exitLevel(): Unit = {
      val l = level - 1
      level = l
      mask = if (l >= 0) levelMasks(l) else DEFAULT_MASK
    }
  }
}
