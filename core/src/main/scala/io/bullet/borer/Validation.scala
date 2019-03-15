/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import java.util
import java.math.{BigDecimal ⇒ JBigDecimal, BigInteger ⇒ JBigInteger}
import scala.annotation.tailrec
import io.bullet.borer

object Validation {

  /**
    * Validation config settings
    *
    * @param maxArrayLength the maximum array length to accept
    * @param maxMapLength the maximum map length to accept
    * @param maxNestingLevels the maximum number of nesting levels to accept
    */
  final case class Config(maxArrayLength: Long = Int.MaxValue,
                          maxMapLength: Long = Int.MaxValue,
                          maxNestingLevels: Int = 1000) {

    Util.requireNonNegative(maxArrayLength, "maxArrayLength")
    Util.requireNonNegative(maxMapLength, "maxMapLength")
    Util.requireNonNegative(maxNestingLevels, "maxNestingLevels")

    if (maxMapLength > Long.MaxValue / 2)
      throw new IllegalArgumentException(s"maxMapLength must be <= ${Long.MaxValue / 2}, but was $maxMapLength")
  }

  def creator[IO](target: Borer.Target, config: Option[Config]): Receiver.Creator[IO] =
    config match {
      case Some(x) ⇒ new Validation.Receiver[IO](_, target eq Json, x)
      case None    ⇒ identity
    }

  /**
    * A [[Receiver]] wrapping another [[Receiver]].
    * Performs basic structural checks on the incoming data, e.g. ensures that BREAKs only appear where allowed,
    * the input doesn't break off in the middle of an array or map, etc.
    *
    * Throws [[Borer.Error]] exceptions upon detecting any problem with the input.
    */
  final class Receiver[IO](private var _target: borer.Receiver[IO], isJson: Boolean, config: Config)
      extends borer.Receiver[IO] with java.lang.Cloneable {

    import io.bullet.borer.{DataItem ⇒ DI}

    // compile-time constants
    private final val DEFAULT_MASK = DI.AllButBreak
    private final val MAP          = 1 << 30
    private final val UNBOUNDED    = 1 << 31

    private var levelRemaining = new Array[Long](4)
    private var levelMasks     = new Array[Int](4)
    private var level: Int     = -1
    private var mask: Int      = DEFAULT_MASK

    def target = _target

    def onNull(io: IO): IO = {
      checkAllowed(io, DI.Null)
      count(io)
      _target.onNull(io)
    }

    def onUndefined(io: IO): IO = {
      checkAllowed(io, DI.Undefined)
      count(io)
      _target.onUndefined(io)
    }

    def onBool(io: IO, value: Boolean): IO = {
      checkAllowed(io, DI.Bool)
      count(io)
      _target.onBool(io, value)
    }

    def onInt(io: IO, value: Int): IO = {
      checkAllowed(io, DI.Int)
      count(io)
      _target.onInt(io, value)
    }

    def onLong(io: IO, value: Long): IO = {
      checkAllowed(io, DI.Long)
      count(io)
      _target.onLong(io, value)
    }

    def onOverLong(io: IO, negative: Boolean, value: Long): IO = {
      checkAllowed(io, DI.OverLong)
      count(io)
      _target.onOverLong(io, negative, value)
    }

    def onFloat16(io: IO, value: Float): IO = {
      checkAllowed(io, DI.Float16)
      count(io)
      _target.onFloat16(io, value)
    }

    def onFloat(io: IO, value: Float): IO = {
      checkAllowed(io, DI.Float)
      count(io)
      _target.onFloat(io, value)
    }

    def onDouble(io: IO, value: Double): IO = {
      checkAllowed(io, DI.Double)
      count(io)
      _target.onDouble(io, value)
    }

    def onBigInteger(io: IO, value: JBigInteger): IO = {
      checkAllowed(io, DI.BigInteger)
      count(io)
      _target.onBigInteger(io, value)
    }

    def onBigDecimal(io: IO, value: JBigDecimal): IO = {
      checkAllowed(io, DI.BigDecimal)
      count(io)
      _target.onBigDecimal(io, value)
    }

    def onBytes[Bytes: ByteAccess](io: IO, value: Bytes): IO = {
      checkAllowed(io, DI.Bytes)
      count(io)
      _target.onBytes(io, value)
    }

    def onBytesStart(io: IO): IO = {
      checkAllowed(io, DI.BytesStart)
      enterLevel(io, 0, DI.Bytes | DI.BytesStart | UNBOUNDED)
      _target.onBytesStart(io)
    }

    def onString(io: IO, value: String): IO = {
      checkAllowed(io, DI.String)
      count(io)
      _target.onString(io, value)
    }

    def onText[Bytes: ByteAccess](io: IO, value: Bytes): IO = {
      checkAllowed(io, DI.Text)
      count(io)
      _target.onText(io, value)
    }

    def onTextStart(io: IO): IO = {
      checkAllowed(io, DI.TextStart)
      enterLevel(io, 0, DI.String | DI.Text | DI.TextStart | UNBOUNDED)
      _target.onTextStart(io)
    }

    def onArrayHeader(io: IO, length: Long): IO = {
      checkAllowed(io, DI.ArrayHeader)
      if (length <= config.maxArrayLength) {
        if (length > 0) enterLevel(io, length, DEFAULT_MASK) else count(io)
        _target.onArrayHeader(io, length)
      } else {
        val msg = s"Array length $length is greater than the configured maximum of ${config.maxArrayLength}"
        throw Borer.Error.Unsupported(io, msg)
      }
    }

    def onArrayStart(io: IO): IO = {
      checkAllowed(io, DI.ArrayStart)
      enterLevel(io, 0, DEFAULT_MASK | UNBOUNDED)
      _target.onArrayStart(io)
    }

    def onMapHeader(io: IO, length: Long): IO = {
      checkAllowed(io, DI.MapHeader)
      if (length <= config.maxMapLength) {
        if (length > 0) enterLevel(io, length << 1, DEFAULT_MASK | MAP) else count(io)
        _target.onMapHeader(io, length)
      } else {
        val msg = s"Map length $length is greater than the configured maximum of ${config.maxMapLength}"
        throw Borer.Error.Unsupported(io, msg)
      }
    }

    def onMapStart(io: IO): IO = {
      checkAllowed(io, DI.MapStart)
      enterLevel(io, 0, DEFAULT_MASK | MAP | UNBOUNDED)
      if (isJson) mask = DI.String | MAP | UNBOUNDED
      _target.onMapStart(io)
    }

    def onBreak(io: IO): IO =
      if (level >= 0 && isMasked(UNBOUNDED)) {
        if (!isMasked(MAP) || isEvenNumberedElement) {
          exitLevel()
          count(io) // level-entering items are only counted when the level is exited, not when they are entered
          _target.onBreak(io)
        } else throw Borer.Error.UnexpectedDataItem(io, "map entry value data item", "BREAK")
      } else throw Borer.Error.UnexpectedDataItem(io, "any data item except for BREAK", "BREAK")

    def onTag(io: IO, value: Tag): IO = {
      value match {
        case Tag.EpochDateTime ⇒
          checkAllowed(io, DI.Tag)
          enterLevel(io, 1L, DI.Int | DI.Long | DI.Float16 | DI.Float | DI.Double)

        case Tag.PositiveBigNum | Tag.NegativeBigNum ⇒
          checkAllowed(io, DI.Tag)
          enterLevel(io, 1L, DI.Bytes | DI.BytesStart)

        case Tag.EmbeddedCBOR ⇒
          checkAllowed(io, DI.Tag)
          enterLevel(io, 1L, DI.Bytes | DI.BytesStart)

        case Tag.DateTimeString | Tag.TextUri | Tag.TextBase64Url | Tag.TextBase64 | Tag.TextRegex | Tag.TextMime ⇒
          checkAllowed(io, DI.Tag)
          enterLevel(io, 1L, DI.String | DI.Text)

        case Tag.DecimalFraction | Tag.BigFloat ⇒
          checkAllowed(io, DI.Tag)
          enterLevel(io, 1L, DI.ArrayHeader) // we don't fully verify compliance of the subsequent array content

        case Tag.HintBase64url | Tag.HintBase64 | Tag.HintBase16 | Tag.MagicHeader | Tag.Other(_) ⇒
          checkAllowed(io, DI.Tag)
      }
      _target.onTag(io, value)
    }

    def onSimpleValue(io: IO, value: Int): IO = {
      checkAllowed(io, DI.SimpleValue)
      count(io)
      _target.onSimpleValue(io, value)
    }

    def onEndOfInput(io: IO) =
      if (level >= 0) throw Borer.Error.InsufficientInput(io, 1)
      else _target.onEndOfInput(io)

    def copy = {
      val clone = super.clone().asInstanceOf[Receiver[IO]]
      clone._target = _target.copy
      clone.levelRemaining = levelRemaining.clone()
      clone.levelMasks = levelMasks.clone()
      clone
    }

    private def checkAllowed(io: IO, dataItem: Int): Unit =
      if (!isMasked(dataItem)) {
        throw Borer.Error.UnexpectedDataItem(io, DataItem stringify mask, DataItem stringify dataItem)
      }

    @inline private def isMasked(test: Int): Boolean = (mask & test) != 0

    @inline private def isEvenNumberedElement: Boolean = (levelRemaining(level) & 1) == 0

    @tailrec private def count(io: IO): Unit = {
      val l = level
      if (l >= 0) {
        val remaining  = levelRemaining(l) - 1
        def ok(): Unit = levelRemaining(l) = remaining
        def overflow(tpe: String, max: Long): Nothing = {
          val msg = s"Unbounded $tpe length ${-remaining} is greater than the configured maximum of $max"
          throw Borer.Error.Overflow(io, msg)
        }
        if (isMasked(UNBOUNDED)) {
          if (isMasked(MAP)) {
            if (isJson) {
              mask = if (isEvenNumberedElement) DEFAULT_MASK | MAP | UNBOUNDED else DI.String | MAP | UNBOUNDED
            }
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
      } else throw Borer.Error.Overflow(io, s"Exceeded ${config.maxNestingLevels} maximum array/map nesting levels")
    }

    private def exitLevel(): Unit = {
      val l = level - 1
      level = l
      mask = if (l >= 0) levelMasks(l) else DEFAULT_MASK
    }
  }
}
