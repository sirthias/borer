/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import java.util

import io.bullet.borer

import scala.annotation.tailrec

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

  def creator(target: Borer.Target, config: Option[Config]): Receiver.Creator =
    config match {
      case Some(x) ⇒ new Validation.Receiver(_, target eq Json, x)
      case None    ⇒ Util.identityFunc
    }

  /**
    * A [[Receiver]] wrapping another [[Receiver]].
    * Performs basic structural checks on the incoming data, e.g. ensures that BREAKs only appear where allowed,
    * the input doesn't break off in the middle of an array or map, etc.
    *
    * Throws [[Borer.Error]] exceptions upon detecting any problem with the input.
    */
  final class Receiver(private var _target: borer.Receiver, isJson: Boolean, config: Config)
      extends borer.Receiver with java.lang.Cloneable {

    import io.bullet.borer.{DataItem ⇒ DI}

    // compile-time constants
    private final val DEFAULT_MASK = DI.AllButBreak
    private final val MAP          = 1 << 30
    private final val UNBOUNDED    = 1 << 31

    private var levelRemaining = new Array[Long](4)
    private var levelMasks     = new Array[Int](4)
    private var level: Int     = -1
    private var mask: Int      = DEFAULT_MASK

    override def target = _target

    def onNull(): Unit = {
      checkAllowed(DI.Null)
      count()
      _target.onNull()
    }

    def onUndefined(): Unit = {
      checkAllowed(DI.Undefined)
      count()
      _target.onUndefined()
    }

    def onBool(value: Boolean): Unit = {
      checkAllowed(DI.Bool)
      count()
      _target.onBool(value)
    }

    def onInt(value: Int): Unit = {
      checkAllowed(DI.Int)
      count()
      _target.onInt(value)
    }

    def onLong(value: Long): Unit = {
      checkAllowed(DI.Long)
      count()
      _target.onLong(value)
    }

    def onOverLong(negative: Boolean, value: Long): Unit = {
      checkAllowed(DI.OverLong)
      count()
      _target.onOverLong(negative, value)
    }

    def onFloat16(value: Float): Unit = {
      checkAllowed(DI.Float16)
      count()
      _target.onFloat16(value)
    }

    def onFloat(value: Float): Unit = {
      checkAllowed(DI.Float)
      count()
      _target.onFloat(value)
    }

    def onDouble(value: Double): Unit = {
      checkAllowed(DI.Double)
      count()
      _target.onDouble(value)
    }

    def onNumberString(value: String): Unit = {
      checkAllowed(DI.NumberString)
      count()
      _target.onNumberString(value)
    }

    def onBytes[Bytes: ByteAccess](value: Bytes): Unit = {
      checkAllowed(DI.Bytes)
      count()
      _target.onBytes(value)
    }

    def onBytesStart(): Unit = {
      checkAllowed(DI.BytesStart)
      enterLevel(0, DI.Bytes | DI.BytesStart | UNBOUNDED)
      _target.onBytesStart()
    }

    def onString(value: String): Unit = {
      checkAllowed(DI.String)
      count()
      _target.onString(value)
    }

    def onChars(buffer: Array[Char], from: Int, until: Int): Unit = {
      checkAllowed(DI.Chars)
      count()
      _target.onChars(buffer, from, until)
    }

    def onText[Bytes: ByteAccess](value: Bytes): Unit = {
      checkAllowed(DI.Text)
      count()
      _target.onText(value)
    }

    def onTextStart(): Unit = {
      checkAllowed(DI.TextStart)
      enterLevel(0, DI.StringLike | DI.Text | DI.TextStart | UNBOUNDED)
      _target.onTextStart()
    }

    def onArrayHeader(length: Long): Unit = {
      checkAllowed(DI.ArrayHeader)
      if (length <= config.maxArrayLength) {
        if (length > 0) enterLevel(length, DEFAULT_MASK) else count()
        _target.onArrayHeader(length)
      } else {
        val msg = s"Array length $length is greater than the configured maximum of ${config.maxArrayLength}"
        throw Borer.Error.Unsupported(Position.unavailable, msg)
      }
    }

    def onArrayStart(): Unit = {
      checkAllowed(DI.ArrayStart)
      enterLevel(0, DEFAULT_MASK | UNBOUNDED)
      _target.onArrayStart()
    }

    def onMapHeader(length: Long): Unit = {
      checkAllowed(DI.MapHeader)
      if (length <= config.maxMapLength) {
        if (length > 0) enterLevel(length << 1, DEFAULT_MASK | MAP) else count()
        _target.onMapHeader(length)
      } else {
        val msg = s"Map length $length is greater than the configured maximum of ${config.maxMapLength}"
        throw Borer.Error.Unsupported(Position.unavailable, msg)
      }
    }

    def onMapStart(): Unit = {
      checkAllowed(DI.MapStart)
      enterLevel(0, DEFAULT_MASK | MAP | UNBOUNDED)
      if (isJson) mask = DI.StringLike | MAP | UNBOUNDED
      _target.onMapStart()
    }

    def onBreak(): Unit =
      if (level >= 0 && isMasked(UNBOUNDED)) {
        if (!isMasked(MAP) || isEvenNumberedElement) {
          exitLevel()
          count() // level-entering items are only counted when the level is exited, not when they are entered
          _target.onBreak()
        } else throw Borer.Error.UnexpectedDataItem(Position.unavailable, "map entry value data item", "BREAK")
      } else throw Borer.Error.UnexpectedDataItem(Position.unavailable, "any data item except for BREAK", "BREAK")

    def onTag(value: Tag): Unit = {
      value match {
        case Tag.EpochDateTime ⇒
          checkAllowed(DI.Tag)
          enterLevel(1L, DI.Int | DI.Long | DI.Float16 | DI.Float | DI.Double)

        case Tag.PositiveBigNum | Tag.NegativeBigNum ⇒
          checkAllowed(DI.Tag)
          enterLevel(1L, DI.Bytes | DI.BytesStart)

        case Tag.EmbeddedCBOR ⇒
          checkAllowed(DI.Tag)
          enterLevel(1L, DI.Bytes | DI.BytesStart)

        case Tag.DateTimeString | Tag.TextUri | Tag.TextBase64Url | Tag.TextBase64 | Tag.TextRegex | Tag.TextMime ⇒
          checkAllowed(DI.Tag)
          enterLevel(1L, DI.StringLike | DI.Text)

        case Tag.DecimalFraction | Tag.BigFloat ⇒
          checkAllowed(DI.Tag)
          enterLevel(1L, DI.ArrayHeader) // we don't fully verify compliance of the subsequent array content

        case Tag.HintBase64url | Tag.HintBase64 | Tag.HintBase16 | Tag.MagicHeader | Tag.Other(_) ⇒
          checkAllowed(DI.Tag)
      }
      _target.onTag(value)
    }

    def onSimpleValue(value: Int): Unit = {
      checkAllowed(DI.SimpleValue)
      count()
      _target.onSimpleValue(value)
    }

    def onEndOfInput(): Unit =
      if (level >= 0) throw Borer.Error.InsufficientInput(Position.unavailable, 1)
      else _target.onEndOfInput()

    override def copy = {
      val clone = super.clone().asInstanceOf[Receiver]
      clone._target = _target.copy
      clone.levelRemaining = levelRemaining.clone()
      clone.levelMasks = levelMasks.clone()
      clone
    }

    private def checkAllowed(dataItem: Int): Unit =
      if (!isMasked(dataItem)) {
        throw Borer.Error.UnexpectedDataItem(Position.unavailable, DataItem stringify mask, DataItem stringify dataItem)
      }

    @inline private def isMasked(test: Int): Boolean = (mask & test) != 0

    @inline private def isEvenNumberedElement: Boolean = (levelRemaining(level) & 1) == 0

    @tailrec private def count(): Unit = {
      val l = level
      if (l >= 0) {
        val remaining  = levelRemaining(l) - 1
        def ok(): Unit = levelRemaining(l) = remaining
        def overflow(tpe: String, max: Long): Nothing = {
          val msg = s"Unbounded $tpe length ${-remaining} is greater than the configured maximum of $max"
          throw Borer.Error.Overflow(Position.unavailable, msg)
        }
        if (isMasked(UNBOUNDED)) {
          if (isMasked(MAP)) {
            if (isJson) {
              mask = if (isEvenNumberedElement) DEFAULT_MASK | MAP | UNBOUNDED else DI.StringLike | MAP | UNBOUNDED
            }
            if (remaining < -config.maxMapLength) overflow("map", config.maxMapLength) else ok()
          } else {
            if (remaining < -config.maxArrayLength) overflow("array", config.maxArrayLength) else ok()
          }
        } else {
          if (remaining == 0) {
            exitLevel()
            count() // level-entering items are only counted when the level is exited, not when they are entered
          } else ok()
        }
      }
    }

    private def enterLevel(remaining: Long, mask: Int): Unit = {
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
      } else
        throw Borer.Error
          .Overflow(Position.unavailable, s"Exceeded ${config.maxNestingLevels} maximum array/map nesting levels")
    }

    private def exitLevel(): Unit = {
      val l = level - 1
      level = l
      mask = if (l >= 0) levelMasks(l) else DEFAULT_MASK
    }
  }
}
