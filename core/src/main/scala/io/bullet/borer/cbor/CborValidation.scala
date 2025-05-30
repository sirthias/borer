/*
 * Copyright (c) 2019-2024 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.cbor

import java.util

import io.bullet.borer
import io.bullet.borer._
import io.bullet.borer.Borer.Error

import scala.annotation.tailrec

private[borer] object CborValidation:

  trait Config:

    /**
     * @return the maximum array length to accept
     */
    def maxArrayLength: Long

    /**
     * @return the maximum map length to accept
     */
    def maxMapLength: Long

    /**
     * @return the maximum number of nesting levels to accept
     */
    def maxNestingLevels: Int

  private[this] val _wrapper: borer.Receiver.Transformer[Config] = new Receiver(_, _)
  def wrapper[C <: Config]: borer.Receiver.Transformer[C]        = _wrapper.asInstanceOf[borer.Receiver.Transformer[C]]

  /**
   * A [[Receiver]] wrapping another [[Receiver]].
   * Performs basic structural checks on the incoming data, e.g. ensures that BREAKs only appear where allowed,
   * the input doesn't break off in the middle of an array or map, etc.
   *
   * Throws [[Borer.Error]] exceptions upon detecting any problem with the input.
   */
  final class Receiver(target: borer.Receiver, config: Config) extends borer.Receiver:

    import io.bullet.borer.{DataItem => DI}

    // compile-time constants
    final private val DEFAULT_MASK = DI.AllButBreak
    final private val MAP          = 1 << 30
    final private val UNBOUNDED    = 1 << 31

    private var levelRemaining = new Array[Long](4)
    private var levelMasks     = new Array[Int](4)
    private var level: Int     = -1
    private var mask: Int      = DEFAULT_MASK

    def onNull(): Unit =
      checkAllowed(DI.Null)
      count()
      target.onNull()

    def onUndefined(): Unit =
      checkAllowed(DI.Undefined)
      count()
      target.onUndefined()

    def onBoolean(value: Boolean): Unit =
      checkAllowed(DI.Boolean)
      count()
      target.onBoolean(value)

    def onInt(value: Int): Unit =
      checkAllowed(DI.Int)
      count()
      target.onInt(value)

    def onLong(value: Long): Unit =
      checkAllowed(DI.Long)
      count()
      target.onLong(value)

    def onOverLong(negative: Boolean, value: Long): Unit =
      checkAllowed(DI.OverLong)
      count()
      target.onOverLong(negative, value)

    def onFloat16(value: Float): Unit =
      checkAllowed(DI.Float16)
      count()
      target.onFloat16(value)

    def onFloat(value: Float): Unit =
      checkAllowed(DI.Float)
      count()
      target.onFloat(value)

    def onDouble(value: Double): Unit =
      checkAllowed(DI.Double)
      count()
      target.onDouble(value)

    def onNumberString(value: String): Unit =
      checkAllowed(DI.NumberString)
      count()
      target.onNumberString(value)

    def onBytes[Bytes: ByteAccess](value: Bytes): Unit =
      checkAllowed(DI.Bytes)
      count()
      target.onBytes(value)

    def onBytesStart(): Unit =
      checkAllowed(DI.BytesStart)
      enterLevel(0, DI.Bytes | DI.BytesStart | UNBOUNDED)
      target.onBytesStart()

    def onString(value: String): Unit =
      checkAllowed(DI.String)
      count()
      target.onString(value)

    def onChars(buffer: Array[Char], length: Int): Unit =
      checkAllowed(DI.Chars)
      count()
      target.onChars(buffer, length)

    def onText[Bytes: ByteAccess](value: Bytes): Unit =
      checkAllowed(DI.Text)
      count()
      target.onText(value)

    def onTextStart(): Unit =
      checkAllowed(DI.TextStart)
      enterLevel(0, DI.String | DI.Chars | DI.Text | DI.TextStart | UNBOUNDED)
      target.onTextStart()

    def onArrayHeader(length: Long): Unit =
      checkAllowed(DI.ArrayHeader)
      if (length <= config.maxArrayLength)
        if (length > 0) enterLevel(length, DEFAULT_MASK) else count()
        target.onArrayHeader(length)
      else
        val msg = s"Array length $length is greater than the configured maximum of ${config.maxArrayLength}"
        throw new Borer.Error.Unsupported(null, msg)

    def onArrayStart(): Unit =
      checkAllowed(DI.ArrayStart)
      enterLevel(0, DEFAULT_MASK | UNBOUNDED)
      target.onArrayStart()

    def onMapHeader(length: Long): Unit =
      checkAllowed(DI.MapHeader)
      if (length <= config.maxMapLength)
        if (length > 0) enterLevel(length << 1, DEFAULT_MASK | MAP) else count()
        target.onMapHeader(length)
      else
        val msg = s"Map length $length is greater than the configured maximum of ${config.maxMapLength}"
        throw new Borer.Error.Unsupported(null, msg)

    def onMapStart(): Unit =
      checkAllowed(DI.MapStart)
      enterLevel(0, DEFAULT_MASK | MAP | UNBOUNDED)
      target.onMapStart()

    def onBreak(): Unit =
      def failBreak() =
        if (level == 0)
          if (isMasked(UNBOUNDED)) failInvalid("map entry value data item", "BREAK")
          else
            val tpe = if (isMasked(MAP)) "map" else "array"
            failInvalid(s"${levelRemaining(level)} more data items of definite-length $tpe", "BREAK")
        else failInvalid("next data item on outermost CBOR structure level", "BREAK")

      if (level >= 0 && isMasked(UNBOUNDED) && (!isMasked(MAP) || isEvenNumberedElement))
        exitLevel()
        count() // level-entering items are only counted when the level is exited, not when they are entered
        target.onBreak()
      else failBreak()

    def onTag(value: Tag): Unit =
      value match
        case Tag.EpochDateTime =>
          checkAllowed(DI.Tag)
          enterLevel(1L, DI.Int | DI.Long | DI.Float16 | DI.Float | DI.Double | DI.NumberString)

        case Tag.PositiveBigNum | Tag.NegativeBigNum =>
          checkAllowed(DI.Tag)
          enterLevel(1L, DI.Bytes | DI.BytesStart)

        case Tag.EmbeddedCBOR =>
          checkAllowed(DI.Tag)
          enterLevel(1L, DI.Bytes | DI.BytesStart)

        case Tag.DateTimeString | Tag.TextUri | Tag.TextBase64Url | Tag.TextBase64 | Tag.TextRegex | Tag.TextMime =>
          checkAllowed(DI.Tag)
          enterLevel(1L, DI.String | DI.Chars | DI.Text)

        case Tag.DecimalFraction | Tag.BigFloat =>
          checkAllowed(DI.Tag)
          enterLevel(1L, DI.ArrayHeader) // we don't fully verify compliance of the subsequent array content

        case Tag.HintBase64url | Tag.HintBase64 | Tag.HintBase16 | Tag.MagicHeader | Tag.Other(_) =>
          checkAllowed(DI.Tag)
      target.onTag(value)

    def onSimpleValue(value: Int): Unit =
      checkAllowed(DI.SimpleValue)
      count()
      target.onSimpleValue(value)

    def onEndOfInput(): Unit =
      if (level >= 0)
        def remaining = levelRemaining(level)
        val msg       =
          (isMasked(MAP), isMasked(UNBOUNDED), isEvenNumberedElement) match
            case (false, false, _)    => s"$remaining more data items of definite-length array"
            case (false, true, _)     => "next array data item or BREAK"
            case (true, false, false) => s"next map data value and ${remaining / 2} more entries of definite-length map"
            case (true, false, true)  => s"$remaining more data items of definite-length map"
            case (true, true, false)  => "next map data value"
            case (true, true, true)   => "next map entry or BREAK"
        throw new Borer.Error.UnexpectedEndOfInput(null, msg)
      else target.onEndOfInput()

    private def checkAllowed(dataItem: Int): Unit =
      if (!isMasked(dataItem))
        throw new Error.InvalidInputData(null, DataItem stringify mask, DataItem stringify dataItem)

    private inline def isMasked(test: Int): Boolean = (mask & test) != 0

    private inline def isEvenNumberedElement: Boolean = (levelRemaining(level) & 1) == 0

    @tailrec private def count(): Unit =
      val l = level
      if (l >= 0)
        val remaining                                 = levelRemaining(l) - 1
        def ok(): Unit                                = levelRemaining(l) = remaining
        def overflow(tpe: String, max: Long): Nothing =
          val msg = s"Unbounded $tpe length ${-remaining} is greater than the configured maximum of $max"
          throw new Borer.Error.Overflow(null, msg)
        if (isMasked(UNBOUNDED))
          if (isMasked(MAP))
            if (remaining < -config.maxMapLength) overflow("map", config.maxMapLength) else ok()
          else if (remaining < -config.maxArrayLength) overflow("array", config.maxArrayLength)
          else ok()
        else if (remaining == 0)
          exitLevel()
          count() // level-entering items are only counted when the level is exited, not when they are entered
        else ok()

    private def enterLevel(remaining: Long, mask: Int): Unit =
      val l = level + 1
      if (l <= config.maxNestingLevels)
        if (l == levelMasks.length)
          val l2     = l << 1
          val newLen = if (l2 >= 0) l2 else Int.MaxValue // overflow protection
          levelRemaining = util.Arrays.copyOf(levelRemaining, newLen)
          levelMasks = util.Arrays.copyOf(levelMasks, newLen)
        level = l
        levelRemaining(l) = remaining
        levelMasks(l) = mask
        this.mask = mask
      else throw new Borer.Error.Overflow(null, s"Exceeded ${config.maxNestingLevels} maximum array/map nesting levels")

    private def exitLevel(): Unit =
      val l = level - 1
      level = l
      mask = if (l >= 0) levelMasks(l) else DEFAULT_MASK

    private def failInvalid(expected: String, actual: String) =
      throw new Borer.Error.InvalidInputData(null, expected, actual)
