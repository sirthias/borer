/*
 * Copyright (c) 2019-2023 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.cbor

import java.lang.{Double => JDouble, Float => JFloat}

import io.bullet.borer.{Borer, _}
import io.bullet.borer.internal.{Parser, Util}

import scala.annotation.switch

/**
 * Encapsulates the basic CBOR decoding logic.
 */
final private[borer] class CborParser[Bytes: ByteAccess](val input: Input[Bytes])(using CborParser.Config)
    extends Parser[Bytes]:

  private[this] var _valueIndex: Long = _

  def valueIndex: Long = _valueIndex

  /**
   * Reads the next data item from the input and sends it to the given [[Receiver]].
   * The given [[Receiver]] receives exactly one call to one of its methods.
   * The returned `Int` is the [[DataItem]] code for the value the [[Receiver]] received.
   */
  def pull(receiver: Receiver): Int =

    def decodePositiveInteger(uLong: Long): Int =
      if (Util.isUnsignedInt(uLong))
        receiver.onInt(uLong.toInt)
        DataItem.Int
      else if (Util.isUnsignedLong(uLong))
        receiver.onLong(uLong)
        DataItem.Long
      else
        receiver.onOverLong(negative = false, uLong)
        DataItem.OverLong

    def decodeNegativeInteger(uLong: Long): Int =
      if (Util.isUnsignedInt(uLong))
        receiver.onInt((~uLong).toInt)
        DataItem.Int
      else if (Util.isUnsignedLong(uLong))
        receiver.onLong(~uLong)
        DataItem.Long
      else
        receiver.onOverLong(negative = true, uLong)
        DataItem.OverLong

    def decodeByteString(uLong: Long, indefiniteLength: Boolean): Int =
      if (indefiniteLength)
        receiver.onBytesStart()
        DataItem.BytesStart
      else if (Util.isUnsignedLong(uLong))
        receiver.onBytes(input.readBytes(uLong, this))
        DataItem.Bytes
      else failOverflow("This decoder does not support byte strings with size >= 2^63")

    def decodeTextString(uLong: Long, indefiniteLength: Boolean): Int =
      if (indefiniteLength)
        receiver.onTextStart()
        DataItem.TextStart
      else if (Util.isUnsignedLong(uLong))
        receiver.onText(input.readBytes(uLong, this))
        DataItem.Text
      else failOverflow("This decoder does not support text strings with size >= 2^63")

    def decodeArray(uLong: Long, indefiniteLength: Boolean): Int =
      if (indefiniteLength)
        receiver.onArrayStart()
        DataItem.ArrayStart
      else if (Util.isUnsignedLong(uLong))
        receiver.onArrayHeader(uLong)
        DataItem.ArrayHeader
      else failOverflow("This decoder does not support arrays with size >= 2^63")

    def decodeMap(uLong: Long, indefiniteLength: Boolean): Int =
      if (indefiniteLength)
        receiver.onMapStart()
        DataItem.MapStart
      else if (Util.isUnsignedLong(uLong))
        receiver.onMapHeader(uLong)
        DataItem.MapHeader
      else failOverflow("This decoder does not support maps with size >= 2^63")

    def decodeTag(uLong: Long): Int =
      val tag =
        uLong match
          case 0     => Tag.DateTimeString
          case 1     => Tag.EpochDateTime
          case 2     => Tag.PositiveBigNum
          case 3     => Tag.NegativeBigNum
          case 4     => Tag.DecimalFraction
          case 5     => Tag.BigFloat
          case 21    => Tag.HintBase64url
          case 22    => Tag.HintBase64
          case 23    => Tag.HintBase16
          case 24    => Tag.EmbeddedCBOR
          case 32    => Tag.TextUri
          case 33    => Tag.TextBase64Url
          case 34    => Tag.TextBase64
          case 35    => Tag.TextRegex
          case 36    => Tag.TextMime
          case 55799 => Tag.MagicHeader
          case x     => Tag.Other(x)
      receiver.onTag(tag)
      DataItem.Tag

    def decodeExtra(info: Int, uLong: Long): Int =
      (info: @switch) match
        case 20 | 21 =>
          receiver.onBoolean(info == 21)
          DataItem.Boolean
        case 22 =>
          receiver.onNull()
          DataItem.Null
        case 23 =>
          receiver.onUndefined()
          DataItem.Undefined
        case 24 =>
          uLong.toInt match
            case x if SimpleValue.isLegal(x) => receiver.onSimpleValue(x)
            case x => failInvalidInput(s"Simple value must be in the range ${SimpleValue.legalRange}, but was $x")
          DataItem.SimpleValue
        case 25 =>
          receiver.onFloat16(Float16.shortToFloat(uLong.toInt))
          DataItem.Float16
        case 26 =>
          receiver.onFloat(JFloat.intBitsToFloat(uLong.toInt))
          DataItem.Float
        case 27 =>
          receiver.onDouble(JDouble.longBitsToDouble(uLong))
          DataItem.Double
        case 31 =>
          receiver.onBreak()
          DataItem.Break
        case x =>
          if (SimpleValue.isLegal(x))
            receiver.onSimpleValue(x)
            DataItem.SimpleValue
          else failUnsupported(s"CBOR major type 7 code $x is unsupported by this decoder")

    _valueIndex = -1
    val byte = input.readBytePadded(this)
    if (_valueIndex < 0)
      _valueIndex = input.cursor - 1
      val majorType = byte << 24 >>> 29
      val info      = byte & 0x1F
      val uLong =
        (info: @switch) match
          case 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 |
              23 =>
            info.toLong
          case 24 => input.readBytePadded(this) & 0xFFL
          case 25 => input.readDoubleByteBigEndianPadded(this) & 0xFFFFL
          case 26 => input.readQuadByteBigEndianPadded(this) & 0xFFFFFFFFL
          case 27 => input.readOctaByteBigEndianPadded(this)
          case _ =>
            if (info == 31 && 2 <= majorType && majorType <= 5 || majorType == 7) 0L // handled specially
            else failInvalidInput(s"Additional info `$info` is invalid (major type `$majorType`)")

      (majorType: @switch) match
        case 0 => decodePositiveInteger(uLong)
        case 1 => decodeNegativeInteger(uLong)
        case 2 => decodeByteString(uLong, info == 31)
        case 3 => decodeTextString(uLong, info == 31)
        case 4 => decodeArray(uLong, info == 31)
        case 5 => decodeMap(uLong, info == 31)
        case 6 => decodeTag(uLong)
        case 7 => decodeExtra(info, uLong)
    else
      receiver.onEndOfInput()
      DataItem.EndOfInput

  def padByte(): Byte =
    if (_valueIndex < 0)
      _valueIndex = 0
      0
    else failUnexpectedEOI("8-bit integer")

  def padDoubleByte(remaining: Int): Nothing        = failUnexpectedEOI("16-bit integer")
  def padQuadByte(remaining: Int): Nothing          = failUnexpectedEOI("32-bit integer")
  def padOctaByte(remaining: Int): Nothing          = failUnexpectedEOI("64-bit integer")
  def padBytes(rest: Bytes, missing: Long): Nothing = failUnexpectedEOI(s"at least $missing more bytes")

  private def failUnexpectedEOI(expected: String) = throw new Borer.Error.UnexpectedEndOfInput(lastPos, expected)
  private def failInvalidInput(msg: String)       = throw new Borer.Error.InvalidInputData(lastPos, msg)
  private def failOverflow(msg: String)           = throw new Borer.Error.Overflow(lastPos, msg)
  private def failUnsupported(msg: String)        = throw new Borer.Error.Unsupported(lastPos, msg)

  private def lastPos = input.position(_valueIndex)

object CborParser:

  trait Config:
    def maxByteStringLength: Int
    def maxTextStringLength: Int

  private[this] val _creator: Parser.Creator[Any, CborParser.Config] =
    (input, byteAccess, config) => new CborParser(input)(using byteAccess, config)

  def creator[Bytes, Conf <: CborParser.Config]: Parser.Creator[Bytes, Conf] =
    _creator.asInstanceOf[Parser.Creator[Bytes, Conf]]
