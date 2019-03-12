/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.cbor

import java.lang.{Double ⇒ JDouble, Float ⇒ JFloat}
import java.math.{BigDecimal ⇒ JBigDecimal, BigInteger ⇒ JBigInteger}
import io.bullet.borer._

/**
  * Encapsulates the basic CBOR decoding logic.
  * Has no internal state and can therefore be a singleton object.
  */
private[borer] object CborParser extends Receiver.Parser {
  import Borer.Error

  /**
    * Reads the next CBOR data item from the input and sends it to the given [[Receiver]].
    * The given [[Receiver]] receives exactly one call to one of its methods,
    * whose result is also the return value of this call to `pull`.
    */
  def pull(input: Input, receiver: Receiver[Input]): Input = {

    def decodePositiveInteger(in: Input, uLong: Long): Input =
      if (Util.isUnsignedInt(uLong)) receiver.onInt(in, uLong.toInt)
      else if (Util.isUnsignedLong(uLong)) receiver.onLong(in, uLong)
      else receiver.onOverLong(in, negative = false, uLong)

    def decodeNegativeInteger(in: Input, uLong: Long): Input =
      if (Util.isUnsignedInt(uLong)) receiver.onInt(in, (~uLong).toInt)
      else if (Util.isUnsignedLong(uLong)) receiver.onLong(in, ~uLong)
      else receiver.onOverLong(in, negative = true, uLong)

    def decodeByteString(in: Input, uLong: Long, indefiniteLength: Boolean): Input =
      if (indefiniteLength) {
        receiver.onBytesStart(in)
      } else if (Util.isUnsignedLong(uLong)) {
        if (in.hasBytes(uLong)) {
          val in2 = in.readBytes(uLong)
          receiver.onBytes(in2, in2.lastBytes)(in2.byteAccess)
        } else throw Error.InsufficientInput(in, uLong)
      } else throw Error.Overflow(in, "This decoder does not support byte strings with size >= 2^63")

    def decodeTextString(in: Input, uLong: Long, indefiniteLength: Boolean): Input =
      if (indefiniteLength) {
        receiver.onTextStart(in)
      } else if (Util.isUnsignedLong(uLong)) {
        if (in.hasBytes(uLong)) {
          val in2 = in.readBytes(uLong)
          receiver.onText(in2, in2.lastBytes)(in2.byteAccess)
        } else throw Error.InsufficientInput(in, uLong)
      } else throw Error.Overflow(in, "This decoder does not support text strings with size >= 2^63")

    def decodeArray(in: Input, uLong: Long, indefiniteLength: Boolean): Input =
      if (indefiniteLength) {
        receiver.onArrayStart(in)
      } else if (Util.isUnsignedLong(uLong)) {
        receiver.onArrayHeader(in, uLong)
      } else throw Error.Overflow(in, "This decoder does not support arrays with >= 2^63 elements")

    def decodeMap(in: Input, uLong: Long, indefiniteLength: Boolean): Input =
      if (indefiniteLength) {
        receiver.onMapStart(in)
      } else if (Util.isUnsignedLong(uLong)) {
        receiver.onMapHeader(in, uLong)
      } else throw Error.Overflow(in, "This decoder does not support maps with >= 2^63 entries")

    def decodeTag(in: Input, uLong: Long): Input =
      uLong match {
        case 0     ⇒ receiver.onTag(in, Tag.DateTimeString)
        case 1     ⇒ receiver.onTag(in, Tag.EpochDateTime)
        case 2     ⇒ pull(in, decodeBigInteger(negative = false, receiver))
        case 3     ⇒ pull(in, decodeBigInteger(negative = true, receiver))
        case 4     ⇒ pull(in, decodeBigDecimal(receiver))
        case 5     ⇒ receiver.onTag(in, Tag.BigFloat)
        case 21    ⇒ receiver.onTag(in, Tag.HintBase64url)
        case 22    ⇒ receiver.onTag(in, Tag.HintBase64)
        case 23    ⇒ receiver.onTag(in, Tag.HintBase16)
        case 24    ⇒ receiver.onTag(in, Tag.EmbeddedCBOR)
        case 32    ⇒ receiver.onTag(in, Tag.TextUri)
        case 33    ⇒ receiver.onTag(in, Tag.TextBase64Url)
        case 34    ⇒ receiver.onTag(in, Tag.TextBase64)
        case 35    ⇒ receiver.onTag(in, Tag.TextRegex)
        case 36    ⇒ receiver.onTag(in, Tag.TextMime)
        case 55799 ⇒ receiver.onTag(in, Tag.MagicHeader)
        case x     ⇒ receiver.onTag(in, Tag.Other(x))
      }

    def decodeExtra(input: Input, info: Int, uLong: Long): Input =
      info match {
        case 20 ⇒ receiver.onBool(input, value = false)
        case 21 ⇒ receiver.onBool(input, value = true)
        case 22 ⇒ receiver.onNull(input)
        case 23 ⇒ receiver.onUndefined(input)
        case 24 ⇒
          uLong.toInt match {
            case x if SimpleValue.isLegal(x) ⇒ receiver.onSimpleValue(input, x)
            case x ⇒
              val msg = s"Simple value must be in the range ${SimpleValue.legalRange}, but was $x"
              throw Error.InvalidCborData(input, msg)
          }
        case 25          ⇒ receiver.onFloat16(input, Float16.shortToFloat(uLong.toInt))
        case 26          ⇒ receiver.onFloat(input, JFloat.intBitsToFloat(uLong.toInt))
        case 27          ⇒ receiver.onDouble(input, JDouble.longBitsToDouble(uLong))
        case 31          ⇒ receiver.onBreak(input)
        case x if x < 20 ⇒ receiver.onSimpleValue(input, x)
        case x           ⇒ throw Error.Unsupported(input, s"CBOR major type 7 code $x is unsupported by this decoder")
      }

    var in = input
    if (in.hasBytes(1)) {
      in = in.readByte()
      val byte      = Util.toUnsignedInt(in.lastByte)
      val majorType = byte >> 5
      val info      = byte & 0x1F
      val uLong = if (info >= 24) {
        if (info <= 27) {
          val byteCount = 1 << (info - 24)
          if (in.hasBytes(byteCount.toLong)) {
            var x = { in = in.readByte(); Util.toUnsignedLong(in.lastByte) }
            if (byteCount >= 2) {
              x = x << 8 | { in = in.readByte(); Util.toUnsignedLong(in.lastByte) }
              if (byteCount >= 4) {
                x = x << 8 | { in = in.readByte(); Util.toUnsignedLong(in.lastByte) }
                x = x << 8 | { in = in.readByte(); Util.toUnsignedLong(in.lastByte) }
                if (byteCount == 8) {
                  x = x << 8 | { in = in.readByte(); Util.toUnsignedLong(in.lastByte) }
                  x = x << 8 | { in = in.readByte(); Util.toUnsignedLong(in.lastByte) }
                  x = x << 8 | { in = in.readByte(); Util.toUnsignedLong(in.lastByte) }
                  x << 8 | { in = in.readByte(); Util.toUnsignedLong(in.lastByte) }
                } else x
              } else x
            } else x
          } else throw Error.InsufficientInput(in, byteCount.toLong)
        } else if (info == 31 && 2 <= majorType && majorType <= 7 && majorType != 6) 0L // handled specially
        else throw Error.InvalidCborData(in, s"Additional info [$info] is invalid (major type [$majorType])")
      } else info.toLong // can never be negative
      majorType match {
        case 0 ⇒ decodePositiveInteger(in, uLong)
        case 1 ⇒ decodeNegativeInteger(in, uLong)
        case 2 ⇒ decodeByteString(in, uLong, info == 31)
        case 3 ⇒ decodeTextString(in, uLong, info == 31)
        case 4 ⇒ decodeArray(in, uLong, info == 31)
        case 5 ⇒ decodeMap(in, uLong, info == 31)
        case 6 ⇒ decodeTag(in, uLong)
        case 7 ⇒ decodeExtra(in, info, uLong)
      }
    } else receiver.onEndOfInput(in)
  }

  private def decodeBigInteger(negative: Boolean, receiver: Receiver[Input]): Receiver[Input] =
    new Receiver.Abstract[Input] {

      override def onBytes[Bytes](in: Input, value: Bytes)(implicit ba: ByteAccess[Bytes]): Input = {
        val bigInteger = new JBigInteger(1, ba.toByteArray(value))
        receiver.onBigInteger(in, if (negative) bigInteger.not else bigInteger)
      }

      protected def default(in: Input, dataItem: Int): Input =
        throw Error.UnexpectedDataItem(in, "ByteString for decoding a BigInteger", DataItem.stringify(dataItem))
    }

  private def decodeBigDecimal(receiver: Receiver[Input]): Receiver[Input] =
    new Receiver.Abstract[Input] {

      // 0: expect ArrayHeader(2)
      // 1: expect Int for exponent
      // 2: expect Int, Long, OverLong or BigInteger for mantissa
      private[this] var state         = 0
      private[this] var exponent: Int = _

      override def onArrayHeader(in: Input, length: Long): Input =
        state match {
          case 0 if length == 2 ⇒
            state = 1
            pull(in, this)
          case _ ⇒ unexpected(in, actual = s"Array of length $length")
        }

      override def onInt(in: Input, value: Int): Input =
        state match {
          case 1 ⇒
            exponent = value
            state = 2
            pull(in, this)
          case 2 ⇒ result(in, JBigInteger.valueOf(value.toLong))
          case _ ⇒ super.onInt(in, value)
        }

      override def onLong(in: Input, value: Long) =
        state match {
          case 2 ⇒ result(in, JBigInteger.valueOf(value))
          case _ ⇒ super.onLong(in, value)
        }

      override def onOverLong(in: Input, negative: Boolean, value: Long) =
        state match {
          case 2 ⇒
            val bi = new JBigInteger(1, Util.toBigEndianBytes(value))
            result(in, if (negative) bi.not() else bi)
          case _ ⇒ super.onOverLong(in, negative, value)
        }

      override def onBigInteger(in: Input, value: JBigInteger) =
        state match {
          case 2 ⇒ result(in, value)
          case _ ⇒ super.onBigInteger(in, value)
        }

      protected def default(in: Input, dataItem: Int) =
        unexpected(in, DataItem.stringify(dataItem))

      private def result(in: Input, mantissa: JBigInteger): Input =
        receiver.onBigDecimal(in, new JBigDecimal(mantissa, exponent))

      private def unexpected(in: Input, actual: String) = {
        val expected = state match {
          case 0 ⇒ "Array of length 2 for decoding a BigDecimal"
          case 1 ⇒ "BigDecimal exponent as Int"
          case 2 ⇒ "Integer or BigNum for decoding BigDecimal mantissa"
        }
        throw Error.UnexpectedDataItem(in, expected, actual)
      }
    }
}
