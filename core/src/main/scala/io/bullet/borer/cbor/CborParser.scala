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
    * Reads the next data item from the input and sends it to the given [[Receiver]].
    * The given [[Receiver]] receives exactly one call to one of its methods.
    * The returned `Long` is the index of the next byte to consume from the input
    * (and can be used for the subsequent call to this method).
    */
  def pull[Input](input: Input, index: Long, receiver: Receiver)(implicit ia: InputAccess[Input]): Long = {

    def pos(ix: Long) = Position(input, ix)

    @inline def decodePositiveInteger(ix: Long, uLong: Long): Long = {
      if (Util.isUnsignedInt(uLong)) receiver.onInt(uLong.toInt)
      else if (Util.isUnsignedLong(uLong)) receiver.onLong(uLong)
      else receiver.onOverLong(negative = false, uLong)
      ix
    }

    @inline def decodeNegativeInteger(ix: Long, uLong: Long): Long = {
      if (Util.isUnsignedInt(uLong)) receiver.onInt((~uLong).toInt)
      else if (Util.isUnsignedLong(uLong)) receiver.onLong(~uLong)
      else receiver.onOverLong(negative = true, uLong)
      ix
    }

    @inline def decodeByteString(ix: Long, uLong: Long, indefiniteLength: Boolean): Long =
      if (indefiniteLength) {
        receiver.onBytesStart()
        ix
      } else if (Util.isUnsignedLong(uLong)) {
        if (ia.hasByteAtIndex(input, ix + uLong - 1)) {
          receiver.onBytes(ia.bytesAt(input, ix, uLong))(ia.byteAccess)
          ix + uLong
        } else throw Error.InsufficientInput(pos(ix), uLong)
      } else throw Error.Overflow(pos(ix), "This decoder does not support byte strings with size >= 2^63")

    @inline def decodeTextString(ix: Long, uLong: Long, indefiniteLength: Boolean): Long =
      if (indefiniteLength) {
        receiver.onTextStart()
        ix
      } else if (Util.isUnsignedLong(uLong)) {
        if (ia.hasByteAtIndex(input, ix + uLong - 1)) {
          receiver.onText(ia.bytesAt(input, ix, uLong))(ia.byteAccess)
          ix + uLong
        } else throw Error.InsufficientInput(pos(ix), uLong)
      } else throw Error.Overflow(pos(ix), "This decoder does not support text strings with size >= 2^63")

    @inline def decodeArray(ix: Long, uLong: Long, indefiniteLength: Boolean): Long = {
      if (indefiniteLength) receiver.onArrayStart()
      else if (Util.isUnsignedLong(uLong)) receiver.onArrayHeader(uLong)
      else throw Error.Overflow(pos(ix), "This decoder does not support arrays with >= 2^63 elements")
      ix
    }

    @inline def decodeMap(ix: Long, uLong: Long, indefiniteLength: Boolean): Long = {
      if (indefiniteLength) receiver.onMapStart()
      else if (Util.isUnsignedLong(uLong)) receiver.onMapHeader(uLong)
      else throw Error.Overflow(pos(ix), "This decoder does not support maps with >= 2^63 entries")
      ix
    }

    @inline def decodeTag(ix: Long, uLong: Long): Long =
      uLong match {
        case 0 ⇒ receiver.onTag(Tag.DateTimeString); ix
        case 1 ⇒ receiver.onTag(Tag.EpochDateTime); ix
        case 2 ⇒ pull(input, ix, decodeBigInteger(input, ix, negative = false, receiver))
        case 3 ⇒ pull(input, ix, decodeBigInteger(input, ix, negative = true, receiver))
        case 4 ⇒
          val rec = decodeBigDecimal(input, ix, receiver)
          pull(input, pull(input, pull(input, ix, rec), rec), rec)
        case 5     ⇒ receiver.onTag(Tag.BigFloat); ix
        case 21    ⇒ receiver.onTag(Tag.HintBase64url); ix
        case 22    ⇒ receiver.onTag(Tag.HintBase64); ix
        case 23    ⇒ receiver.onTag(Tag.HintBase16); ix
        case 24    ⇒ receiver.onTag(Tag.EmbeddedCBOR); ix
        case 32    ⇒ receiver.onTag(Tag.TextUri); ix
        case 33    ⇒ receiver.onTag(Tag.TextBase64Url); ix
        case 34    ⇒ receiver.onTag(Tag.TextBase64); ix
        case 35    ⇒ receiver.onTag(Tag.TextRegex); ix
        case 36    ⇒ receiver.onTag(Tag.TextMime); ix
        case 55799 ⇒ receiver.onTag(Tag.MagicHeader); ix
        case x     ⇒ receiver.onTag(Tag.Other(x)); ix
      }

    @inline def decodeExtra(ix: Long, info: Int, uLong: Long): Long = {
      info match {
        case 20 ⇒ receiver.onBool(value = false)
        case 21 ⇒ receiver.onBool(value = true)
        case 22 ⇒ receiver.onNull()
        case 23 ⇒ receiver.onUndefined()
        case 24 ⇒
          uLong.toInt match {
            case x if SimpleValue.isLegal(x) ⇒ receiver.onSimpleValue(x)
            case x ⇒
              val msg = s"Simple value must be in the range ${SimpleValue.legalRange}, but was $x"
              throw Error.InvalidCborData(pos(ix), msg)
          }
        case 25          ⇒ receiver.onFloat16(Float16.shortToFloat(uLong.toInt))
        case 26          ⇒ receiver.onFloat(JFloat.intBitsToFloat(uLong.toInt))
        case 27          ⇒ receiver.onDouble(JDouble.longBitsToDouble(uLong))
        case 31          ⇒ receiver.onBreak()
        case x if x < 20 ⇒ receiver.onSimpleValue(x)
        case x           ⇒ throw Error.Unsupported(pos(ix), s"CBOR major type 7 code $x is unsupported by this decoder")
      }
      ix
    }

    if (ia.hasByteAtIndex(input, index)) {
      val byte      = Util.toUnsignedInt(ia.byteAt(input, index))
      var ix        = index + 1
      val majorType = byte >> 5
      val info      = byte & 0x1F
      val uLong = if (info >= 24) {
        if (info <= 27) {
          val byteCount     = 1 << (info - 24)
          val byteCountLong = byteCount.toLong
          ix += byteCountLong
          if (ia.hasByteAtIndex(input, index + byteCountLong)) {
            var x = Util.toUnsignedLong(ia.byteAt(input, index + 1))
            if (byteCount >= 2) {
              x = x << 8 | Util.toUnsignedLong(ia.byteAt(input, index + 2))
              if (byteCount >= 4) {
                x = x << 8 | Util.toUnsignedLong(ia.byteAt(input, index + 3))
                x = x << 8 | Util.toUnsignedLong(ia.byteAt(input, index + 4))
                if (byteCount == 8) {
                  x = x << 8 | Util.toUnsignedLong(ia.byteAt(input, index + 5))
                  x = x << 8 | Util.toUnsignedLong(ia.byteAt(input, index + 6))
                  x = x << 8 | Util.toUnsignedLong(ia.byteAt(input, index + 7))
                  x << 8 | Util.toUnsignedLong(ia.byteAt(input, index + 8))
                } else x
              } else x
            } else x
          } else throw Error.InsufficientInput(pos(index), byteCountLong)
        } else if (info == 31 && 2 <= majorType && majorType <= 7 && majorType != 6) 0L // handled specially
        else throw Error.InvalidCborData(pos(index), s"Additional info [$info] is invalid (major type [$majorType])")
      } else info.toLong // can never be negative
      majorType match {
        case 0 ⇒ decodePositiveInteger(ix, uLong)
        case 1 ⇒ decodeNegativeInteger(ix, uLong)
        case 2 ⇒ decodeByteString(ix, uLong, info == 31)
        case 3 ⇒ decodeTextString(ix, uLong, info == 31)
        case 4 ⇒ decodeArray(ix, uLong, info == 31)
        case 5 ⇒ decodeMap(ix, uLong, info == 31)
        case 6 ⇒ decodeTag(ix, uLong)
        case 7 ⇒ decodeExtra(ix, info, uLong)
      }
    } else {
      receiver.onEndOfInput()
      index
    }
  }

  private def decodeBigInteger[Input](input: Input, ix: Long, negative: Boolean, receiver: Receiver): Receiver =
    new Receiver.Abstract {

      override def onBytes[Bytes](value: Bytes)(implicit ba: ByteAccess[Bytes]): Unit = {
        val bigInteger = new JBigInteger(1, ba.toByteArray(value))
        receiver.onBigInteger(if (negative) bigInteger.not else bigInteger)
      }

      protected def default(dataItem: Int): Unit =
        throw Error.UnexpectedDataItem(
          Position(input, ix),
          "ByteString for decoding a BigInteger",
          DataItem.stringify(dataItem))
    }

  private def decodeBigDecimal[Input: InputAccess](input: Input, ix: Long, receiver: Receiver): Receiver =
    new Receiver.Abstract {

      // 0: expect ArrayHeader(2)
      // 1: expect Int for exponent
      // 2: expect Int, Long, OverLong or BigInteger for mantissa
      private[this] var state         = 0
      private[this] var exponent: Int = _

      override def onArrayHeader(length: Long): Unit =
        state match {
          case 0 if length == 2 ⇒ state = 1
          case _                ⇒ unexpected(actual = s"Array of length $length")
        }

      override def onInt(value: Int): Unit =
        state match {
          case 1 ⇒ exponent = value; state = 2
          case 2 ⇒ result(JBigInteger.valueOf(value.toLong))
          case _ ⇒ super.onInt(value)
        }

      override def onLong(value: Long): Unit =
        state match {
          case 2 ⇒ result(JBigInteger.valueOf(value))
          case _ ⇒ super.onLong(value)
        }

      override def onOverLong(negative: Boolean, value: Long): Unit =
        state match {
          case 2 ⇒
            val bi = new JBigInteger(1, Util.toBigEndianBytes(value))
            result(if (negative) bi.not() else bi)
          case _ ⇒ super.onOverLong(negative, value)
        }

      override def onBigInteger(value: JBigInteger): Unit =
        state match {
          case 2 ⇒ result(value)
          case _ ⇒ super.onBigInteger(value)
        }

      protected def default(dataItem: Int): Unit = unexpected(DataItem.stringify(dataItem))

      private def result(mantissa: JBigInteger): Unit = receiver.onBigDecimal(new JBigDecimal(mantissa, exponent))

      import Error.{UnexpectedDataItem ⇒ UDI}

      private def unexpected(actual: String) = {
        def off = if (exponent <= 23) 2 else if (exponent >> 8 == 0) 3 else if (exponent >> 16 == 0) 4 else 5
        state match {
          case 0 ⇒ throw UDI(Position(input, ix), "Array of length 2 for decoding a BigDecimal", actual)
          case 1 ⇒ throw UDI(Position(input, ix + 1), "BigDecimal exponent as Int", actual)
          case 2 ⇒ throw UDI(Position(input, ix + off), "Integer or BigNum for decoding BigDecimal mantissa", actual)
        }
      }
    }
}
