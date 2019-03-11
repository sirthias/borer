/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.cbor

import java.lang.{Double ⇒ JDouble, Float ⇒ JFloat}
import io.bullet.borer._

/**
  * Encapsulates then basic CBOR decoding logic.
  * Has no internal state and can therefore be a singleton object.
  */
object ByteReader {
  import Cbor.Error

  /**
    * Reads the next CBOR data item from the input and sends it to the given [[Receiver]].
    * The given [[Receiver]] receives exactly one call to one of its methods,
    * whose result is also the return value of this call to `pull`.
    */
  def pull(input: Input, receiver: Receiver[Input]): Input = {

    def decodePositiveInteger(in: Input, uLong: Long): Input =
      if (Util.isUnsignedInt(uLong)) receiver.onInt(in, uLong.toInt)
      else if (Util.isUnsignedLong(uLong)) receiver.onLong(in, uLong)
      else receiver.onPosOverLong(in, uLong)

    def decodeNegativeInteger(in: Input, uLong: Long): Input =
      if (Util.isUnsignedInt(uLong)) receiver.onInt(in, (~uLong).toInt)
      else if (Util.isUnsignedLong(uLong)) receiver.onLong(in, ~uLong)
      else receiver.onNegOverLong(in, uLong)

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

    def decodeTag(in: Input, uLong: Long): Input = {
      val tag = uLong match {
        case 0     ⇒ Tag.DateTimeString
        case 1     ⇒ Tag.EpochDateTime
        case 2     ⇒ Tag.PositiveBigNum
        case 3     ⇒ Tag.NegativeBigNum
        case 4     ⇒ Tag.DecimalFraction
        case 5     ⇒ Tag.BigFloat
        case 21    ⇒ Tag.HintBase64url
        case 22    ⇒ Tag.HintBase64
        case 23    ⇒ Tag.HintBase16
        case 24    ⇒ Tag.EmbeddedCBOR
        case 32    ⇒ Tag.TextUri
        case 33    ⇒ Tag.TextBase64Url
        case 34    ⇒ Tag.TextBase64
        case 35    ⇒ Tag.TextRegex
        case 36    ⇒ Tag.TextMime
        case 55799 ⇒ Tag.MagicHeader
        case x     ⇒ Tag.Other(x)
      }
      receiver.onTag(in, tag)
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
}
