/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.cbor

import java.lang.{Double ⇒ JDouble, Float ⇒ JFloat}

import io.bullet.borer.{Borer, _}
import io.bullet.borer.internal.Util

import scala.annotation.switch

/**
  * Encapsulates the basic CBOR decoding logic.
  * Stateless.
  */
final private[borer] class CborParser[Input](val input: Input, config: CborParser.Config)(
    implicit ia: InputAccess[Input])
    extends Receiver.Parser[Input] {
  import Borer.Error

  private[this] val inputLen             = ia.length(input)
  private[this] var lastValueStart: Long = _

  def lastValueStartIndex: Long = lastValueStart

  /**
    * Reads the next data item from the input and sends it to the given [[Receiver]].
    * The given [[Receiver]] receives exactly one call to one of its methods.
    * The returned `Long` is the index of the next byte to consume from the input
    * (and can be used for the subsequent call to this method).
    */
  def pull(index: Long, receiver: Receiver): Long = {

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
        receiver.onBytes(ia.bytes(input, ix, uLong))(ia.byteAccess)
        ix + uLong
      } else throw new Error.Overflow(pos(ix), "This decoder does not support byte strings with size >= 2^63")

    @inline def decodeTextString(ix: Long, uLong: Long, indefiniteLength: Boolean): Long =
      if (indefiniteLength) {
        receiver.onTextStart()
        ix
      } else if (Util.isUnsignedLong(uLong)) {
        receiver.onText(ia.bytes(input, ix, uLong))(ia.byteAccess)
        ix + uLong
      } else throw new Error.Overflow(pos(ix), "This decoder does not support text strings with size >= 2^63")

    @inline def decodeArray(ix: Long, uLong: Long, indefiniteLength: Boolean): Long = {
      if (indefiniteLength) receiver.onArrayStart()
      else if (Util.isUnsignedLong(uLong)) receiver.onArrayHeader(uLong)
      else throw new Error.Overflow(pos(ix), "This decoder does not support arrays with >= 2^63 elements")
      ix
    }

    @inline def decodeMap(ix: Long, uLong: Long, indefiniteLength: Boolean): Long = {
      if (indefiniteLength) receiver.onMapStart()
      else if (Util.isUnsignedLong(uLong)) receiver.onMapHeader(uLong)
      else throw new Error.Overflow(pos(ix), "This decoder does not support maps with >= 2^63 entries")
      ix
    }

    def tag(uLong: Long): Tag =
      uLong match {
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

    @inline def decodeExtra(ix: Long, info: Int, uLong: Long): Long = {
      (info: @switch) match {
        case 20 ⇒ receiver.onBool(value = false)
        case 21 ⇒ receiver.onBool(value = true)
        case 22 ⇒ receiver.onNull()
        case 23 ⇒ receiver.onUndefined()
        case 24 ⇒
          uLong.toInt match {
            case x if SimpleValue.isLegal(x) ⇒ receiver.onSimpleValue(x)
            case x ⇒
              val msg = s"Simple value must be in the range ${SimpleValue.legalRange}, but was $x"
              throw new Error.InvalidInputData(pos(ix), msg)
          }
        case 25          ⇒ receiver.onFloat16(Float16.shortToFloat(uLong.toInt))
        case 26          ⇒ receiver.onFloat(JFloat.intBitsToFloat(uLong.toInt))
        case 27          ⇒ receiver.onDouble(JDouble.longBitsToDouble(uLong))
        case 31          ⇒ receiver.onBreak()
        case x if x < 20 ⇒ receiver.onSimpleValue(x)
        case x           ⇒ throw new Error.Unsupported(pos(ix), s"CBOR major type 7 code $x is unsupported by this decoder")
      }
      ix
    }

    lastValueStart = index
    if (index < inputLen) {
      val byte      = ia.unsafeByte(input, index) & 0xFF
      var ix        = index + 1
      val majorType = byte >> 5
      val info      = byte & 0x1F
      val uLong =
        (info: @switch) match {
          case 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 |
              23 ⇒
            info.toLong
          case 24 ⇒
            ix += 1
            if (ix > inputLen) throw new Borer.Error.UnexpectedEndOfInput(pos(index + 1), "8-bit integer")
            ia.unsafeByte(input, index + 1) & 0xffl
          case 25 ⇒
            ix += 2
            if (ix > inputLen) throw new Borer.Error.UnexpectedEndOfInput(pos(index + 1), "16-bit integer")
            ia.doubleByteBigEndian(input, index + 1) & 0xffffl
          case 26 ⇒
            ix += 4
            if (ix > inputLen) throw new Borer.Error.UnexpectedEndOfInput(pos(index + 1), "32-bit integer")
            ia.quadByteBigEndian(input, index + 1) & 0xffffffffl
          case 27 ⇒
            ix += 8
            if (ix > inputLen) throw new Borer.Error.UnexpectedEndOfInput(pos(index + 1), "64-bit integer")
            ia.octaByteBigEndian(input, index + 1)
          case 31 if 2 <= majorType && majorType <= 5 || majorType == 7 ⇒
            0l // handled specially
          case 28 | 29 | 30 ⇒
            throw new Error.InvalidInputData(
              pos(index),
              s"Additional info [$info] is invalid (major type [$majorType])")
        }

      (majorType: @switch) match {
        case 0 ⇒ decodePositiveInteger(ix, uLong)
        case 1 ⇒ decodeNegativeInteger(ix, uLong)
        case 2 ⇒ decodeByteString(ix, uLong, info == 31)
        case 3 ⇒ decodeTextString(ix, uLong, info == 31)
        case 4 ⇒ decodeArray(ix, uLong, info == 31)
        case 5 ⇒ decodeMap(ix, uLong, info == 31)
        case 6 ⇒ receiver.onTag(tag(uLong)); ix
        case 7 ⇒ decodeExtra(ix, info, uLong)
      }
    } else {
      receiver.onEndOfInput()
      index
    }
  }
}

object CborParser {

  trait Config {
    def maxByteStringLength: Int
    def maxTextStringLength: Int
  }

  private[this] val _creator: Receiver.ParserCreator[Any, CborParser.Config] =
    (input, config, inputAccess) ⇒ new CborParser[Any](input, config)(inputAccess)

  def creator[Input, C <: CborParser.Config]: Receiver.ParserCreator[Input, C] =
    _creator.asInstanceOf[Receiver.ParserCreator[Input, C]]
}
