/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.json

import java.util

import io.bullet.borer.{Borer, _}
import io.bullet.borer.internal.Util

import scala.annotation.{switch, tailrec}

/**
  * Encapsulates the basic JSON parsing logic.
  * Also performs inline UTF-8 decoding from raw bytes.
  *
  * This [[Receiver.Parser]] only produces data items that can be directly represented in JSON, specifically
  * - null
  * - Boolean
  * - Int
  * - Long
  * - Float (if a decimal number can be (easily) represented as a float)
  * - Double (if a decimal number can be (easily) represented as a double)
  * - NumberString (if a decimal number cannot easily be represented as a float or double)
  * - String
  * - Indefinite-Length Array
  * - Indefinite-Length Map
  *
  * These data items are never produced:
  * - undefined
  * - Overlong
  * - Float16
  * - Byte String
  * - Byte String Stream
  * - Text Byte String
  * - Text Byte String Stream
  * - Definite-Length Array
  * - Definite-Length Map
  * - Tag
  * - Simple Value
  *
  * @see https://tools.ietf.org/html/rfc8259
  */
private[borer] final class JsonParser[Input](val input: Input, val config: JsonParser.Config)(
    implicit ia: InputAccess[Input])
    extends Receiver.Parser[Input] {
  import JsonParser._

  private[this] val inputLen           = ia.length(input)
  private[this] val inputLenMinus8     = inputLen - 8
  private[this] val maxStringLength    = config.maxStringLength
  private[this] var chars: Array[Char] = new Array[Char](32)
  private[this] var state: Int         = EXPECT_VALUE
  private[this] var auxInt: Int        = _
  private[this] var auxLong: Long      = _

  private[this] var level: Int = _ // valid range: 0..64

  // keeps the type of each level as a bit map: 0 -> Array, 1 -> Map
  // the current level is always the LSB (bit 0)
  private[this] var levelType: Long = _

  /**
    * Reads the next data item from the input and sends it to the given [[Receiver]].
    * The [[Receiver]] receives exactly one call to one of its methods.
    * The returned `Long` is the index of the next byte to consume from the input
    * (and can be used for the subsequent call to this method).
    */
  def pull(index: Long, receiver: Receiver): Long = {

    def appendChar(charCursor: Int, c: Char): Int = {
      val newCursor = charCursor + 1
      if (newCursor <= maxStringLength) {
        ensureCharsLen(newCursor)
        chars(charCursor) = c
        newCursor
      } else failStringTooLong(skipWhiteSpace(index))
    }

    def appendAll(start: Long, end: Long, charCursor: Int): Int = {
      val len = charCursor + (end - start).toInt
      if (len <= maxStringLength) {
        ensureCharsLen(len)
        @tailrec def slowRec(ix: Long, cc: Int): Int =
          if (cc < len) {
            chars(cc) = getInputByteUnsafe(ix).toChar
            slowRec(ix + 1, cc + 1)
          } else cc
        val len4 = len - 4
        @tailrec def fastRec(ix: Long, cc: Int): Int =
          if (cc < len4) {
            val quad = ia.quadByteBigEndian(input, ix)
            chars(cc) = (quad >>> 24).toChar
            chars(cc + 1) = ((quad << 8) >>> 24).toChar
            chars(cc + 2) = ((quad << 16) >>> 24).toChar
            chars(cc + 3) = (quad & 0xFF).toChar
            fastRec(ix + 4, cc + 4)
          } else slowRec(ix, cc)
        fastRec(start, charCursor)
      } else failStringTooLong(skipWhiteSpace(index))
    }

    @inline def parseNull(ix: Long): Long =
      if (ix < inputLen - 3 && ia.quadByteBigEndian(input, ix) == 0x6e756c6c) { // "null"
        receiver.onNull()
        ix + 4
      } else failSyntaxError(ix, "`null`")

    @inline def parseFalse(ix: Long): Long =
      if (ix < inputLen - 3 && ia.quadByteBigEndian(input, ix) == 0x616c7365) { // "alse"
        receiver.onBool(value = false)
        ix + 4
      } else failSyntaxError(ix - 1, "`false`")

    @inline def parseTrue(ix: Long): Long =
      if (ix < inputLen - 3 && ia.quadByteBigEndian(input, ix) == 0x74727565) { // "true"
        receiver.onBool(value = true)
        ix + 4
      } else failSyntaxError(ix, "`true`")

    def parseNumberStringExponentPart(idx: Long, charCursor: Int): Long = {
      var ix = idx
      var c  = getInputByteOrEOI(ix) & 0xFF
      if (c == '+' || c == '-') {
        ix += 1
        c = getInputByteOrEOI(ix) & 0xFF
      }
      val ix0 = ix
      ix = parseDigits(ix0, ('0' - c).toLong)
      if (ix > ix0) {
        val exp = -auxLong.toInt
        if (0 <= exp && exp <= config.maxNumberAbsExponent) {
          receiver.onNumberString(getString(appendAll(idx, ix, charCursor)))
          ix
        } else failNumberExponentTooLarge(ix0)
      } else failSyntaxError(ix0, "DIGIT", c.toLong)
    }

    @tailrec def parseNumberStringFractionPart(ix: Long, maxIx: Long, charCursor: Int): Long =
      if (ix < maxIx) {
        val c = getInputByteOrEOI(ix) & 0xFF
        if ((c ^ 0x30) > 9) {
          if ((c | 0x20) != 'e') {
            receiver.onNumberString(getString(charCursor))
            ix
          } else parseNumberStringExponentPart(ix + 1, appendChar(charCursor, 'e'))
        } else parseNumberStringFractionPart(ix + 1, maxIx, appendChar(charCursor, c.toChar))
      } else failNumberMantissaTooLong(ix)

    @tailrec def parseNumberStringIntegralPart(ix: Long, maxIx: Long, charCursor: Int): Long =
      if (ix < maxIx) {
        val c = getInputByteOrEOI(ix) & 0xFF
        if ((c ^ 0x30) > 9) {
          if (c != '.') {
            if ((c | 0x20) != 'e') {
              receiver.onNumberString(getString(charCursor))
              ix
            } else parseNumberStringExponentPart(ix + 1, appendChar(charCursor, 'e'))
          } else parseNumberStringFractionPart(ix + 1, maxIx + 1, appendChar(charCursor, '.'))
        } else parseNumberStringIntegralPart(ix + 1, maxIx, appendChar(charCursor, c.toChar))
      } else failNumberMantissaTooLong(ix)

    /**
      * Produces the index of the first non-digit character as a return value, in `auxInt` the first non-digit character
      * and in `auxLong` the negative (!) parsed value or > 0 if the parsed value cannot be represented in a Long.
      */
    @tailrec def parseDigits(ix: Long, value: Long): Long = {
      // fetch 8 bytes (chars) at the same time with the first becoming the (left-most) MSB of the `octa` long
      val octa = getSafeOctaBigEndian(ix)

      // bytes containing ['0'..'9'] become 0..9, all others become >= 10
      val vMask = octa ^ 0x3030303030303030L

      // bytes containing ['0'..'9'] or [0xB0-0xB9] get their MSBit unset (< 0x80), all others have it set (>= 0x80)
      var mask = (vMask & 0x7f7f7f7f7f7f7f7fL) + 0x7676767676767676L

      // bytes containing ['0'..'9'] become zero, all others 0x80
      mask = (octa | mask) & 0x8080808080808080L

      val nlz        = java.lang.Long.numberOfLeadingZeros(mask)
      val digitCount = nlz >> 3

      val d0 = vMask >>> 56
      def d1 = (vMask << 8) >>> 56
      def d2 = (vMask << 16) >>> 56
      def d3 = (vMask << 24) >>> 56
      def d4 = (vMask << 32) >>> 56
      def d5 = (vMask << 40) >>> 56
      def d6 = (vMask << 48) >>> 56
      def d7 = vMask & 0xFFL

      def v1 = value * 10 - d0
      def v2 = value * 100 - d0 * 10 - d1
      def v3 = value * 1000 - d0 * 100 - d1 * 10 - d2
      def v4 = value * 10000 - d0 * 1000 - d1 * 100 - d2 * 10 - d3
      def v5 = value * 100000 - d0 * 10000 - d1 * 1000 - d2 * 100 - d3 * 10 - d4
      def v6 = value * 1000000 - d0 * 100000 - d1 * 10000 - d2 * 1000 - d3 * 100 - d4 * 10 - d5
      def v7 = value * 10000000 - d0 * 1000000 - d1 * 100000 - d2 * 10000 - d3 * 1000 - d4 * 100 - d5 * 10 - d6
      def v8 =
        value * 100000000 - d0 * 10000000 - d1 * 1000000 - d2 * 100000 - d3 * 10000 - d4 * 1000 - d5 * 100 - d6 * 10 - d7

      def returnWithV(ix: Long, value: Long, stopChar: Long): Long = {
        auxInt = stopChar.toInt
        auxLong = value
        ix
      }

      import Long.{MinValue ⇒ LMV}
      digitCount match {
        case 0 ⇒ returnWithV(ix, value, octa >>> 56)
        case 1 ⇒ returnWithV(ix + 1, if (0 >= value && value >= LMV / 10) v1 else 1, (octa << 8) >>> 56)
        case 2 ⇒ returnWithV(ix + 2, if (0 >= value && value >= LMV / 100) v2 else 1, (octa << 16) >>> 56)
        case 3 ⇒ returnWithV(ix + 3, if (0 >= value && value >= LMV / 1000) v3 else 1, (octa << 24) >>> 56)
        case 4 ⇒ returnWithV(ix + 4, if (0 >= value && value >= LMV / 10000) v4 else 1, (octa << 32) >>> 56)
        case 5 ⇒ returnWithV(ix + 5, if (0 >= value && value >= LMV / 100000) v5 else 1, (octa << 40) >>> 56)
        case 6 ⇒ returnWithV(ix + 6, if (0 >= value && value >= LMV / 1000000) v6 else 1, (octa << 48) >>> 56)
        case 7 ⇒ returnWithV(ix + 7, if (0 >= value && value >= LMV / 10000000) v7 else 1, (octa << 56) >>> 56)
        case 8 ⇒ parseDigits(ix + 8, if (0 >= value && value >= LMV / 100000000) v8 else 1)
      }
    }

    /**
      * Parses a JSON number and dispatches it to the [[Receiver]] either as
      * - Int
      * - Long
      * - Double
      * - or NumberString,
      *
      * whatever is the most efficient form that the number can be losslessly represented in.
      * Since [[Int]] is just the smaller variant of [[Long]] the core task is finding out,
      * whether the number fits losslessly in a [[Long]] or a [[Double]].
      * If neither is possible the fallback is always the NumberString, which
      * transports the number in exactly the format that is present in the JSON source.
      *
      * A side-task is to determine whether the number violates the JSON spec and produce the
      * respective error if that should be the case.
      *
      * @param idx the index to start parsing with
      * @param startIndex the index of the very first character that needs to go into the NumberString (if required)
      * @param negValue the initial value to start parsing with (as the negative of the actual number)
      * @param negative true if the JSON number is negative
      * @return the index after the last number character
      */
    def parseNumber(idx: Long, startIndex: Long, negValue: Long, negative: Boolean): Long = {
      def dispatchNumberString(ix: Long) = {
        receiver.onNumberString(getString(appendAll(startIndex, ix, 0)))
        ix
      }
      def dispatchIntOrLong(ix: Long, negValue: Long) = {
        var long = negValue
        if (negative || negValue != Long.MinValue && { long = -negValue; true }) {
          if (Util.isInt(long)) receiver.onInt(long.toInt)
          else receiver.onLong(long)
          ix
        } else dispatchNumberString(ix)
      }
      def dispatchDouble(ix: Long, double: Double) = {
        receiver.onDouble(if (negative) double else -double)
        ix
      }

      var ix                      = idx
      var stopChar                = 0
      var maxNumberMantissaDigits = config.maxNumberMantissaDigits
      var negMantissa =
        if (negValue == 0) {
          stopChar = getInputByteOrEOI(ix) & 0xFF
          if ((stopChar ^ 0x30) < 10) failSyntaxError(idx, "'.', 'e' or 'E'", stopChar.toLong)
          negValue
        } else {
          ix = parseDigits(idx, negValue)
          stopChar = auxInt
          auxLong
        }
      if (negMantissa <= 0) { // otherwise the integral part (before the decimal point) doesn't fit into 63 bit
        var negFractionDigits = 0
        if (stopChar == '.') {
          val ix0 = ix + 1
          maxNumberMantissaDigits += 1
          ix = parseDigits(ix0, negMantissa)
          negFractionDigits = (ix0 - ix).toInt
          if (negFractionDigits == 0) failSyntaxError(ix0, "DIGIT", stopChar.toLong)
          stopChar = auxInt
          negMantissa = auxLong
        }
        if (ix - idx >= maxNumberMantissaDigits) failNumberMantissaTooLong(ix)
        if (negMantissa <= 0) { // otherwise the mantissa (value with the decimal point removed) doesn't fit into 63 bit
          var expNeg    = false
          var expDigits = 0
          val posExp =
            if ((stopChar | 0x20) == 'e') {
              val c = getInputByteOrEOI(ix + 1) & 0xFF
              expNeg = c == '-'
              val ix0 = ix + (if (expNeg || c == '+') 2 else 1)
              ix = parseDigits(ix0, 0)
              expDigits = (ix - ix0).toInt
              if (expDigits == 0) failSyntaxError(ix0, "DIGIT", auxInt.toLong)
              val e = -auxLong.toInt
              if (e < 0 || e > config.maxNumberAbsExponent) failNumberExponentTooLarge(ix)
              e
            } else 0
          val exp = if (expNeg) negFractionDigits - posExp else negFractionDigits + posExp
          if (exp != 0) {
            if (exp > 0) {
              if (exp < 19 && negMantissa > long10pow(exp << 1)) {
                // the value is an integer that fits into a 63 bit Long
                dispatchIntOrLong(ix, negMantissa * long10pow((exp << 1) + 1))
              } else if (negMantissa > -(1L << 53) && exp < 23) {
                // the value is an integer that can be represented losslessly by a Double
                dispatchDouble(ix, negMantissa * double10pow(exp))
              } else dispatchNumberString(ix)
            } else if (negMantissa > -(1L << 53) && exp > -23) {
              // the value is a decimal number that can be represented losslessly by a Double
              dispatchDouble(ix, negMantissa.toDouble / double10pow(-exp))
            } else dispatchNumberString(ix)
          } else dispatchIntOrLong(ix, negMantissa) // normal, unscaled integer
        } else parseNumberStringFractionPart(ix, idx + maxNumberMantissaDigits.toLong - 2, appendAll(startIndex, ix, 0))
      } else parseNumberStringIntegralPart(ix, idx + maxNumberMantissaDigits.toLong - 2, appendAll(startIndex, ix, 0))
    }

    def parseNegNumber(ix: Long): Long = {
      val c = getInputByteOrEOI(ix) & 0xFF
      val x = c ^ 0x30L
      if (x <= 9) parseNumber(ix + 1, ix - 1, -x, negative = true)
      else failSyntaxError(ix, "DIGIT", c.toLong)
    }

    def parseEscapeSeq(ix: Long, charCursor: Int): Long = {
      var i = ix + 1
      val c =
        (getInputByteOrEOI(ix): @switch) match {
          case '"'  ⇒ '"'
          case '/'  ⇒ '/'
          case '\\' ⇒ '\\'
          case 'b'  ⇒ '\b'
          case 'f'  ⇒ '\f'
          case 'n'  ⇒ '\n'
          case 'r'  ⇒ '\r'
          case 't'  ⇒ '\t'
          case 'u' ⇒
            if (ix >= inputLen - 4) failIllegalEscapeSeq(ix)
            val q                       = ia.quadByteBigEndian(input, i)
            @inline def hd(c: Int): Int = InputAccess.ForByteArray.unsafeByte(HexDigits, c.toLong).toInt

            val x = (hd(q >>> 24) << 12) | (hd((q << 8) >>> 24) << 8) | (hd((q << 16) >>> 24) << 4) | hd(q & 0xFF)
            if (x < 0) failIllegalEscapeSeq(i)
            i = ix + 5
            x.toChar
          case _ ⇒ failIllegalEscapeSeq(ix)
        }
      auxInt = appendChar(charCursor, c)
      i
    }

    def parseMultiByteUtf8Char(ix: Long, b1: Int, charCursor: Int): Long = {
      val byteCount = Integer.numberOfLeadingZeros(~b1) - 25
      val b2        = getInputByteOrEOI(ix).toInt
      var cc        = charCursor
      val cp = (byteCount | 0x80) ^ (b2 & 0xC0) match {
        case 1 ⇒
          if ((b1 & 0x1E) == 0) failIllegalUtf8(ix - 1)
          (b1 << 6) ^ (b2 ^ 0xF80)
        case 2 ⇒
          val b3 = getInputByteOrEOI(ix + 1).toInt
          val c  = (b1 << 12) ^ (b2 << 6) ^ (b3 ^ 0xFFFE1F80)
          if ((b1 == 0xE0 && (b2 & 0xE0) == 0x80) || (b3 & 0xC0) != 0x80 || ((c >> 11) == 0x1b)) failIllegalUtf8(ix - 1)
          c
        case 3 ⇒
          val b3 = getInputByteOrEOI(ix + 1).toInt
          val b4 = getInputByteOrEOI(ix + 2).toInt
          val c  = (b1 << 18) ^ (b2 << 12) ^ (b3 << 6) ^ (b4 ^ 0x381F80)
          if ((b3 & 0xC0) != 0x80 || (b4 & 0xC0) != 0x80 || c < 0x010000 || c > 0x10FFFF) failIllegalUtf8(ix - 1)
          cc = appendChar(charCursor, (0xD7C0 + (c >> 10)).toChar) // high surrogate
          0xDC00 + (c & 0x3FF) // low surrogate
        case _ ⇒ failIllegalUtf8(ix - 1)
      }
      auxInt = appendChar(cc, cp.toChar)
      ix + byteCount
    }

    @tailrec def parseUtf8String(ix: Long, charCursor: Int): Long = {
      // fetch 8 bytes (chars) at the same time with the first becoming the (left-most) MSB of the `octa` long
      val octa     = getSafeOctaBigEndian(ix)
      val octa7bit = octa & 0x7f7f7f7f7f7f7f7fL

      // mask '"' characters: only '"' and 0xA2 become 0x80, all others become < 0x80
      val qMask = (octa7bit ^ 0x5d5d5d5d5d5d5d5dL) + 0x0101010101010101L

      // mask '\' characters: only '\' and 0xAF become 0x80, all others become < 0x80
      val bMask = (octa7bit ^ 0x2323232323232323L) + 0x0101010101010101L

      // mask ctrl characters (0 - 0x1F): only ctrl chars and [0x80 - 0x9F] get their high-bit set
      var mask = (octa | 0x1f1f1f1f1f1f1f1fL) - 0x2020202020202020L

      // the special chars '"', '\', 8-bit (> 127) and ctrl chars become 0x80, all normal chars zero
      mask = (octa | qMask | bMask | mask) & 0x8080808080808080L

      val nlz       = java.lang.Long.numberOfLeadingZeros(mask)
      val charCount = nlz >> 3 // the number of "good" normal chars before a special char [0..8]

      // in order to decrease instruction dependencies we always speculatively write all 8 chars to the char buffer,
      // independently of how many are actually "good" chars, this keeps the pipelines maximally busy
      ensureCharsLen(charCursor + 8)
      chars(charCursor) = (octa >>> 56).toChar
      chars(charCursor + 1) = ((octa << 8) >>> 56).toChar
      chars(charCursor + 2) = ((octa << 16) >>> 56).toChar
      chars(charCursor + 3) = ((octa << 24) >>> 56).toChar
      chars(charCursor + 4) = ((octa << 32) >>> 56).toChar
      chars(charCursor + 5) = ((octa << 40) >>> 56).toChar
      chars(charCursor + 6) = ((octa << 48) >>> 56).toChar
      chars(charCursor + 7) = (octa & 0xFFL).toChar

      val newCursor = charCursor + charCount
      if (newCursor > maxStringLength) failStringTooLong(skipWhiteSpace(index))

      if (nlz < 64) {
        val byteMask     = 0x8000000000000000L >>> nlz
        val byteMask7bit = byteMask & ~octa // mask that only selects the respective 7-bit variants from qMask or bMask
        val nextIx       = ix + charCount + 1
        if ((qMask & byteMask7bit) != 0) { // first special char is '"'
          receiver.onChars(newCursor, chars)
          nextIx
        } else if ((bMask & byteMask7bit) != 0) { // first special char is '\'
          parseUtf8String(parseEscapeSeq(nextIx, newCursor), auxInt)
        } else if ((octa & byteMask) != 0) { // first special char is 8-bit
          parseUtf8String(parseMultiByteUtf8Char(nextIx, ((octa << nlz) >> 56).toInt, newCursor), auxInt)
        } else { // first special char is a ctrl char
          failSyntaxError(ix + charCount, "JSON string character", (octa << nlz) >>> 56)
        }
      } else parseUtf8String(ix + 8, newCursor) // we have written 8 normal chars, so recurse immediately
    }

    def pushArray(ix: Long): Long =
      if (level < 64) {
        levelType <<= 1
        level += 1
        receiver.onArrayStart()
        state = EXPECT_ARRAY_VALUE_OR_BREAK
        ix
      } else failOverflow(ix, "This JSON parser does not support more than 64 Array/Object nesting levels")

    def pushMap(ix: Long): Long =
      if (level < 64) {
        levelType = (levelType << 1) | 1
        level += 1
        receiver.onMapStart()
        state = EXPECT_MAP_KEY_OR_BREAK
        ix
      } else failOverflow(ix, "This JSON parser does not support more than 64 Array/Object nesting levels")

    def popLevel(nextIx: Long): Long = {
      level -= 1
      levelType >>>= 1
      state = if (level > 0) {
        if ((levelType & 1) == 0) EXPECT_COMMA_AND_ARRAY_VALUE_OR_BREAK
        else EXPECT_COMMA_AND_MAP_KEY_OR_BREAK
      } else EXPECT_END_OF_INPUT
      receiver.onBreak()
      nextIx
    }

    def parseValue(c: Long, nextIx: Long): Long =
      (InputAccess.ForByteArray.unsafeByte(TokenTable, c): @switch) match {
        case DQUOTE      ⇒ parseUtf8String(nextIx, 0)
        case MAP_START   ⇒ pushMap(nextIx)
        case ARRAY_START ⇒ pushArray(nextIx)
        case LOWER_N     ⇒ parseNull(nextIx - 1)
        case LOWER_F     ⇒ parseFalse(nextIx)
        case LOWER_T     ⇒ parseTrue(nextIx - 1)
        case MINUS       ⇒ parseNegNumber(nextIx)
        case DIGIT       ⇒ parseNumber(nextIx, nextIx - 1, '0' - c, negative = false)
        case _           ⇒ failSyntaxError(nextIx - 1, "JSON value", c)
      }

    def parseArrayValueOrBreak(c: Long, nextIx: Long): Long =
      if (c != ']') {
        state = EXPECT_COMMA_AND_ARRAY_VALUE_OR_BREAK
        parseValue(c, nextIx)
      } else popLevel(nextIx)

    def parseCommaAndArrayValueOrBreak(c: Long, nextIx: Long): Long =
      if (c == ',') {
        var c  = getInputByteOrEOI(nextIx) & 0xFFL
        var ix = nextIx + 1
        if (c <= ' ') {
          ix = skipWhiteSpace(ix) + 1
          c = auxLong
        }
        parseValue(c, ix)
      } else if (c == ']') popLevel(nextIx)
      else failSyntaxError(nextIx, "',' or ']'", c)

    def parseMapKeyOrBreak(c: Long, nextIx: Long): Long =
      if (c != '}') {
        if (c == '"') {
          state = EXPECT_COLON_AND_MAP_VALUE
          parseUtf8String(nextIx, 0)
        } else failSyntaxError(nextIx, "JSON object member or '}'", c)
      } else popLevel(nextIx)

    def parseCommaAndMapKeyOrBreak(c: Long, nextIx: Long): Long =
      if (c == ',') {
        var c  = getInputByteOrEOI(nextIx) & 0xFFL
        var ix = nextIx + 1
        if (c <= ' ') {
          ix = skipWhiteSpace(ix) + 1
          c = auxLong
        }
        if (c == '"') {
          state = EXPECT_COLON_AND_MAP_VALUE
          parseUtf8String(ix, 0)
        } else failSyntaxError(nextIx, "'\"'", c)
      } else if (c == '}') popLevel(nextIx)
      else failSyntaxError(nextIx, "',' or '}'", c)

    def parseColonAndMapValue(c: Long, nextIx: Long): Long =
      if (c == ':') {
        var c  = getInputByteOrEOI(nextIx) & 0xFFL
        var ix = nextIx + 1
        if (c <= ' ') {
          ix = skipWhiteSpace(ix) + 1
          c = auxLong
        }
        state = EXPECT_COMMA_AND_MAP_KEY_OR_BREAK
        parseValue(c, ix)
      } else failSyntaxError(nextIx, "':'", c)

    def parseEndOfInput(c: Long): Long =
      if (c == EOI) {
        receiver.onEndOfInput()
        index
      } else failSyntaxError(index, "end of input", c)

    var nextIx = index + 1
    var c      = getInputByteOrEOI(index) & 0xFFL
    if (c <= ' ') {
      nextIx = skipWhiteSpace(nextIx) + 1
      c = auxLong
    }
    (state: @switch) match {
      case EXPECT_ARRAY_VALUE_OR_BREAK           ⇒ parseArrayValueOrBreak(c, nextIx)
      case EXPECT_COMMA_AND_ARRAY_VALUE_OR_BREAK ⇒ parseCommaAndArrayValueOrBreak(c, nextIx)
      case EXPECT_MAP_KEY_OR_BREAK               ⇒ parseMapKeyOrBreak(c, nextIx)
      case EXPECT_COLON_AND_MAP_VALUE            ⇒ parseColonAndMapValue(c, nextIx)
      case EXPECT_COMMA_AND_MAP_KEY_OR_BREAK     ⇒ parseCommaAndMapKeyOrBreak(c, nextIx)
      case EXPECT_VALUE                          ⇒ state = EXPECT_END_OF_INPUT; parseValue(c, nextIx)
      case EXPECT_END_OF_INPUT                   ⇒ parseEndOfInput(c)
    }
  }

  @tailrec private def skipWhiteSpace(ix: Long): Long = {
    // fetch 8 bytes (chars) at the same time with the first becoming the (left-most) MSByte of the `octa` long
    val octa = getSafeOctaBigEndian(ix)

    // bytes containing [0..0x20] or [0x80-0xA0] get their MSBit unset (< 0x80), all others have it set (>= 0x80)
    var mask = (octa & 0x7f7f7f7f7f7f7f7fL) + 0x5f5f5f5f5f5f5f5fL

    // bytes containing [0..0x20] become zero, all others 0x80
    mask = (octa | mask) & 0x8080808080808080L

    val nlz = java.lang.Long.numberOfLeadingZeros(mask)
    if (nlz < 64) {
      auxLong = (octa << nlz) >>> 56 // "return" the first non-whitespace char
      ix + (nlz >> 3) // and the index of the first non-whitespace char
    } else skipWhiteSpace(ix + 8)
  }

  @inline private def getInputByteUnsafe(ix: Long): Byte = ia.unsafeByte(input, ix)

  @inline private def getInputByteOrEOI(ix: Long): Byte = if (ix < inputLen) getInputByteUnsafe(ix) else -1

  private def getSafeOctaBigEndian(ix: Long): Long = {
    def partialOcta: Long =
      inputLen - ix match {
        case 0 ⇒ -1L // == EOI
        case 1 ⇒ (ia.unsafeByte(input, ix).toLong << 56) | 0xFFFFFFFFFFFFFFL
        case 2 ⇒ (ia.doubleByteBigEndian(input, ix).toLong << 48) | 0xFFFFFFFFFFFFL
        case 3 ⇒
          (ia.doubleByteBigEndian(input, ix).toLong << 48) |
            (ia.unsafeByte(input, ix + 2).toLong << 40) | 0xFFFFFFFFFFL
        case 4 ⇒ (ia.quadByteBigEndian(input, ix).toLong << 32) | 0xFFFFFFFFL
        case 5 ⇒
          (ia.quadByteBigEndian(input, ix).toLong << 32) |
            (ia.unsafeByte(input, ix + 4).toLong << 24) | 0xFFFFFFL
        case 6 ⇒
          (ia.quadByteBigEndian(input, ix).toLong << 32) |
            (ia.doubleByteBigEndian(input, ix + 4).toLong << 16) | 0xFFFFL
        case 7 ⇒
          (ia.quadByteBigEndian(input, ix).toLong << 32) |
            (ia.doubleByteBigEndian(input, ix + 4).toLong << 16) |
            (ia.unsafeByte(input, ix + 6).toLong << 8) | 0xFFL
      }
    if (ix <= inputLenMinus8) ia.octaByteBigEndian(input, ix)
    else partialOcta
  }

  private def ensureCharsLen(len: Int): Unit =
    if (len > chars.length) {
      chars = util.Arrays.copyOf(chars, math.max(chars.length << 1, len))
    }

  @inline private def getString(len: Int): String =
    if (len > 0) new String(chars, 0, len) else ""

  private def failStringTooLong(ix: Long) =
    failOverflow(ix, s"JSON String longer than configured maximum of $maxStringLength characters")
  private def failNumberMantissaTooLong(ix: Long) =
    failOverflow(ix, s"JSON number mantissa longer than configured maximum of ${config.maxNumberMantissaDigits} digits")
  private def failNumberExponentTooLarge(ix: Long) =
    failOverflow(ix, s"absolute JSON number exponent larger than configured maximum ${config.maxNumberAbsExponent}")
  private def failOverflow(ix: Long, msg: String) =
    throw new Borer.Error.Overflow(pos(ix), msg)
  private def failIllegalUtf8(ix: Long) =
    throw new Borer.Error.InvalidInputData(pos(ix), "Illegal UTF-8 character encoding")
  private def failIllegalEscapeSeq(ix: Long) =
    throw new Borer.Error.InvalidInputData(pos(ix), "Illegal JSON escape sequence")
  private def failSyntaxError(ix: Long, expected: String) =
    throw new Borer.Error.InvalidInputData(pos(ix), "Invalid JSON syntax")
  private def failSyntaxError(ix: Long, expected: String, actual: Long) = {
    val actualChar =
      if (actual == EOI) "end of input"
      else if (Character.isISOControl(actual.toInt)) s"'\\u${Integer.toHexString(actual.toInt)}'"
      else s"'${actual.toChar}'"
    throw new Borer.Error.InvalidInputData(pos(ix), expected, actualChar)
  }
}

private[borer] object JsonParser {

  trait Config {
    def maxStringLength: Int
    def maxNumberMantissaDigits: Int
    def maxNumberAbsExponent: Int
  }

  private[this] final val _creator: Receiver.ParserCreator[Any, JsonParser.Config] =
    (input, config, inputAccess) ⇒ new JsonParser[Any](input, config)(inputAccess)

  def creator[Input, C <: JsonParser.Config]: Receiver.ParserCreator[Input, C] =
    _creator.asInstanceOf[Receiver.ParserCreator[Input, C]]

  private final val EXPECT_ARRAY_VALUE_OR_BREAK           = 0
  private final val EXPECT_COMMA_AND_ARRAY_VALUE_OR_BREAK = 1
  private final val EXPECT_MAP_KEY_OR_BREAK               = 2
  private final val EXPECT_COLON_AND_MAP_VALUE            = 3
  private final val EXPECT_COMMA_AND_MAP_KEY_OR_BREAK     = 4
  private final val EXPECT_VALUE                          = 5
  private final val EXPECT_END_OF_INPUT                   = 6

  private final val DQUOTE      = 0
  private final val MAP_START   = 1
  private final val ARRAY_START = 2
  private final val LOWER_N     = 3
  private final val LOWER_F     = 4
  private final val LOWER_T     = 5
  private final val MINUS       = 6
  private final val DIGIT       = 7

  private final val EOI = 0xFF // illegal initial UTF8 byte, used as "End-Of-Input" marker

  private final val TokenTable: Array[Byte] = {
    val array = new Array[Byte](256)
    java.util.Arrays.fill(array, -1.toByte)
    array('"'.toInt) = DQUOTE
    array('{'.toInt) = MAP_START
    array('['.toInt) = ARRAY_START
    array('n'.toInt) = LOWER_N
    array('f'.toInt) = LOWER_F
    array('t'.toInt) = LOWER_T
    array('-'.toInt) = MINUS
    array('0'.toInt) = DIGIT
    array('1'.toInt) = DIGIT
    array('2'.toInt) = DIGIT
    array('3'.toInt) = DIGIT
    array('4'.toInt) = DIGIT
    array('5'.toInt) = DIGIT
    array('6'.toInt) = DIGIT
    array('7'.toInt) = DIGIT
    array('8'.toInt) = DIGIT
    array('9'.toInt) = DIGIT
    array
  }

  private final val HexDigits: Array[Byte] = {
    val array = new Array[Byte](256)
    java.util.Arrays.fill(array, -1.toByte)
    array('0'.toInt) = 0x00
    array('1'.toInt) = 0x01
    array('2'.toInt) = 0x02
    array('3'.toInt) = 0x03
    array('4'.toInt) = 0x04
    array('5'.toInt) = 0x05
    array('6'.toInt) = 0x06
    array('7'.toInt) = 0x07
    array('8'.toInt) = 0x08
    array('9'.toInt) = 0x09
    array('A'.toInt) = 0x0A
    array('B'.toInt) = 0x0B
    array('C'.toInt) = 0x0C
    array('D'.toInt) = 0x0D
    array('E'.toInt) = 0x0E
    array('F'.toInt) = 0x0F
    array('a'.toInt) = 0x0a
    array('b'.toInt) = 0x0b
    array('c'.toInt) = 0x0c
    array('d'.toInt) = 0x0d
    array('e'.toInt) = 0x0e
    array('f'.toInt) = 0x0f
    array
  }

  // powers of 10 which can be represented exactly in a `Double`
  private final val double10pow: Array[Double] = Array(1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9, 1e10, 1e11,
    1e12, 1e13, 1e14, 1e15, 1e16, 1e17, 1e18, 1e19, 1e20, 1e21, 1e22)

  private final val long10pow = Array(
    Long.MinValue / 1,
    1,
    Long.MinValue / 10,
    10,
    Long.MinValue / 100,
    100,
    Long.MinValue / 1000,
    1000,
    Long.MinValue / 10000,
    10000,
    Long.MinValue / 100000,
    100000,
    Long.MinValue / 1000000,
    1000000,
    Long.MinValue / 10000000,
    10000000,
    Long.MinValue / 100000000,
    100000000,
    Long.MinValue / 1000000000,
    1000000000,
    Long.MinValue / 10000000000L,
    10000000000L,
    Long.MinValue / 100000000000L,
    100000000000L,
    Long.MinValue / 1000000000000L,
    1000000000000L,
    Long.MinValue / 10000000000000L,
    10000000000000L,
    Long.MinValue / 100000000000000L,
    100000000000000L,
    Long.MinValue / 1000000000000000L,
    1000000000000000L,
    Long.MinValue / 10000000000000000L,
    10000000000000000L,
    Long.MinValue / 100000000000000000L,
    100000000000000000L,
    Long.MinValue / 1000000000000000000L,
    1000000000000000000L)
}
