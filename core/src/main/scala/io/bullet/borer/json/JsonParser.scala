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
final private[borer] class JsonParser[In <: Input](val input: In, val config: JsonParser.Config)
    extends Receiver.Parser[In] {
  import JsonParser._

  private[this] val maxStringLength    = config.maxStringLength
  private[this] var chars: Array[Char] = new Array[Char](256)
  private[this] var state: Int         = EXPECT_VALUE
  private[this] var auxInt: Int        = _
  private[this] var auxLong: Long      = _

  private[this] var level: Int = _ // valid range: 0..64

  // keeps the type of each level as a bit map: 0 -> Array, 1 -> Map
  // the current level is always the LSB (bit 0)
  private[this] var levelType: Long = _

  private[this] var lastCursorPlusOne: Long = _

  def lastCursor: Long = lastCursorPlusOne - 1

  /**
    * Reads the next data item from the input and sends it to the given [[Receiver]].
    * The given [[Receiver]] receives exactly one call to one of its methods.
    * The returned `Int` is the [[DataItem]] code for the value the [[Receiver]] received.
    */
  def pull(receiver: Receiver): Int = {

    @inline def appendChar(charCursor: Int, c: Char): Int = {
      val newCursor = charCursor + 1
      ensureCharsLen(newCursor)
      chars(charCursor) = c
      newCursor
    }

    @inline def parseNull(): Int =
      if (input.unread(1).readQuadByteBigEndianPaddedFF() == 0x6e756c6c) { // "null"
        receiver.onNull()
        DataItem.Null
      } else failSyntaxError(-4, "`null`")

    @inline def parseFalse(): Int =
      if (input.readQuadByteBigEndianPaddedFF() == 0x616c7365) { // "alse"
        receiver.onBool(value = false)
        DataItem.Bool
      } else failSyntaxError(-5, "`false`")

    @inline def parseTrue(): Int =
      if (input.unread(1).readQuadByteBigEndianPaddedFF() == 0x74727565) { // "true"
        receiver.onBool(value = true)
        DataItem.Bool
      } else failSyntaxError(-4, "`true`")

    def parseNumberStringExponentPart(len: Int): Int = {
      val c      = input.readByteOrFF().toInt
      var newLen = len
      if (c != '-' && c != '+') input.unread(1) else newLen += 1
      newLen = parseDigits(0l, newLen)
      input.unread(1) // unread stop char
      if (newLen == len) failSyntaxError(0, "DIGIT", input.readByteOrFF().toInt)
      val exp = -auxLong.toInt
      if (exp < 0 || config.maxNumberAbsExponent < exp) failNumberExponentTooLarge(newLen)
      receiver.onNumberString(input.precedingBytesAsAsciiString(newLen))
      DataItem.NumberString
    }

    /**
      * Produces the new number string length as a return value, in `auxInt` the first non-digit character (stop-char)
      * and in `auxLong` the negative (!) parsed value or > 0 if the parsed value cannot be represented in a Long.
      * The input cursor is left _after_ the stop-char, i.e. the next read will not include the stop-char!
      */
    @tailrec def parseDigits(value: Long, len: Int): Int = {
      // fetch 8 bytes (chars) at the same time with the first becoming the (left-most) MSB of the `octa` long
      val octa = input.readOctaByteBigEndianPaddedFF()
      // bytes containing ['0'..'9'] become 0..9, all others become >= 10
      val vMask = octa ^ 0x3030303030303030l

      // bytes containing ['0'..'9'] or [0xB0-0xB9] get their MSBit unset (< 0x80), all others have it set (>= 0x80)
      var mask = (vMask & 0x7f7f7f7f7f7f7f7fl) + 0x7676767676767676l

      // bytes containing ['0'..'9'] become zero, all others 0x80
      mask = (octa | mask) & 0x8080808080808080l

      val nlz        = java.lang.Long.numberOfLeadingZeros(mask)
      val digitCount = nlz >> 3

      val d0         = vMask >>> 56
      val d1         = vMask << 8 >>> 56
      val d2         = vMask << 16 >>> 56
      @inline def d3 = vMask << 24 >>> 56
      @inline def d4 = vMask << 32 >>> 56
      @inline def d5 = vMask << 40 >>> 56
      @inline def d6 = vMask << 48 >>> 56
      @inline def d7 = vMask & 0xffl

      @inline def v1 =
        value * 10 - d0
      @inline def v2 =
        value * 100 - d0 * 10 - d1
      @inline def v3 =
        value * 1000 - d0 * 100 - d1 * 10 - d2
      @inline def v4 =
        value * 10000 - d0 * 1000 - d1 * 100 - d2 * 10 - d3
      @inline def v5 =
        value * 100000 - d0 * 10000 - d1 * 1000 - d2 * 100 - d3 * 10 - d4
      @inline def v6 =
        value * 1000000 - d0 * 100000 - d1 * 10000 - d2 * 1000 - d3 * 100 - d4 * 10 - d5
      @inline def v7 =
        value * 10000000 - d0 * 1000000 - d1 * 100000 - d2 * 10000 - d3 * 1000 - d4 * 100 - d5 * 10 - d6
      @inline def v8 =
        value * 100000000 - d0 * 10000000 - d1 * 1000000 - d2 * 100000 - d3 * 10000 - d4 * 1000 - d5 * 100 - d6 * 10 - d7

      @inline def returnWithV(value: Long, stopChar: Long): Int = {
        input.unread(7 - digitCount)
        auxInt = (stopChar >>> 56).toInt
        auxLong = value
        len + digitCount
      }

      digitCount match {
        case 0 ⇒ returnWithV(value, octa)
        case 1 ⇒ returnWithV(if (0 >= value && value >= Long.MinValue / 10) v1 else 1, octa << 8)
        case 2 ⇒ returnWithV(if (0 >= value && value >= Long.MinValue / 100) v2 else 1, octa << 16)
        case 3 ⇒ returnWithV(if (0 >= value && value >= Long.MinValue / 1000) v3 else 1, octa << 24)
        case 4 ⇒ returnWithV(if (0 >= value && value >= Long.MinValue / 10000) v4 else 1, octa << 32)
        case 5 ⇒ returnWithV(if (0 >= value && value >= Long.MinValue / 100000) v5 else 1, octa << 40)
        case 6 ⇒ returnWithV(if (0 >= value && value >= Long.MinValue / 1000000) v6 else 1, octa << 48)
        case 7 ⇒ returnWithV(if (0 >= value && value >= Long.MinValue / 10000000) v7 else 1, octa << 56)
        case 8 ⇒ parseDigits(if (0 >= value && value >= Long.MinValue / 100000000) v8 else 1, len + 8)
      }
    }

    /**
      * Parses a JSON number and dispatches it to the [[Receiver]] either as
      * - Int
      * - Long
      * - Double
      * - or NumberString,
      *
      * whatever is the most efficient form that the number can be easily and losslessly represented in.
      * Since [[Int]] is just the smaller variant of [[Long]] the core task is finding out, without much overhead,
      * whether the number fits losslessly in a [[Long]] or a [[Double]].
      * If neither is possible the fallback is always the NumberString, which
      * transports the number in exactly the format that is present in the JSON source.
      *
      * A side-task is to determine whether the number violates the JSON spec and produce the
      * respective error if that should be the case.
      *
      * @param negValue the initial value to start parsing with (as the negative of the actual number)
      * @param strLen the number of already parsed characters belonging to the number string
      * @param negative true if the JSON number is negative
      * @return [[DataItem]] code for the value the [[Receiver]] received
      */
    def parseNumber(negValue: Long, strLen: Int, negative: Boolean): Int = {
      @inline def dispatchNumberString(len: Int) = {
        receiver.onNumberString(input.precedingBytesAsAsciiString(len))
        DataItem.NumberString
      }
      @inline def dispatchDouble(d: Double) = { receiver.onDouble(if (negative) d else -d); DataItem.Double }
      @inline def dispatchIntOrLong(len: Int, negValue: Long) = {
        var long = negValue
        if (negative || negValue != Long.MinValue && { long = -negValue; true }) {
          if (Util.isInt(long)) {
            receiver.onInt(long.toInt)
            DataItem.Int
          } else {
            receiver.onLong(long)
            DataItem.Long
          }
        } else dispatchNumberString(len)
      }
      @inline def parseNumberStringExponentPartOrDispatchNumberString(len: Int, stopChar: Int) =
        if ((stopChar | 0x20) != 'e') {
          input.unread(1) // unread stop char
          dispatchNumberString(len)
        } else parseNumberStringExponentPart(len + 1)

      var len               = strLen
      var stopChar          = 0
      var maxMantissaEndLen = len + config.maxNumberMantissaDigits - 1
      var negMantissa =
        if (negValue == 0) {
          stopChar = input.readByteOrFF() & 0xFF
          if ((stopChar ^ 0x30) < 10) failSyntaxError(-1, "'.', 'e' or 'E'", stopChar)
          negValue
        } else {
          len = parseDigits(negValue, len)
          stopChar = auxInt
          auxLong
        }
      if (negMantissa <= 0) { // otherwise the integral part (before the decimal point) doesn't fit into 63 bit
        var negFractionDigits = 0
        if (stopChar == '.') {
          val len0 = len + 1
          maxMantissaEndLen += 1
          len = parseDigits(negMantissa, len0)
          stopChar = auxInt
          negMantissa = auxLong
          negFractionDigits = len0 - len
          if (negFractionDigits == 0) failSyntaxError(-1, "DIGIT", stopChar)
        }
        if (len > maxMantissaEndLen) failNumberMantissaTooLong(-len)
        if (negMantissa <= 0) { // otherwise the mantissa (value with the decimal point removed) doesn't fit into 63 bit
          var expNeg    = false
          var expDigits = 0
          val posExp =
            if ((stopChar | 0x20) == 'e') {
              val c = input.readByteOrFF() & 0xFF
              expNeg = c == '-'
              val len0 = len + (if (!expNeg && c != '+') {
                                  input.unread(1); 1
                                } else 2)
              len = parseDigits(0l, len0)
              expDigits = len - len0
              if (expDigits == 0) failSyntaxError(0, "DIGIT", auxInt)
              val e = -auxLong.toInt
              if (e < 0 || config.maxNumberAbsExponent < e) failNumberExponentTooLarge(-expDigits)
              e
            } else 0
          input.unread(1) // unread stop-char
          val exp = if (expNeg) negFractionDigits - posExp else negFractionDigits + posExp
          if (exp != 0) {
            if (exp > 0) {
              if (exp < 19 && negMantissa > long10pow(exp << 1)) {
                // the value is an integer that fits into a 63 bit Long
                dispatchIntOrLong(len, negMantissa * long10pow((exp << 1) + 1))
              } else if (negMantissa > -(1l << 53) && exp < 23) {
                // the value is an integer that can be represented losslessly by a Double
                dispatchDouble(negMantissa * double10pow(exp))
              } else dispatchNumberString(len)
            } else if (negMantissa > -(1l << 53) && exp > -23) {
              // the value is a decimal number that can be represented losslessly by a Double
              dispatchDouble(negMantissa.toDouble / double10pow(-exp))
            } else dispatchNumberString(len)
          } else dispatchIntOrLong(len, negMantissa) // normal, unscaled integer
        } else parseNumberStringExponentPartOrDispatchNumberString(len, stopChar)
      } else {
        if (len > maxMantissaEndLen) failNumberMantissaTooLong(-len)
        if (stopChar == '.' && { len = parseDigits(1l, len + 1); stopChar = auxInt; len > maxMantissaEndLen + 1 }) {
          failNumberMantissaTooLong(-len)
        } else parseNumberStringExponentPartOrDispatchNumberString(len, stopChar)
      }
    }

    def parseNegNumber(): Int = {
      val c = input.readByteOrFF() & 0xFF
      val x = c ^ 0x30l
      if (x <= 9) parseNumber(-x, strLen = 2, negative = true)
      else failSyntaxError(-1, "DIGIT", c)
    }

    def parseEscapeSeq(charCursor: Int): Int = {
      var cc = charCursor
      val c =
        (input.readByteOrFF(): @switch) match {
          case '"'  ⇒ '"'
          case '/'  ⇒ '/'
          case '\\' ⇒ '\\'
          case 'b'  ⇒ '\b'
          case 'f'  ⇒ '\f'
          case 'n'  ⇒ '\n'
          case 't'  ⇒ '\t'
          case 'r' ⇒
            if (input.readDoubleByteBigEndianPaddedFF() == 0x5c6e) {
              cc = appendChar(cc, '\r')
              '\n'
            } else {
              input.unread(2)
              '\r'
            }
          case 'u' ⇒
            @inline def hd(c: Int): Int = HexDigits(c).toInt

            var q = input.readQuadByteBigEndianPaddedFF()
            var x = (hd(q >>> 24) << 12) | (hd(q << 8 >>> 24) << 8) | (hd(q << 16 >>> 24) << 4) | hd(q & 0xFF)
            if (x < 0) failIllegalEscapeSeq(-4)

            // we immediately check whether there is another `u` sequence following and decode that as well if so
            if (input.readDoubleByteBigEndianPaddedFF() == 0x5c75) {
              q = input.readQuadByteBigEndianPaddedFF()
              cc = appendChar(cc, x.toChar)
              x = (hd(q >>> 24) << 12) | (hd(q << 8 >>> 24) << 8) | (hd(q << 16 >>> 24) << 4) | hd(q & 0xFF)
              if (x < 0) failIllegalEscapeSeq(-4)
            } else input.unread(2)
            x.toChar
          case _ ⇒ failIllegalEscapeSeq(-2)
        }
      appendChar(cc, c)
    }

    @tailrec def parseMultiByteUtf8Char(b1: Int, charCursor: Int): Int = {
      val byteCount = Integer.numberOfLeadingZeros(~b1) - 25
      val quad      = input.readQuadByteBigEndianPaddedFF()
      val b2        = quad >> 24
      var cc        = charCursor
      val cp = (byteCount | 0x80) ^ (b2 & 0xC0) match {
        case 1 ⇒
          if ((b1 & 0x1E) == 0) failIllegalUtf8(-5)
          (b1 << 6) ^ b2 ^ 0xF80
        case 2 ⇒
          val b3 = quad << 8 >> 24
          val c  = (b1 << 12) ^ (b2 << 6) ^ b3 ^ 0xFFFE1F80
          if ((b1 == 0xE0 && (b2 & 0xE0) == 0x80) || (b3 & 0xC0) != 0x80 || ((c >> 11) == 0x1b)) failIllegalUtf8(-5)
          c
        case 3 ⇒
          val b3 = quad << 8 >> 24
          val b4 = quad << 16 >> 24
          val c  = (b1 << 18) ^ (b2 << 12) ^ (b3 << 6) ^ b4 ^ 0x381F80
          if ((b3 & 0xC0) != 0x80 || (b4 & 0xC0) != 0x80 || c < 0x010000 || c > 0x10FFFF) failIllegalUtf8(-5)
          cc = appendChar(charCursor, (0xD7C0 + (c >> 10)).toChar) // high surrogate
          0xDC00 + (c & 0x3FF)                                     // low surrogate
        case _ ⇒ failIllegalUtf8(-5)
      }
      cc = appendChar(cc, cp.toChar)
      input.unread(3 - byteCount)

      // if the next byte is also an 8-bit character (which is not that unlikely) we decode that as well right away
      val nextByte = quad << (byteCount << 3) >> 24
      if (nextByte >= 0) {
        input.unread(1)
        cc
      } else parseMultiByteUtf8Char(nextByte, cc)
    }

    @tailrec def parseUtf8String(charCursor: Int): Int = {
      // fetch 8 bytes (chars) at the same time with the first becoming the (left-most) MSB of the `octa` long
      val octa     = input.readOctaByteBigEndianPaddedFF()
      val octa7bit = octa & 0x7f7f7f7f7f7f7f7fl

      // mask '"' characters: only '"' and 0xA2 become 0x80, all others become < 0x80
      val qMask = (octa7bit ^ 0x5d5d5d5d5d5d5d5dl) + 0x0101010101010101l

      // mask '\' characters: only '\' and 0xAF become 0x80, all others become < 0x80
      val bMask = (octa7bit ^ 0x2323232323232323l) + 0x0101010101010101l

      // mask ctrl characters (0 - 0x1F): only ctrl chars and [0x80 - 0x9F] get their high-bit set
      var mask = (octa | 0x1f1f1f1f1f1f1f1fl) - 0x2020202020202020l

      // the special chars '"', '\', 8-bit (> 127) and ctrl chars become 0x80, all normal chars zero
      mask = (octa | qMask | bMask | mask) & 0x8080808080808080l

      val nlz       = java.lang.Long.numberOfLeadingZeros(mask)
      val charCount = nlz >> 3 // the number of "good" normal chars before a special char [0..8]

      // in order to decrease instruction dependencies we always speculatively write all 8 chars to the char buffer,
      // independently of how many are actually "good" chars, this keeps CPU pipelines maximally busy
      ensureCharsLen(charCursor + 8)
      chars(charCursor) = (octa >>> 56).toChar
      chars(charCursor + 1) = (octa << 8 >>> 56).toChar
      chars(charCursor + 2) = (octa << 16 >>> 56).toChar
      chars(charCursor + 3) = (octa << 24 >>> 56).toChar
      chars(charCursor + 4) = (octa << 32 >>> 56).toChar
      chars(charCursor + 5) = (octa << 40 >>> 56).toChar
      chars(charCursor + 6) = (octa << 48 >>> 56).toChar
      chars(charCursor + 7) = (octa & 0xffl).toChar

      val newCursor = charCursor + charCount
      if (newCursor > maxStringLength) failStringTooLong(-charCursor)

      if (nlz < 64) {
        val byteMask     = 0x8000000000000000l >>> nlz
        val byteMask7bit = byteMask & ~octa // mask that only selects the respective 7-bit variants from qMask or bMask
        input.unread(7 - charCount)        // unread all chars after the stop-char
        if ((qMask & byteMask7bit) != 0) { // first special char is '"'
          receiver.onChars(newCursor, chars)
          DataItem.Chars
        } else if ((bMask & byteMask7bit) != 0) { // first special char is '\'
          parseUtf8String(parseEscapeSeq(newCursor))
        } else if ((octa & byteMask) != 0) { // first special char is 8-bit
          parseUtf8String(parseMultiByteUtf8Char((octa << nlz >> 56).toInt, newCursor))
        } else { // first special char is a ctrl char
          failSyntaxError(-1, "JSON string character", (octa << nlz >>> 56).toInt)
        }
      } else parseUtf8String(newCursor) // we have written 8 normal chars, so recurse immediately
    }

    def pushArray(): Int =
      if (level < 64) {
        levelType <<= 1
        level += 1
        receiver.onArrayStart()
        state = EXPECT_ARRAY_VALUE_OR_BREAK
        DataItem.ArrayStart
      } else failOverflow(0, "This JSON parser does not support more than 64 Array/Object nesting levels")

    def pushMap(): Int =
      if (level < 64) {
        levelType = (levelType << 1) | 1
        level += 1
        receiver.onMapStart()
        state = EXPECT_MAP_KEY_OR_BREAK
        DataItem.MapStart
      } else failOverflow(0, "This JSON parser does not support more than 64 Array/Object nesting levels")

    def popLevel(): Int = {
      level -= 1
      levelType >>>= 1
      state = if (level > 0) levelType.toInt & 1 else EXPECT_END_OF_INPUT
      markCursorForValue()
      receiver.onBreak()
      DataItem.Break
    }

    def parseValue(c: Int): Int = {
      markCursorForValue()
      (TokenTable(c): @switch) match {
        case DQUOTE      ⇒ parseUtf8String(0)
        case MAP_START   ⇒ pushMap()
        case ARRAY_START ⇒ pushArray()
        case LOWER_N     ⇒ parseNull()
        case LOWER_F     ⇒ parseFalse()
        case LOWER_T     ⇒ parseTrue()
        case MINUS       ⇒ parseNegNumber()
        case DIGIT       ⇒ parseNumber(0x30l - c, 1, negative = false)
        case _           ⇒ failSyntaxError(-1, "JSON value", c)
      }
    }

    def parseArrayValueOrBreak(c: Int): Int =
      if (c != ']') {
        state = EXPECT_COMMA_AND_ARRAY_VALUE_OR_BREAK
        parseValue(c)
      } else popLevel()

    def parseCommaAndArrayValueOrBreak(c: Int): Int =
      if (c == ',') parseValue(nextCharAfterWhitespace())
      else if (c == ']') popLevel()
      else failSyntaxError(-1, "',' or ']'", c)

    def parseMapKeyOrBreak(c: Int): Int =
      if (c != '}') {
        if (c == '"') {
          state = EXPECT_COLON_AND_MAP_VALUE
          markCursorForValue()
          parseUtf8String(0)
        } else failSyntaxError(-1, "JSON object member or '}'", c)
      } else popLevel()

    def parseCommaAndMapKeyOrBreak(c: Int): Int =
      if (c == ',') {
        val c = nextCharAfterWhitespace()
        state = EXPECT_COLON_AND_MAP_VALUE
        markCursorForValue()
        if (c == '"') parseUtf8String(0)
        else failSyntaxError(-1, "'\"'", c)
      } else if (c == '}') popLevel()
      else failSyntaxError(-1, "',' or '}'", c)

    def parseColonAndMapValue(c: Int): Int =
      if (c == ':') {
        state = EXPECT_COMMA_AND_MAP_KEY_OR_BREAK
        parseValue(nextCharAfterWhitespace())
      } else failSyntaxError(-1, "':'", c)

    def parseEndOfInput(c: Int): Int =
      if (c == EOI) {
        markCursorForValue()
        receiver.onEndOfInput()
        DataItem.EndOfInput
      } else failSyntaxError(-1, "end of input", c)

    val c = nextCharAfterWhitespace()
    (state: @switch) match {
      case EXPECT_COMMA_AND_ARRAY_VALUE_OR_BREAK ⇒ parseCommaAndArrayValueOrBreak(c)
      case EXPECT_COMMA_AND_MAP_KEY_OR_BREAK     ⇒ parseCommaAndMapKeyOrBreak(c)
      case EXPECT_ARRAY_VALUE_OR_BREAK           ⇒ parseArrayValueOrBreak(c)
      case EXPECT_MAP_KEY_OR_BREAK               ⇒ parseMapKeyOrBreak(c)
      case EXPECT_COLON_AND_MAP_VALUE            ⇒ parseColonAndMapValue(c)
      case EXPECT_VALUE                          ⇒ state = EXPECT_END_OF_INPUT; parseValue(c)
      case EXPECT_END_OF_INPUT                   ⇒ parseEndOfInput(c)
    }
  }

  private def nextCharAfterWhitespace(): Int = {
    @tailrec def skip8(): Int = {
      // fetch 8 bytes (chars) at the same time with the first becoming the (left-most) MSByte of the `octa` long
      val octa = input.readOctaByteBigEndianPaddedFF()

      // bytes containing [0..0x20] or [0x80-0xA0] get their MSBit unset (< 0x80), all others have it set (>= 0x80)
      var mask = (octa & 0x7f7f7f7f7f7f7f7fl) + 0x5f5f5f5f5f5f5f5fl

      // bytes containing [0..0x20] become zero, all others 0x80
      mask = (octa | mask) & 0x8080808080808080l

      val nlz = java.lang.Long.numberOfLeadingZeros(mask)
      if (nlz < 64) {
        input.unread(7 - (nlz >> 3))
        (octa << nlz >>> 56).toInt // "return" the first non-whitespace char
      } else skip8()
    }

    var c = input.readByteOrFF() & 0xFF
    if (c <= 0x20) { // 1st char is whitespace
      c = input.readByteOrFF() & 0xFF
      if (c <= 0x20) { // 2nd char is also whitespace,
        skip8()        // so there are probably (lots) more coming
      } else c
    } else c
  }

  @inline private def ensureCharsLen(len: Int): Unit =
    if (len > chars.length) {
      chars = util.Arrays.copyOf(chars, math.max(chars.length << 1, len))
    }

  private def failStringTooLong(offset: Int) =
    failOverflow(offset, s"JSON String longer than configured maximum of $maxStringLength characters")
  private def failNumberMantissaTooLong(offset: Int) =
    failOverflow(
      offset,
      s"JSON number mantissa longer than configured maximum of ${config.maxNumberMantissaDigits} digits")
  private def failNumberExponentTooLarge(offset: Int) =
    failOverflow(offset, s"absolute JSON number exponent larger than configured maximum ${config.maxNumberAbsExponent}")
  private def failOverflow(offset: Int, msg: String) =
    throw new Borer.Error.Overflow(pos(offset), msg)
  private def failIllegalUtf8(offset: Int) =
    throw new Borer.Error.InvalidInputData(pos(offset), "Illegal UTF-8 character encoding")
  private def failIllegalEscapeSeq(offset: Int) =
    throw new Borer.Error.InvalidInputData(pos(offset), "Illegal JSON escape sequence")
  private def failSyntaxError(offset: Int, expected: String) =
    throw new Borer.Error.InvalidInputData(pos(offset), "Invalid JSON syntax")
  private def failSyntaxError(offset: Int, expected: String, actual: Int) = {
    val actualChar =
      if (actual == EOI) "end of input"
      else if (Character.isISOControl(actual)) f"'\\u$actual%04x'"
      else s"'${actual.toChar}'"
    throw new Borer.Error.InvalidInputData(pos(offset), expected, actualChar)
  }

  private def markCursorForValue(): Unit = lastCursorPlusOne = input.cursor

  private def pos(offset: Int) = input.position(input.cursor + offset.toLong)
}

private[borer] object JsonParser {

  trait Config {
    def maxStringLength: Int
    def maxNumberMantissaDigits: Int
    def maxNumberAbsExponent: Int
  }

  final private[this] val _creator: Receiver.ParserCreator[Input, JsonParser.Config] =
    (input, config) ⇒ new JsonParser[Input](input, config)

  def creator[In <: Input, Conf <: JsonParser.Config]: Receiver.ParserCreator[In, Conf] =
    _creator.asInstanceOf[Receiver.ParserCreator[In, Conf]]

  final private val EXPECT_COMMA_AND_ARRAY_VALUE_OR_BREAK = 0
  final private val EXPECT_COMMA_AND_MAP_KEY_OR_BREAK     = 1
  final private val EXPECT_ARRAY_VALUE_OR_BREAK           = 2
  final private val EXPECT_MAP_KEY_OR_BREAK               = 3
  final private val EXPECT_COLON_AND_MAP_VALUE            = 4
  final private val EXPECT_VALUE                          = 5
  final private val EXPECT_END_OF_INPUT                   = 6

  final private val DQUOTE      = 0
  final private val MAP_START   = 1
  final private val ARRAY_START = 2
  final private val LOWER_N     = 3
  final private val LOWER_F     = 4
  final private val LOWER_T     = 5
  final private val MINUS       = 6
  final private val DIGIT       = 7

  final private val EOI = 0xFF // illegal initial UTF8 byte, used as "End-Of-Input" marker

  final private val TokenTable: Array[Byte] = {
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

  final private val HexDigits: Array[Byte] = {
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
  final private val double10pow: Array[Double] = Array(1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9, 1e10, 1e11,
    1e12, 1e13, 1e14, 1e15, 1e16, 1e17, 1e18, 1e19, 1e20, 1e21, 1e22)

  final private val long10pow = Array(
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
    Long.MinValue / 10000000000l,
    10000000000l,
    Long.MinValue / 100000000000l,
    100000000000l,
    Long.MinValue / 1000000000000l,
    1000000000000l,
    Long.MinValue / 10000000000000l,
    10000000000000l,
    Long.MinValue / 100000000000000l,
    100000000000000l,
    Long.MinValue / 1000000000000000l,
    1000000000000000l,
    Long.MinValue / 10000000000000000l,
    10000000000000000l,
    Long.MinValue / 100000000000000000l,
    100000000000000000l,
    Long.MinValue / 1000000000000000000l,
    1000000000000000000l)
}
