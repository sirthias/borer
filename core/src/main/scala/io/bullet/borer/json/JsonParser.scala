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

  private[this] val allowDoubleParsing = !config.readDecimalNumbersOnlyAsNumberStrings
  private[this] var chars: Array[Char] = new Array[Char](config.initialCharbufferSize)
  private[this] var state: Int         = EXPECT_VALUE
  private[this] var auxLong: Long      = _
  private[this] var valueCursor: Long  = _
  private[this] var level: Int         = _ // valid range: 0..64

  // keeps the type of each level as a bit map: 0 -> Array, 1 -> Map
  // the current level is always the LSB (bit 0)
  private[this] var levelType: Long = _

  private[this] var nextChar: Int = nextCharAfterWhitespace()

  def valueIndex: Long = valueCursor - 1

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

    @inline def parseNull(): Int = {
      val quad = input.readQuadByteBigEndianPaddedFF()
      if ((quad >>> 8) == 0x00756c6c) { // "ull"
        nextChar = nextCharAfterWhitespace(quad & 0xFF)
        receiver.onNull()
        DataItem.Null
      } else failSyntaxError(-5, "`null`")
    }

    @inline def parseFalse(): Int =
      if (input.readQuadByteBigEndianPaddedFF() == 0x616c7365) { // "alse"
        fetchNextChar()
        receiver.onBoolean(value = false)
        DataItem.Boolean
      } else failSyntaxError(-6, "`false`")

    @inline def parseTrue(): Int = {
      val quad = input.readQuadByteBigEndianPaddedFF()
      if ((quad >>> 8) == 0x00727565) { // "rue"
        nextChar = nextCharAfterWhitespace(quad & 0xFF)
        receiver.onBoolean(value = true)
        DataItem.Boolean
      } else failSyntaxError(-5, "`true`")
    }

    def parseNumberStringExponentPart(len: Int): Int = {
      val c      = input.readByteOrFF().toInt
      var newLen = len
      if (c != '-' && c != '+') input.moveCursor(-1) else newLen += 1
      newLen = parseDigits(0L, newLen)
      if (newLen == len) failSyntaxError("DIGIT")
      val exp = -auxLong.toInt
      if (exp < 0 || config.maxNumberAbsExponent < exp) failNumberExponentTooLarge(newLen)
      input.moveCursor(-1) // "unread" the stopChar
      val numberString = input.precedingBytesAsAsciiString(newLen)
      input.moveCursor(1)
      nextChar = nextCharAfterWhitespace(nextChar)
      receiver.onNumberString(numberString)
      DataItem.NumberString
    }

    // produces the new number string length as a return value, in `nextChar` the first non-digit character (stopchar)
    // and in `auxLong` the negative (!) parsed value or > 0 if the parsed value cannot be represented in a Long.
    @tailrec def parseDigits(value: Long, len: Int): Int = {
      // fetch 8 bytes (chars) at the same time with the first becoming the (left-most) MSB of the `octa` long
      val octa = input.readOctaByteBigEndianPaddedFF()
      // bytes containing ['0'..'9'] become 0..9, all others become >= 10
      val vMask = octa ^ 0X3030303030303030L

      // bytes containing ['0'..'9'] or [0xB0-0xB9] get their MSBit unset (< 0x80), all others have it set (>= 0x80)
      var mask = (vMask & 0X7F7F7F7F7F7F7F7FL) + 0X7676767676767676L

      // bytes containing ['0'..'9'] become zero, all others 0x80
      mask = (octa | mask) & 0X8080808080808080L

      val nlz        = java.lang.Long.numberOfLeadingZeros(mask)
      val digitCount = nlz >> 3

      val d0 = vMask >>> 56
      val d1 = vMask << 8 >>> 56
      val d2 = vMask << 16 >>> 56
      val d3 = vMask << 24 >>> 56
      val d4 = vMask << 32 >>> 56
      val d5 = vMask << 40 >>> 56
      val d6 = vMask << 48 >>> 56
      val d7 = vMask & 0XFFL

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
        input.moveCursor(digitCount - 7)
        nextChar = (stopChar >>> 56).toInt
        auxLong = value
        len + digitCount
      }

      digitCount match {
        case 0 => returnWithV(value, octa)
        case 1 => returnWithV(if (0 >= value && value >= Long.MinValue / 10) v1 else 1, octa << 8)
        case 2 => returnWithV(if (0 >= value && value >= Long.MinValue / 100) v2 else 1, octa << 16)
        case 3 => returnWithV(if (0 >= value && value >= Long.MinValue / 1000) v3 else 1, octa << 24)
        case 4 => returnWithV(if (0 >= value && value >= Long.MinValue / 10000) v4 else 1, octa << 32)
        case 5 => returnWithV(if (0 >= value && value >= Long.MinValue / 100000) v5 else 1, octa << 40)
        case 6 => returnWithV(if (0 >= value && value >= Long.MinValue / 1000000) v6 else 1, octa << 48)
        case 7 => returnWithV(if (0 >= value && value >= Long.MinValue / 10000000) v7 else 1, octa << 56)
        case 8 => parseDigits(if (0 >= value && value >= Long.MinValue / 100000000) v8 else 1, len + 8)
      }
    }

    // Parses a JSON number and dispatches it to the [[Receiver]] either as
    // - Int
    // - Long
    // - Double
    // - or NumberString,
    //
    // whatever is the most efficient form that the number can be easily and losslessly represented in.
    // Since [[Int]] is just the smaller variant of [[Long]] the core task is finding out, without much overhead,
    // whether the number fits losslessly in a [[Long]] or a [[Double]].
    // If neither is possible the fallback is always the NumberString, which
    // transports the number in exactly the format that is present in the JSON source.
    //
    // A side-task is to determine whether the number violates the JSON spec and produce the
    // respective error if that should be the case.
    //
    // @param negValue the initial value to start parsing with (as the negative of the actual number)
    // @param strLen the number of already parsed characters belonging to the number string
    // @param negative true if the JSON number is negative
    // @return DataItem code for the value the Receiver received
    def parseNumber(negValue: Long, strLen: Int, negative: Boolean): Int = {
      @inline def dispatchNumberString(len: Int) = {
        input.moveCursor(-1) // "unread" stopchar
        receiver.onNumberString(input.precedingBytesAsAsciiString(len))
        input.moveCursor(1) // re-consume stopchar
        DataItem.NumberString
      }
      @inline def dispatchDouble(d: Double) = {
        receiver.onDouble(if (negative || d == 0.0) d else -d)
        DataItem.Double
      }
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
          val result = dispatchNumberString(len)
          nextChar = nextCharAfterWhitespace(stopChar)
          result
        } else parseNumberStringExponentPart(len + 1)

      var len               = strLen
      var stopChar          = 0
      var maxMantissaEndLen = len + config.maxNumberMantissaDigits - 1
      var negMantissa =
        if (negValue == 0) {
          stopChar = input.readByteOrFF() & 0xFF
          if ((stopChar ^ 0x30) < 10) {
            nextChar = stopChar
            failSyntaxError("'.', 'e' or 'E'")
          }
          negValue
        } else {
          len = parseDigits(negValue, len)
          stopChar = nextChar
          auxLong
        }
      if (negMantissa <= 0) { // otherwise the integral part (before the decimal point) doesn't fit into 63 bit
        var negFractionDigits = 0
        if (stopChar == '.') {
          val len0 = len + 1
          maxMantissaEndLen += 1
          len = parseDigits(negMantissa, len0)
          stopChar = nextChar
          negMantissa = auxLong
          negFractionDigits = len0 - len
          if (negFractionDigits == 0) failSyntaxError("DIGIT")
        }
        if (len > maxMantissaEndLen) failNumberMantissaTooLong(-len)
        if (negMantissa <= 0) { // otherwise the mantissa (value with the decimal point removed) doesn't fit into 63 bit
          var expNeg    = false
          var expDigits = 0
          val posExp =
            if ((stopChar | 0x20) == 'e') {
              val c = input.readByteOrFF() & 0xFF
              expNeg = c == '-'
              val len0 = if (!expNeg && c != '+') {
                input.moveCursor(-1)
                len + 1
              } else len + 2
              len = parseDigits(0L, len0)
              stopChar = nextChar
              expDigits = len - len0
              if (expDigits == 0) failSyntaxError("DIGIT")
              val e = -auxLong.toInt
              if (e < 0 || config.maxNumberAbsExponent < e) failNumberExponentTooLarge(-expDigits)
              e
            } else 0
          val exp = if (expNeg) negFractionDigits - posExp else negFractionDigits + posExp
          val result =
            if (exp != 0) {
              if (exp > 0) {
                if (exp < 19 && negMantissa > long10pow(exp << 1)) {
                  // the value is an integer that fits into a 63 bit Long
                  dispatchIntOrLong(len, negMantissa * long10pow((exp << 1) + 1))
                } else if (allowDoubleParsing && negMantissa > -(1L << 53) && exp < 23) {
                  // the value is an integer that can be represented losslessly by a Double
                  dispatchDouble(negMantissa * double10pow(exp))
                } else dispatchNumberString(len)
              } else if (allowDoubleParsing && negMantissa > -(1L << 53) && exp > -23) {
                // the value is a decimal number that can be represented losslessly by a Double
                dispatchDouble(negMantissa.toDouble / double10pow(-exp))
              } else dispatchNumberString(len)
            } else dispatchIntOrLong(len, negMantissa) // normal, unscaled integer
          nextChar = nextCharAfterWhitespace(stopChar)
          result
        } else parseNumberStringExponentPartOrDispatchNumberString(len, stopChar)
      } else {
        if (len > maxMantissaEndLen) failNumberMantissaTooLong(-len)
        if (stopChar == '.' && { len = parseDigits(1L, len + 1); stopChar = nextChar; len > maxMantissaEndLen + 1 }) {
          failNumberMantissaTooLong(-len)
        } else parseNumberStringExponentPartOrDispatchNumberString(len, stopChar)
      }
    }

    def parseNegNumber(): Int = {
      val c = input.readByteOrFF() & 0xFF
      val x = c ^ 0X30L
      if (x > 9) {
        nextChar = c
        failSyntaxError("DIGIT")
      } else parseNumber(-x, strLen = 2, negative = true)
    }

    def parseEscapeSeq(charCursor: Int): Int = {
      var cc = charCursor
      val c =
        (input.readByteOrFF(): @switch) match {
          case '"'  => '"'
          case '/'  => '/'
          case '\\' => '\\'
          case 'b'  => '\b'
          case 'f'  => '\f'
          case 'n'  => '\n'
          case 't'  => '\t'
          case 'r' =>
            if (input.readDoubleByteBigEndianPaddedFF() == 0x5c6e) { // are we immediately followed by a \n ?
              cc = appendChar(cc, '\r')
              '\n'
            } else {               // no, not a \r\n sequence
              input.moveCursor(-2) // unread our failed test for /n
              '\r'
            }
          case 'u' =>
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
            } else input.moveCursor(-2)
            x.toChar
          case _ => failIllegalEscapeSeq(-2)
        }
      appendChar(cc, c)
    }

    @tailrec def parseMultiByteUtf8Char(b1: Int, charCursor: Int): Int = {
      val byteCount = Integer.numberOfLeadingZeros(~b1) - 25
      val quad      = input.readQuadByteBigEndianPaddedFF()
      val b2        = quad >> 24
      var cc        = charCursor
      val cp = (byteCount | 0x80) ^ (b2 & 0xC0) match {
        case 1 =>
          if ((b1 & 0x1E) == 0) failIllegalUtf8(-5)
          (b1 << 6) ^ b2 ^ 0xF80
        case 2 =>
          val b3 = quad << 8 >> 24
          val c  = (b1 << 12) ^ (b2 << 6) ^ b3 ^ 0xFFFE1F80
          if ((b1 == 0xE0 && (b2 & 0xE0) == 0x80) || (b3 & 0xC0) != 0x80 || ((c >> 11) == 0x1b)) failIllegalUtf8(-5)
          c
        case 3 =>
          val b3 = quad << 8 >> 24
          val b4 = quad << 16 >> 24
          val c  = (b1 << 18) ^ (b2 << 12) ^ (b3 << 6) ^ b4 ^ 0x381F80
          if ((b3 & 0xC0) != 0x80 || (b4 & 0xC0) != 0x80 || c < 0x010000 || c > 0x10FFFF) failIllegalUtf8(-5)
          cc = appendChar(charCursor, (0xD7C0 + (c >> 10)).toChar) // high surrogate
          0xDC00 + (c & 0x3FF)                                     // low surrogate
        case _ => failIllegalUtf8(-5)
      }
      cc = appendChar(cc, cp.toChar)
      input.moveCursor(byteCount - 3)

      // if the next byte is also an 8-bit character (which is not that unlikely) we decode that as well right away
      val nextByte = quad << (byteCount << 3) >> 24
      if (nextByte >= 0) {
        input.moveCursor(-1) // "unread" the last byte
        cc
      } else parseMultiByteUtf8Char(nextByte, cc)
    }

    @tailrec def parseUtf8String(charCursor: Int): Int = {
      // fetch 8 bytes (chars) at the same time with the first becoming the (left-most) MSB of the `octa` long
      val octa     = input.readOctaByteBigEndianPaddedFF()
      val octa7bit = octa & 0X7F7F7F7F7F7F7F7FL

      // mask '"' characters: only '"' and 0xA2 become 0x80, all others become < 0x80
      val qMask = (octa7bit ^ 0X5D5D5D5D5D5D5D5DL) + 0X0101010101010101L

      // mask '\' characters: only '\' and 0xAF become 0x80, all others become < 0x80
      val bMask = (octa7bit ^ 0X2323232323232323L) + 0X0101010101010101L

      // mask ctrl characters (0 - 0x1F): only ctrl chars and 0xA0 - 0xFF get their high-bit set
      var mask = (octa | 0X1F1F1F1F1F1F1F1FL) - 0X2020202020202020L

      // the special chars '"', '\', 8-bit (> 127) and ctrl chars become 0x80, all normal chars zero
      mask = (octa | qMask | bMask | mask) & 0X8080808080808080L

      val nlz       = java.lang.Long.numberOfLeadingZeros(mask)
      val charCount = nlz >> 3 // the number of "good" normal chars before a special char [0..8]

      // in order to decrease instruction dependencies we always speculatively write all 8 chars to the char buffer,
      // independently of how many are actually "good" chars, this keeps CPU pipelines maximally busy
      ensureCharsLen(charCursor + 8)
      chars(charCursor) = (octa >>> 56).toChar           // here it would be nice
      chars(charCursor + 1) = (octa << 8 >>> 56).toChar  // to have access
      chars(charCursor + 2) = (octa << 16 >>> 56).toChar // to a JVM intrinsic for the
      chars(charCursor + 3) = (octa << 24 >>> 56).toChar // x86 PDEP instruction
      chars(charCursor + 4) = (octa << 32 >>> 56).toChar // which could do this "widening"
      chars(charCursor + 5) = (octa << 40 >>> 56).toChar // of 8 x 8-bit to 16-bit values
      chars(charCursor + 6) = (octa << 48 >>> 56).toChar // in 2 steps à 4 conversion each in parallel
      chars(charCursor + 7) = (octa & 0XFFL).toChar      // see: https://www.felixcloutier.com/x86/pdep

      val newCursor = charCursor + charCount
      if (nlz < 64) { // do we have a special char anywhere?
        val stopChar0 = octa << nlz
        val stopChar  = (stopChar0 >>> 56).toInt // the first special char after `charCount` good chars
        input.moveCursor(charCount - 6) // move the cursor to the 2nd char after the stopChar
        if (stopChar == '"') {
          val c = stopChar0 << 8 >>> 56 // the char after the '"' (or zero, if we haven't read it yet)
          nextChar = if (c <= 0x20) {
            input.moveCursor(-1)
            nextCharAfterWhitespace()
          } else c.toInt
          receiver.onChars(chars, newCursor)
          DataItem.Chars
        } else if (stopChar == '\\') {
          input.moveCursor(-1)
          parseUtf8String(parseEscapeSeq(newCursor))
        } else if (stopChar > 127) {
          input.moveCursor(-1)
          parseUtf8String(parseMultiByteUtf8Char(stopChar.toByte.toInt, newCursor))
        } else { // stopChar char is a ctrl char
          failSyntaxError(-2, "JSON string character")
        }
      } else parseUtf8String(newCursor) // we have written 8 normal chars, so recurse
    }

    def pushMap(): Int =
      if (level < 64) {
        levelType = (levelType << 1) | 1
        level += 1
        fetchNextChar()
        state = if (nextChar == '}') EXPECT_MAP_BREAK else EXPECT_MAP_KEY
        receiver.onMapStart()
        DataItem.MapStart
      } else failOverflow(0, "This JSON parser does not support more than 64 Array/Object nesting levels")

    def pushArray(): Int =
      if (level < 64) {
        levelType <<= 1
        level += 1
        fetchNextChar()
        state = if (nextChar == ']') EXPECT_ARRAY_BREAK else EXPECT_ARRAY_VALUE
        receiver.onArrayStart()
        DataItem.ArrayStart
      } else failOverflow(0, "This JSON parser does not support more than 64 Array/Object nesting levels")

    def popLevel(): Int = {
      level -= 1
      levelType >>>= 1
      fetchNextChar()
      state = if (level > 0) {
        val tpe = levelType.toInt & 1
        if (nextChar == ',') {
          fetchNextChar()
          tpe
        } else 2 | tpe
      } else EXPECT_END_OF_INPUT
      receiver.onBreak()
      DataItem.Break
    }

    // parses the next data item and dispatches it to the receiver,
    // returns the data item code,
    // consumes the subsequent character and leaves it in `nextChar`
    def parseValue(nextStateAfterComma: Int, nextStateNoComma: Int): Int = {
      val result = (TokenTable(nextChar): @switch) match {
        case DQUOTE      => parseUtf8String(0)
        case MAP_START   => return pushMap()
        case ARRAY_START => return pushArray()
        case LOWER_N     => parseNull()
        case LOWER_F     => parseFalse()
        case LOWER_T     => parseTrue()
        case MINUS       => parseNegNumber()
        case DIGIT       => parseNumber(0X30L - nextChar, 1, negative = false)
        case _           => failSyntaxError("JSON value")
      }

      state = if (nextChar == ',') {
        fetchNextChar()
        nextStateAfterComma
      } else nextStateNoComma
      result
    }

    def parseMapKey(): Int =
      if (nextChar == '"') {
        val result = parseUtf8String(0)
        if (nextChar == ':') {
          fetchNextChar()
          state = EXPECT_MAP_VALUE
          result
        } else failSyntaxError("':'")
      } else failSyntaxError("'\"'")

    def parseArrayBreak(): Int =
      if (nextChar == ']') popLevel()
      else failSyntaxError("',' or ']'")

    def parseMapBreak(): Int =
      if (nextChar == '}') popLevel()
      else failSyntaxError("',' or '}'")

    def parseEndOfInput(): Int =
      if (nextChar == EOI) {
        receiver.onEndOfInput()
        DataItem.EndOfInput
      } else failSyntaxError("end of input")

    valueCursor = input.cursor
    val result = (state: @switch) match {
      case EXPECT_ARRAY_VALUE  => parseValue(EXPECT_ARRAY_VALUE, EXPECT_ARRAY_BREAK)
      case EXPECT_MAP_KEY      => parseMapKey()
      case EXPECT_ARRAY_BREAK  => parseArrayBreak()
      case EXPECT_MAP_BREAK    => parseMapBreak()
      case EXPECT_MAP_VALUE    => parseValue(EXPECT_MAP_KEY, EXPECT_MAP_BREAK)
      case EXPECT_VALUE        => parseValue(ILLEGAL_CHAR, EXPECT_END_OF_INPUT)
      case EXPECT_END_OF_INPUT => parseEndOfInput()
      case _                   => failSyntaxError(-2, "End of Input")
    }
    input.releaseBeforeCursor()
    result
  }

  @inline private def fetchNextChar(): Unit = nextChar = nextCharAfterWhitespace()

  private def nextCharAfterWhitespace(): Int = nextCharAfterWhitespace(input.readByteOrFF() & 0xFF)

  private def nextCharAfterWhitespace(nextChar: Int): Int = {
    @tailrec def skip8(): Int = {
      // fetch 8 bytes (chars) at the same time with the first becoming the (left-most) MSByte of the `octa` long
      val octa = input.readOctaByteBigEndianPaddedFF()

      // bytes containing [0..0x20] or [0x80-0xA0] get their MSBit unset (< 0x80), all others have it set (>= 0x80)
      var mask = (octa & 0X7F7F7F7F7F7F7F7FL) + 0X5F5F5F5F5F5F5F5FL

      // bytes containing [0..0x20] become zero, all others 0x80
      mask = (octa | mask) & 0X8080808080808080L

      val nlz = java.lang.Long.numberOfLeadingZeros(mask)
      if (nlz < 64) {
        input.moveCursor((nlz >> 3) - 7)
        (octa << nlz >>> 56).toInt // "return" the first non-whitespace char
      } else skip8()
    }

    def skip1(): Int = {
      val c = input.readByteOrFF() & 0xFF
      if (c <= 0x20) { // 2nd char is also whitespace,
        skip8()        // so there are probably (lots) more coming
      } else c
    }

    if (nextChar <= 0x20) skip1() else nextChar
  }

  @inline private def ensureCharsLen(len: Int): Unit =
    if (len > chars.length) {
      if (len > config.maxStringLength) failStringTooLong(-len)
      val newLen = math.max(chars.length << 1, len)
      chars = util.Arrays.copyOf(chars, newLen)
    }

  private def failStringTooLong(offset: Int) =
    failOverflow(offset, s"JSON String longer than configured maximum of ${config.maxStringLength} characters")
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
    throw new Borer.Error.InvalidInputData(pos(offset), s"Invalid JSON syntax, expected $expected")
  private def failSyntaxError(expected: String) = {
    val actualChar =
      if (nextChar == EOI) "end of input"
      else if (Character.isISOControl(nextChar)) f"'\\u$nextChar%04x'"
      else s"'${nextChar.toChar}'"
    throw new Borer.Error.InvalidInputData(pos(-1), expected, actualChar)
  }

  private def pos(offset: Int) = input.position(input.cursor + offset.toLong)
}

private[borer] object JsonParser {

  trait Config {
    def readDecimalNumbersOnlyAsNumberStrings: Boolean
    def maxStringLength: Int
    def maxNumberMantissaDigits: Int
    def maxNumberAbsExponent: Int
    def initialCharbufferSize: Int
  }

  final private[this] val _creator: Receiver.ParserCreator[Input, JsonParser.Config] =
    (input, config) => new JsonParser[Input](input, config)

  def creator[In <: Input, Conf <: JsonParser.Config]: Receiver.ParserCreator[In, Conf] =
    _creator.asInstanceOf[Receiver.ParserCreator[In, Conf]]

  final private val EXPECT_ARRAY_VALUE  = 0
  final private val EXPECT_MAP_KEY      = 1
  final private val EXPECT_ARRAY_BREAK  = 2
  final private val EXPECT_MAP_BREAK    = 3
  final private val EXPECT_MAP_VALUE    = 4
  final private val EXPECT_VALUE        = 5
  final private val EXPECT_END_OF_INPUT = 6
  final private val ILLEGAL_CHAR        = 7

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
