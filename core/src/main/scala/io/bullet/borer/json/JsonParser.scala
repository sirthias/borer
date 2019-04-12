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
import io.bullet.borer.internal.{CharArrayOut, Util}

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
  * - PosOverLong
  * - NegOverLong
  * - Float
  * - Double
  * - String
  * - Indefinite-Length Array
  * - Indefinite-Length Map
  *
  * Depending on the `nonLongNumberHandling` parameter, JSON numbers that cannot be represented as an [[Int]] or
  * [[Long]] are either produced as [[Double]] or [[String]] values.
  *
  * These data items are never produced:
  * - undefined
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
private[borer] final class JsonParser[Input](val input: Input)(implicit ia: InputAccess[Input])
    extends Receiver.Parser[Input] {
  import JsonParser._

  private[this] val inputLen           = ia.length(input)
  private[this] var chars: Array[Char] = new Array[Char](32)
  private[this] var aux: Int           = _

  /**
    * Reads the next data item from the input and sends it to the given [[Receiver]].
    * The [[Receiver]] receives exactly one call to one of its methods.
    * The returned `Long` is the index of the next byte to consume from the input
    * (and can be used for the subsequent call to this method).
    */
  @tailrec def pull(index: Long, receiver: Receiver): Long = {

    @inline def parseNull(ix: Long): Long =
      if (ix < inputLen - 3) {
        if (ia.quadByteBigEndian(input, ix) == 0x6e756c6c) { // "null"
          receiver.onNull()
          ix + 4
        } else failInvalidJsonSyntax(ix - 1)
      } else failUnexpectedEndOfInput(ix)

    @inline def parseFalse(ix: Long): Long =
      if (ix < inputLen - 3) {
        if (ia.quadByteBigEndian(input, ix) == 0x616c7365) { // "alse"
          receiver.onBool(value = false)
          ix + 4
        } else failInvalidJsonSyntax(ix - 1)
      } else failUnexpectedEndOfInput(ix)

    @inline def parseTrue(ix: Long): Long =
      if (ix < inputLen - 3) {
        if (ia.quadByteBigEndian(input, ix) == 0x74727565) { // "true"
          receiver.onBool(value = true)
          ix + 4
        } else failInvalidJsonSyntax(ix - 1)
      } else failUnexpectedEndOfInput(ix)

    @tailrec def parseNumberString(ix: Long, charCursor: Int): Long = {
      def result(): Long = {
        val lastChar = chars(charCursor - 1)
        if ('0' <= lastChar && lastChar <= '9') {
          receiver.onNumberString(getString(charCursor))
          ix
        } else failInvalidJsonSyntax(ix)
      }
      if (ix < inputLen) {
        val c = getInputByteUnsafe(ix) & 0xFFL
        if (toToken(c) >= MINUS) {
          parseNumberString(ix + 1, appendChar(ix, charCursor, c.toChar))
        } else result()
      } else result()
    }

    @tailrec def parseDecimalNumber(ix: Long,
                                    significand: Long,
                                    scale: Int,
                                    charCursor: Int,
                                    negative: Boolean): Long = {
      @inline def result(): Long =
        if (scale > 0) {
          var double = significand.toDouble / double10pow(scale)
          if (negative) double = -double
          val float = double.toFloat
          if (float.toDouble == double) receiver.onFloat(float)
          else receiver.onDouble(double)
          ix
        } else failInvalidJsonSyntax(ix)
      def breakOutToNumberString(c: Char) = parseNumberString(ix + 1, appendChar(ix, charCursor, c))
      if (ix < inputLen) {
        val c = getInputByteUnsafe(ix) & 0xFFL
        (toToken(c): @switch) match {
          case DIGIT0 | DIGIT19 ⇒
            val newScale = scale + 1
            // check whether we can safely multiply by 10 and add 9 and still have significand fit wholy into a Double
            if (significand < ((1L << 52) / 10 - 1) && newScale < double10pow.length) {
              val newSignificand = (significand << 3) + (significand << 1) + c - '0'
              parseDecimalNumber(ix + 1, newSignificand, newScale, appendChar(ix, charCursor, c.toChar), negative)
            } else breakOutToNumberString(c.toChar)
          case LETTER_E           ⇒ if (scale > 0) breakOutToNumberString(c.toChar) else failInvalidJsonSyntax(ix)
          case MINUS | PLUS | DOT ⇒ failInvalidJsonSyntax(ix)
          case _                  ⇒ result()
        }
      } else result()
    }

    @tailrec def parseNumber(ix: Long, value: Long, charCursor: Int, negative: Boolean): Long = {
      @inline def result(): Long = {
        val long = if (negative) -value else value
        if (Util.isInt(long)) receiver.onInt(long.toInt) else receiver.onLong(long)
        ix
      }
      def breakOutToNumberString(c: Char) = parseNumberString(ix + 1, appendChar(ix, charCursor, c))
      if (ix < inputLen) {
        val c = getInputByteUnsafe(ix) & 0xFFL
        if (value <= Long.MaxValue / 10) {
          val i     = ix + 1
          val mul10 = (value << 3) + (value << 1)
          (toToken(c): @switch) match {
            case DIGIT0 | DIGIT19 ⇒
              if (value == Long.MaxValue / 10 && (c == '8' || c == '9')) { // special case: Long.MinValue
                if (c == '8' && negative && (i >= inputLen || toToken(getInputByteUnsafe(i) & 0xFFL) < DIGIT0)) {
                  receiver.onLong(Long.MinValue)
                  i
                } else breakOutToNumberString(c.toChar)
              } else parseNumber(i, mul10 + c - '0', appendChar(ix, charCursor, c.toChar), negative)
            case DOT      ⇒ parseDecimalNumber(i, value, 0, appendChar(ix, charCursor, '.'), negative)
            case LETTER_E ⇒ breakOutToNumberString('e')
            case _        ⇒ result()
          }
        } else breakOutToNumberString(c.toChar)
      } else result()
    }

    def parseNumber0(ix: Long, negative: Boolean): Long = {
      @inline def result(): Long = {
        receiver.onInt(0)
        ix
      }
      def append(negChars: Long, posChars: Long) =
        if (negative) append3(ix, 0, negChars << 40) else append2(ix, 0, posChars << 48)
      if (ix < inputLen) {
        val c = getInputByteUnsafe(ix) & 0xFFL
        (toToken(c): @switch) match {
          case DOT              ⇒ parseDecimalNumber(ix + 1, 0, 0, append(0x2d302e, 0x302e), negative = false)
          case LETTER_E         ⇒ parseNumberString(ix + 1, append(0x2d3065, 0x3065))
          case DIGIT0 | DIGIT19 ⇒ failInvalidJsonSyntax(ix)
          case _                ⇒ result()
        }
      } else result()
    }

    def parseNegNumber(ix: Long): Long = {
      if (ix < inputLen) {
        val c = getInputByteUnsafe(ix).toChar
        if (c == '0') parseNumber0(ix + 1, negative = true)
        else if ('0' < c && c <= '9') {
          val charCursor = appendChar(ix, appendChar(ix, 0, '-'), c.toChar)
          parseNumber(ix + 1, (c - '0').toLong, charCursor, negative = true)
        } else failInvalidJsonSyntax(ix)
      } else failUnexpectedEndOfInput(ix)
    }

    def parseEscapeSeq(ix: Long, charCursor: Int): Long = {
      var i = ix + 1
      val c =
        getInputByte(ix).toChar match {
          case x @ ('"' | '/' | '\\') ⇒ x
          case 'b'                    ⇒ '\b'
          case 'f'                    ⇒ '\f'
          case 'n'                    ⇒ '\n'
          case 'r'                    ⇒ '\r'
          case 't'                    ⇒ '\t'
          case 'u' ⇒
            if (i < inputLen - 3) {
              def hexDigit(idx: Long): Int = {
                val x     = getInputByteUnsafe(idx) & 0xFF
                val value = (x & 0x1f) + ((x >> 6) * 0x19) - 0x10
                if ((value >> 4) == 0) value
                else failIllegalCodepoint(idx, x)
              }
              i += 4
              ((hexDigit(i - 4) << 12) | (hexDigit(i - 3) << 8) | (hexDigit(i - 2) << 4) | hexDigit(i - 1)).toChar
            } else failUnexpectedEndOfInput(ix)
          case _ ⇒ throw new Borer.Error.InvalidJsonData(pos(ix), "Illegal JSON escape sequence")
        }
      aux = appendChar(ix, charCursor, c)
      i
    }

    def parseMultiByteUtf8Char(ix: Long, charCursor: Int): Long = {
      @inline def trailingByte(i: Long): Int = {
        val x = getInputByte(i) & 0xFF
        if ((x >> 6) != 2) failIllegalUtf8(i)
        x & 0x3F
      }
      var c = getInputByte(ix) & 0xFF
      var i = ix + 1
      if ((c >> 6) == 3) { // 2, 3 or 4-byte UTF-8 char
        if ((c >> 5) == 7) { // 3 or 4-byte UTF-8 char
          if ((c >> 3) == 0x1E) { // 4-byte UTF-8 char
            c = (c & 7) << 6 | trailingByte(i)
            i += 1
          } else { // 3-byte UTF-8 char
            if ((c >> 4) != 0xE) failIllegalUtf8(i)
            c = c & 0xF
          }
          c = (c << 6) | trailingByte(i)
          i += 1
        } else { // 2-byte UTF-8 char
          if ((c >> 5) != 6) failIllegalUtf8(i)
          c = c & 0x1F
        }
        c = (c << 6) | trailingByte(i)
      } else failIllegalUtf8(i)
      if (c < 0xD800 || 0xE000 <= c) {
        aux = if (c > 0xFFFF) { // surrogate pair required?
          val cc = appendChar(i, charCursor, ((c >> 10) + 0xD800 - (0x10000 >> 10)).toChar) // high surrogate
          appendChar(i, cc, (0xDC00 + (c & 0x3FF)).toChar) // low surrogate
        } else appendChar(i, charCursor, c.toChar)
        i + 1
      } else failIllegalCodepoint(i, c)
    }

    @tailrec def parseUtf8StringSlow(ix: Long, charCursor: Int): Long = {
      val c = getInputByte(ix).toChar
      if (c < ' ') failIllegalCharacter(ix, c.toInt)
      if (c == '"') {
        receiver.onChars(charCursor, chars)
        ix + 1
      } else if (c == '\\') {
        parseUtf8StringSlow(parseEscapeSeq(ix + 1, charCursor), aux)
      } else if (c > 127) {
        parseUtf8StringSlow(parseMultiByteUtf8Char(ix, charCursor), aux)
      } else {
        parseUtf8StringSlow(ix + 1, appendChar(ix, charCursor, c))
      }
    }

    @tailrec def parseUtf8String(ix: Long, charCursor: Int, endIx: Long): Long =
      if (ix < endIx) {
        // fetch 8 bytes (chars) at the same time with the first becoming the (left-most) MSB of the `octa` long
        val octa = ia.octaByteBigEndian(input, ix)

        // the following bit-level logic is heavily inspired by hackers delight 6-1 ("Find First 0-Byte")

        // mask '"' characters
        val quotes = octa ^ 0x2222222222222222L // bytes containing '"' become zero
        var qMask  = (quotes & ALL7F) + ALL7F
        qMask = ~(qMask | quotes | ALL7F) // '"' bytes become 0x80, all others 0x00

        // mask '\' characters
        val bSlashed = octa ^ 0x5c5c5c5c5c5c5c5cL // bytes containing '\' become zero
        var bMask    = (bSlashed & ALL7F) + ALL7F
        bMask = ~(bMask | bSlashed | ALL7F) // '\' bytes become 0x80, all others 0x00

        // mask 8-bit characters (> 127)
        val hMask = octa & ALL80 // // bytes containing 8-bit chars become 0x80, all others 0x00

        // mask ctrl characters (0 - 31)
        var cMask = (octa & ALL7F) + 0x6060606060606060L
        cMask = ~(cMask | octa | ALL7F) // ctrl chars become 0x80, all others 0x00

        // the number of leading zeros in the (shifted) or-ed masks uniquely identifies the first "special" char
        val code      = java.lang.Long.numberOfLeadingZeros(qMask | (bMask >>> 1) | (hMask >>> 2) | (cMask >>> 3))
        val charCount = code >> 3 // the number of "good" normal chars before a special char [0..8]

        // write all good chars to the char buffer
        var newCursor = charCursor
        if (charCount > 0) {
          newCursor += charCount
          ensureCharsLen(newCursor)
          CharArrayOut.instance.write(chars, charCursor, octa, charCount)
        }

        // branchless for: if (charCount <= 7) code & 3 else -1
        ((code & 3) | ((7 - charCount) >> 31): @switch) match {

          case -1 ⇒ // we have written eight good 7-bit chars, so we can recurse immediately
            parseUtf8String(ix + 8, newCursor, endIx)

          case 0 ⇒ // we have written `charCount` 7-bit chars before a '"', so we are done with this string
            receiver.onChars(newCursor, chars)
            ix + charCount + 1

          case 1 ⇒ // we have written `charCount` 7-bit chars before a '\', so handle the escape and recurse
            parseUtf8String(parseEscapeSeq(ix + charCount + 1, newCursor), aux, endIx)

          case 2 ⇒ // we have `charCount` 7-bit chars before an 8-bit, so utf8-decode and recurce
            parseUtf8String(parseMultiByteUtf8Char(ix + charCount, newCursor), aux, endIx)

          case 3 ⇒ // we have `charCount` good chars before a CTRL char, so fail
            failIllegalCharacter(ix + charCount, (octa >>> ((7 - charCount) << 3)).toByte.toInt)
        }
      } else parseUtf8StringSlow(ix, charCursor)

    @tailrec def skipWhiteSpaceSlow(ix: Long): Long =
      if (ix < inputLen && toToken(getInputByteUnsafe(ix) & 0xFFL) == WHITESPACE) {
        skipWhiteSpaceSlow(ix + 1)
      } else ix

    @tailrec def skipWhiteSpace(ix: Long, endIx: Long): Long =
      if (ix < endIx) {
        // fetch 8 bytes (chars) at the same time with the first becoming the (left-most) MSB of the `octa` long
        val octa = ia.octaByteBigEndian(input, ix)

        // bytes containing [0..32] or [128-160] become 0xff, all others 0x7f
        var mask = (octa & ALL7F) + 0x5f5f5f5f5f5f5f5fL

        // bytes containing [0..32] become zero, all others 0x80
        mask = (octa | mask | ALL7F) ^ ALL7F

        val wsCharCount = java.lang.Long.numberOfLeadingZeros(mask) >> 3
        if (wsCharCount < 8) ix + wsCharCount
        else skipWhiteSpace(ix + 8, endIx)
      } else skipWhiteSpaceSlow(ix)

    if (index < inputLen) {
      val ix = index + 1
      val c  = getInputByteUnsafe(index) & 0xFFL
      (toToken(c).toInt: @switch) match {
        case WHITESPACE  ⇒ pull(skipWhiteSpace(ix, inputLen - 8), receiver)
        case SEPARATOR   ⇒ pull(ix, receiver)
        case DQUOTE      ⇒ parseUtf8String(ix, 0, inputLen - 8)
        case MAP_START   ⇒ receiver.onMapStart(); ix
        case ARRAY_START ⇒ receiver.onArrayStart(); ix
        case BREAK       ⇒ receiver.onBreak(); ix
        case LOWER_N     ⇒ parseNull(index)
        case LOWER_F     ⇒ parseFalse(ix)
        case LOWER_T     ⇒ parseTrue(index)
        case MINUS       ⇒ parseNegNumber(ix)
        case DIGIT0      ⇒ parseNumber0(ix, negative = false)
        case DIGIT19     ⇒ parseNumber(ix, c - '0', appendChar(ix, 0, c.toChar), negative = false)
        case _           ⇒ failInvalidJsonSyntax(ix)
      }
    } else {
      receiver.onEndOfInput()
      index
    }
  }

  @inline private def getInputByte(ix: Long): Byte = ia.safeByte(input, ix)

  @inline private def getInputByteUnsafe(ix: Long): Byte = ia.unsafeByte(input, ix)

  @inline private def toToken(c: Long): Byte = InputAccess.ForByteArray.unsafeByte(TokenTable, c)

  private def appendChar(ix: Long, charCursor: Int, c: Char): Int = {
    val newCursor = charCursor + 1
    if (newCursor > 0) {
      ensureCharsLen(newCursor)
      CharArrayOut.instance.writeChar(chars, charCursor, c)
      newCursor
    } else failStringTooLong(ix)
  }

  private def append2(ix: Long, charCursor: Int, octa: Long): Int = {
    val newCursor = charCursor + 2
    if (newCursor > 0) {
      ensureCharsLen(newCursor)
      CharArrayOut.instance.write2(chars, charCursor, octa)
      newCursor
    } else failStringTooLong(ix)
  }

  private def append3(ix: Long, charCursor: Int, octa: Long): Int = {
    val newCursor = charCursor + 3
    if (newCursor > 0) {
      ensureCharsLen(newCursor)
      CharArrayOut.instance.write3(chars, charCursor, octa)
      newCursor
    } else failStringTooLong(ix)
  }

  private def ensureCharsLen(len: Int): Unit =
    if (len > chars.length) {
      chars = util.Arrays.copyOf(chars, math.max(chars.length << 1, len))
    }

  @inline private def getString(len: Int): String =
    if (len > 0) new String(chars, 0, len) else ""

  private def failInvalidJsonSyntax(ix: Long) =
    throw new Borer.Error.InvalidJsonData(pos(ix), "Invalid JSON syntax")
  private def failIllegalCodepoint(ix: Long, c: Int) =
    throw new Borer.Error.InvalidJsonData(pos(ix), s"Illegal Unicode Code point [${Integer.toHexString(c)}]")
  private def failIllegalCharacter(ix: Long, c: Int) =
    throw new Borer.Error.InvalidJsonData(pos(ix), s"Illegal character [${Integer.toHexString(c)}]")
  private def failStringTooLong(ix: Long) =
    throw new Borer.Error.Overflow(pos(ix), "JSON String too long")
  private def failUnexpectedEndOfInput(ix: Long) =
    throw new Borer.Error.UnexpectedEndOfInput(pos(ix))
  private def failIllegalUtf8(ix: Long) =
    throw new Borer.Error.InvalidJsonData(pos(ix), "Illegal UTF-8 character encoding")
}

private[borer] object JsonParser {
  private final val WHITESPACE  = 1
  private final val SEPARATOR   = 2
  private final val DQUOTE      = 3
  private final val MAP_START   = 4
  private final val ARRAY_START = 5
  private final val BREAK       = 6
  private final val LOWER_N     = 7
  private final val LOWER_F     = 8
  private final val LOWER_T     = 9
  private final val MINUS       = 10
  private final val DIGIT0      = 11
  private final val DIGIT19     = 12
  private final val PLUS        = 13
  private final val DOT         = 14
  private final val LETTER_E    = 15

  private final val ALL7F = 0x7f7f7f7f7f7f7f7fL
  private final val ALL80 = 0x8080808080808080L

  private final val TokenTable: Array[Byte] = {
    val array = new Array[Byte](256)
    array(' '.toInt) = WHITESPACE
    array('\t'.toInt) = WHITESPACE
    array('\n'.toInt) = WHITESPACE
    array('\r'.toInt) = WHITESPACE
    array(','.toInt) = SEPARATOR
    array(':'.toInt) = SEPARATOR
    array('"'.toInt) = DQUOTE
    array('{'.toInt) = MAP_START
    array('['.toInt) = ARRAY_START
    array('}'.toInt) = BREAK
    array(']'.toInt) = BREAK
    array('n'.toInt) = LOWER_N
    array('f'.toInt) = LOWER_F
    array('t'.toInt) = LOWER_T
    array('-'.toInt) = MINUS
    array('0'.toInt) = DIGIT0
    array('1'.toInt) = DIGIT19
    array('2'.toInt) = DIGIT19
    array('3'.toInt) = DIGIT19
    array('4'.toInt) = DIGIT19
    array('5'.toInt) = DIGIT19
    array('6'.toInt) = DIGIT19
    array('7'.toInt) = DIGIT19
    array('8'.toInt) = DIGIT19
    array('9'.toInt) = DIGIT19
    array('+'.toInt) = PLUS
    array('.'.toInt) = DOT
    array('E'.toInt) = LETTER_E
    array('e'.toInt) = LETTER_E
    array
  }

  // powers of 10 which can be represented exactly in a `Double`
  private val double10pow = Array(1.0e0, 1.0e1, 1.0e2, 1.0e3, 1.0e4, 1.0e5, 1.0e6, 1.0e7, 1.0e8, 1.0e9, 1.0e10, 1.0e11,
    1.0e12, 1.0e13, 1.0e14, 1.0e15, 1.0e16, 1.0e17, 1.0e18, 1.0e19, 1.0e20, 1.0e21, 1.0e22)
}
