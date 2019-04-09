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
private[borer] final class JsonParser extends Receiver.Parser {
  import Borer.Error

  private[this] var chars: Array[Char] = new Array[Char](32)
  private[this] var aux: Int           = _

  /**
    * Reads the next data item from the input and sends it to the given [[Receiver]].
    * The given [[Receiver]] receives exactly one call to one of its methods.
    * The returned `Long` is the index of the next byte to consume from the input
    * (and can be used for the subsequent call to this method).
    */
  @tailrec def pull[Input](input: Input, index: Long, receiver: Receiver)(implicit ia: InputAccess[Input]): Long = {
    import JsonParser._
    val len = ia.length(input)

    def pos(ix: Long) = Position(input, ix)
    def failInvalidJsonValue(ix: Long) =
      throw new Error.InvalidJsonData(pos(ix), "Invalid JSON value")
    def failIllegalCodepoint(ix: Long, c: Int) =
      throw new Error.InvalidJsonData(pos(ix), s"Illegal Unicode Code point [${Integer.toHexString(c)}]")
    def failIllegalCharacter(ix: Long, c: Int) =
      throw new Error.InvalidJsonData(pos(ix), s"Illegal character [${Integer.toHexString(c)}]")
    def failStringTooLong(ix: Long) =
      throw new Error.Overflow(pos(ix), "JSON String too long")
    def failUnexpectedEndOfInput(ix: Long) =
      throw new Error.UnexpectedEndOfInput(pos(ix))
    def failIllegalUtf8(ix: Long) =
      throw new Error.InvalidJsonData(pos(ix), "Illegal UTF-8 character encoding")

    def unsafeLookup(ix: Long) =
      InputAccess.ForByteArray.unsafeByte(LookupTable, ia.unsafeByte(input, ix) & 0xFFL) & 0xFF

    def ensureCharsLen(len: Int): Unit =
      if (len > chars.length) {
        chars = util.Arrays.copyOf(chars, math.max(chars.length << 1, len))
      }

    def appendChar(ix: Long, charCursor: Int, c: Char): Int = {
      val newCursor = charCursor + 1
      if (newCursor > 0) {
        ensureCharsLen(newCursor)
        CharArrayOut.instance.writeChar(chars, charCursor, c)
        newCursor
      } else failStringTooLong(ix)
    }

    def appendNonNegativeLong(ix: Long, charCursor: Int, value: Long): Int =
      if (value != 0) {
        def div10(i: Int) = {
          var q = (i << 3) + (i << 2)
          q += (q << 12) + (q << 8) + (q << 4) + i
          q >>>= 19
          q // 52429 * l / 524288 = l * 0.10000038146972656
        }
        def mul10(i: Int)   = (i << 3) + (i << 1)
        def mul100(l: Long) = (l << 6) + (l << 5) + (l << 2)

        // for small numbers we can use the "fast-path"
        def phase2(i: Int): Int = {
          val q = div10(i)
          val r = i - mul10(q)
          appendChar(ix, if (q != 0) phase2(q) else charCursor, ('0' + r).toChar)
        }

        // for large numbers we bite the bullet of performing one division every two digits
        def phase1(l: Long): Int =
          if (l > 65535L) {
            val q  = l / 100
            val r  = (l - mul100(q)).toInt
            val rq = div10(r)
            append2(ix, phase1(q), (('0' + rq).toLong << 56) | (('0' + r - mul10(rq)).toLong << 48))
          } else phase2(l.toInt)
        phase1(value)
      } else appendChar(ix, charCursor, '0')

    @inline def getString(charCursor: Int): String = if (charCursor > 0) new String(chars, 0, charCursor) else ""

    @inline def parseNull(ix: Long): Long =
      if (ix < len - 3) {
        if (ia.quadByteBigEndian(input, ix) == 0x6e756c6c) { // "null"
          receiver.onNull()
          ix + 4
        } else failInvalidJsonValue(ix)
      } else failUnexpectedEndOfInput(ix)

    @inline def parseFalse(ix: Long): Long =
      if (ix < len - 3) {
        if (ia.quadByteBigEndian(input, ix) == 0x616c7365) { // "alse"
          receiver.onBool(value = false)
          ix + 4
        } else failInvalidJsonValue(ix)
      } else failUnexpectedEndOfInput(ix)

    @inline def parseTrue(ix: Long): Long =
      if (ix < len - 3) {
        if (ia.quadByteBigEndian(input, ix) == 0x74727565) { // "true"
          receiver.onBool(value = true)
          ix + 4
        } else failInvalidJsonValue(ix)
      } else failUnexpectedEndOfInput(ix)

    @tailrec def parseNumberString(ix: Long, charCursor: Int): Long = {
      def result(): Long = {
        receiver.onNumberString(getString(charCursor))
        ix
      }
      if (ix < len) {
        val c = ia.unsafeByte(input, ix) & 0xFFL
        if ((InputAccess.ForByteArray.unsafeByte(LookupTable, c) & 0xF0) != 0) {
          parseNumberString(ix + 1, appendChar(ix, charCursor, c.toChar))
        } else result()
      } else result()
    }

    @tailrec def parseDecimalNumber(ix: Long, significand: Long, scale: Int, negative: Boolean): Long = {
      @inline def result(): Long =
        if (scale > 0) {
          var double = significand.toDouble / double10pow(scale)
          if (negative) double = -double
          val float = double.toFloat
          if (float.toDouble == double) receiver.onFloat(float)
          else receiver.onDouble(double)
          ix
        } else failInvalidJsonValue(ix)
      def breakOutToNumberString(c: Char) = {
        val charCursor =
          if (significand < 0) appendNonNegativeLong(ix, appendChar(ix, 0, '-'), -significand)
          else appendNonNegativeLong(ix, 0, significand)
        val newCharCursor =
          if (scale > 0) {
            val newCursor = charCursor + 1
            if (newCursor > 0) {
              ensureCharsLen(newCursor)
              System.arraycopy(chars, charCursor - scale, chars, newCursor - scale, scale)
              chars(charCursor - scale) = '.'
              newCursor
            } else failStringTooLong(ix)
          } else charCursor
        parseNumberString(ix + 1, appendChar(ix, newCharCursor, c))
      }
      if (ix < len) {
        val c = ia.unsafeByte(input, ix).toInt
        if ('0' <= c && c <= '9') {
          val newScale = scale + 1
          // check whether we can safely multiply by 10 and add 9 and still have significand fit wholy into a Double
          if (significand < ((1L << 52) / 10 - 1) && newScale < double10pow.length) {
            parseDecimalNumber(ix + 1, (significand << 3) + (significand << 1) + c - '0', newScale, negative)
          } else breakOutToNumberString(c.toChar)
        } else if ((c | 0x20) == 'e') breakOutToNumberString(c.toChar)
        else result()
      } else result()
    }

    @tailrec def parseNumber(ix: Long, value: Long, negative: Boolean): Long = {
      @inline def result(): Long = {
        val long = if (negative) -value else value
        if (Util.isInt(long)) receiver.onInt(long.toInt) else receiver.onLong(long)
        ix
      }
      def breakOutToNumberString() =
        parseNumberString(ix, appendNonNegativeLong(ix, if (negative) appendChar(ix, 0, '-') else 0, value))
      if (ix < len) {
        if (value <= Long.MaxValue / 10) {
          val i     = ix + 1
          val mul10 = (value << 3) + (value << 1)
          (unsafeLookup(ix) >> 4: @switch) match {
            case NUM_DIGIT0 ⇒ parseNumber(i, mul10, negative)
            case NUM_DIGIT1 ⇒ parseNumber(i, mul10 + 1, negative)
            case NUM_DIGIT2 ⇒ parseNumber(i, mul10 + 2, negative)
            case NUM_DIGIT3 ⇒ parseNumber(i, mul10 + 3, negative)
            case NUM_DIGIT4 ⇒ parseNumber(i, mul10 + 4, negative)
            case NUM_DIGIT5 ⇒ parseNumber(i, mul10 + 5, negative)
            case NUM_DIGIT6 ⇒ parseNumber(i, mul10 + 6, negative)
            case NUM_DIGIT7 ⇒ parseNumber(i, mul10 + 7, negative)
            case NUM_DIGIT8 ⇒
              if (value == Long.MaxValue / 10) {
                if (negative && (i >= len || unsafeLookup(i) < DIGIT0)) { // special case: Long.MinValue
                  receiver.onLong(Long.MinValue)
                  i
                } else breakOutToNumberString()
              } else parseNumber(i, mul10 + 8, negative)
            case NUM_DIGIT9 ⇒
              if (value == Long.MaxValue / 10) breakOutToNumberString()
              else parseNumber(i, mul10 + 9, negative)
            case NUM_DOT      ⇒ parseDecimalNumber(i, value, 0, negative)
            case NUM_LETTER_E ⇒ breakOutToNumberString()
            case _            ⇒ result()
          }
        } else breakOutToNumberString()
      } else result()
    }

    @inline def parseNumber0(ix: Long): Long = {
      @inline def result(): Long = {
        receiver.onInt(0)
        ix
      }
      if (ix < len) {
        val c = ia.unsafeByte(input, ix).toInt
        if (c == '.') parseDecimalNumber(ix + 1, 0, 0, negative = false)
        else if ((c | 0x20) == 'e') parseNumberString(ix + 1, appendChar(ix, appendChar(ix, 0, '0'), 'e'))
        else result()
      } else result()
    }

    def parseEscapeSeq(ix: Long, charCursor: Int): Long = {
      var i = ix + 1
      val c =
        ia.safeByte(input, ix).toChar match {
          case x @ ('"' | '/' | '\\') ⇒ x
          case 'b'                    ⇒ '\b'
          case 'f'                    ⇒ '\f'
          case 'n'                    ⇒ '\n'
          case 'r'                    ⇒ '\r'
          case 't'                    ⇒ '\t'
          case 'u' ⇒
            if (i < len - 3) {
              val c = (Util.hexValue(ia.unsafeByte(input, i).toChar) << 12) |
                (Util.hexValue(ia.unsafeByte(input, i + 1).toChar) << 8) |
                (Util.hexValue(ia.unsafeByte(input, i + 2).toChar) << 4) |
                Util.hexValue(ia.unsafeByte(input, i + 3).toChar)
              if (0xD800 <= c && c < 0xE000) failIllegalCodepoint(ix, c)
              i += 4
              c.toChar
            } else failUnexpectedEndOfInput(ix)
          case _ ⇒ throw new Error.InvalidJsonData(pos(ix), "Illegal JSON escape sequence")
        }
      aux = appendChar(ix, charCursor, c)
      i
    }

    def parseMultiByteUtf8Char(ix: Long, charCursor: Int): Long = {
      @inline def trailingByte(i: Long): Int = {
        val x = ia.safeByte(input, i) & 0xFF
        if ((x >> 6) != 2) failIllegalUtf8(i)
        x & 0x3F
      }
      var c = ia.safeByte(input, ix) & 0xFF
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
          appendChar(i, charCursor, ((c >> 10) + 0xD800 - (0x10000 >> 10)).toChar) // high surrogate
          appendChar(i, charCursor + 1, (0xDC00 + (c & 0x3FF)).toChar)             // low surrogate
        } else appendChar(i, charCursor, c.toChar)
        i + 1
      } else failIllegalCodepoint(i, c)
    }

    @tailrec def parseUtf8StringSlow(ix: Long, charCursor: Int): Long = {
      val c = ia.safeByte(input, ix).toChar
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

    def append1(ix: Long, charCursor: Int, octa: Long): Int =
      appendChar(ix, charCursor, (octa >>> 56).toChar)

    def append2(ix: Long, charCursor: Int, octa: Long): Int = {
      val newCursor = charCursor + 2
      if (newCursor > 0) {
        ensureCharsLen(newCursor)
        CharArrayOut.instance.write2(chars, charCursor, octa)
        newCursor
      } else failStringTooLong(ix)
    }

    def append3(ix: Long, charCursor: Int, octa: Long): Int = {
      val newCursor = charCursor + 3
      if (newCursor > 0) {
        ensureCharsLen(newCursor)
        CharArrayOut.instance.write3(chars, charCursor, octa)
        newCursor
      } else failStringTooLong(ix)
    }

    def append4(ix: Long, charCursor: Int, octa: Long): Int = {
      val newCursor = charCursor + 4
      if (newCursor > 0) {
        ensureCharsLen(newCursor)
        CharArrayOut.instance.write4(chars, charCursor, octa)
        newCursor
      } else failStringTooLong(ix)
    }

    def append5(ix: Long, charCursor: Int, octa: Long): Int = {
      val newCursor = charCursor + 5
      if (newCursor > 0) {
        ensureCharsLen(newCursor)
        CharArrayOut.instance.write5(chars, charCursor, octa)
        newCursor
      } else failStringTooLong(ix)
    }

    def append6(ix: Long, charCursor: Int, octa: Long): Int = {
      val newCursor = charCursor + 6
      if (newCursor > 0) {
        ensureCharsLen(newCursor)
        CharArrayOut.instance.write6(chars, charCursor, octa)
        newCursor
      } else failStringTooLong(ix)
    }

    def append7(ix: Long, charCursor: Int, octa: Long): Int = {
      val newCursor = charCursor + 7
      if (newCursor > 0) {
        ensureCharsLen(newCursor)
        CharArrayOut.instance.write7(chars, charCursor, octa)
        newCursor
      } else failStringTooLong(ix)
    }

    def append8(ix: Long, charCursor: Int, octa: Long): Int = {
      val newCursor = charCursor + 8
      if (newCursor > 0) {
        ensureCharsLen(newCursor)
        CharArrayOut.instance.write8(chars, charCursor, octa)
        newCursor
      } else failStringTooLong(ix)
    }

    @tailrec def parseUtf8String(ix: Long, charCursor: Int): Long =
      if (ix < len - 8) {
        // fetch 8 bytes (chars) at the same time with the first becoming the (left-most) MSB of the `octa` long
        val octa = ia.octaByteBigEndian(input, ix)

        // the following bit-level logic is heavily inspired by hackers delight 6-1 ("Find First 0-Byte")

        // mask '"' characters
        val quotes = octa ^ 0x2222222222222222L // bytes containing '"' become zero
        var qMask  = (quotes & 0x7f7f7f7f7f7f7f7fL) + 0x7f7f7f7f7f7f7f7fL
        qMask = ~(qMask | quotes | 0x7f7f7f7f7f7f7f7fL) // '"' bytes become 0x80, all others 0x00

        // mask '\' characters
        val bSlashed = octa ^ 0x5c5c5c5c5c5c5c5cL // bytes containing '\' become zero
        var bMask    = (bSlashed & 0x7f7f7f7f7f7f7f7fL) + 0x7f7f7f7f7f7f7f7fL
        bMask = ~(bMask | bSlashed | 0x7f7f7f7f7f7f7f7fL) // '\' bytes become 0x80, all others 0x00

        // mask 8-bit characters (> 127)
        val hMask = octa & 0x8080808080808080L // // bytes containing 8-bit chars become 0x80, all others 0x00

        // mask ctrl characters (0 - 31)
        var cMask = (octa & 0x7f7f7f7f7f7f7f7fL) + 0x6060606060606060L
        cMask = ~(cMask | octa | 0x7f7f7f7f7f7f7f7fL) // ctrl chars become 0x80, all others 0x00

        // the number of leading zeros in the (shifted) or-ed masks uniquely identifies the first "special" char
        val code = java.lang.Long.numberOfLeadingZeros(qMask | (bMask >>> 1) | (hMask >>> 2) | (cMask >>> 3))

        // convert to contiguous number range for compact table-switch and optimized jump locality
        (((code & 3) << 3) | (7 - (code >> 3)): @switch) match {
          case -1 ⇒ // 8 normal chars
            parseUtf8String(ix + 8, append8(ix, charCursor, octa))

          case 0 ⇒ // char #7 is '"'
            receiver.onChars(append7(ix, charCursor, octa), chars); ix + 8
          case 1 ⇒ // char #6 is '"'
            receiver.onChars(append6(ix, charCursor, octa), chars); ix + 7
          case 2 ⇒ // char #5 is '"'
            receiver.onChars(append5(ix, charCursor, octa), chars); ix + 6
          case 3 ⇒ // char #4 is '"'
            receiver.onChars(append4(ix, charCursor, octa), chars); ix + 5
          case 4 ⇒ // char #3 is '"'
            receiver.onChars(append3(ix, charCursor, octa), chars); ix + 4
          case 5 ⇒ // char #2 is '"'
            receiver.onChars(append2(ix, charCursor, octa), chars); ix + 3
          case 6 ⇒ // char #1 is '"'
            receiver.onChars(append1(ix, charCursor, octa), chars); ix + 2
          case 7 ⇒ // char #0 is '"'
            receiver.onChars(charCursor, chars); ix + 1

          case 8 ⇒ // char #7 is '\'
            parseUtf8String(parseEscapeSeq(ix + 8, append7(ix, charCursor, octa)), aux)
          case 9 ⇒ // char #6 is '\'
            parseUtf8String(parseEscapeSeq(ix + 7, append6(ix, charCursor, octa)), aux)
          case 10 ⇒ // char #5 is '\'
            parseUtf8String(parseEscapeSeq(ix + 6, append5(ix, charCursor, octa)), aux)
          case 11 ⇒ // char #4 is '\'
            parseUtf8String(parseEscapeSeq(ix + 5, append4(ix, charCursor, octa)), aux)
          case 12 ⇒ // char #3 is '\'
            parseUtf8String(parseEscapeSeq(ix + 4, append3(ix, charCursor, octa)), aux)
          case 13 ⇒ // char #2 is '\'
            parseUtf8String(parseEscapeSeq(ix + 3, append2(ix, charCursor, octa)), aux)
          case 14 ⇒ // char #1 is '\'
            parseUtf8String(parseEscapeSeq(ix + 2, append1(ix, charCursor, octa)), aux)
          case 15 ⇒ // char #0 is '\'
            parseUtf8String(parseEscapeSeq(ix + 1, charCursor), aux)

          case 16 ⇒ // char #7 is 8-bit
            parseUtf8String(parseMultiByteUtf8Char(ix + 7, append7(ix, charCursor, octa)), aux)
          case 17 ⇒ // char #6 is 8-bit
            parseUtf8String(parseMultiByteUtf8Char(ix + 6, append6(ix, charCursor, octa)), aux)
          case 18 ⇒ // char #5 is 8-bit
            parseUtf8String(parseMultiByteUtf8Char(ix + 5, append5(ix, charCursor, octa)), aux)
          case 19 ⇒ // char #4 is 8-bit
            parseUtf8String(parseMultiByteUtf8Char(ix + 4, append4(ix, charCursor, octa)), aux)
          case 20 ⇒ // char #3 is 8-bit
            parseUtf8String(parseMultiByteUtf8Char(ix + 3, append3(ix, charCursor, octa)), aux)
          case 21 ⇒ // char #2 is 8-bit
            parseUtf8String(parseMultiByteUtf8Char(ix + 2, append2(ix, charCursor, octa)), aux)
          case 22 ⇒ // char #1 is 8-bit
            parseUtf8String(parseMultiByteUtf8Char(ix + 1, append1(ix, charCursor, octa)), aux)
          case 23 ⇒ // char #0 is 8-bit
            parseUtf8String(parseMultiByteUtf8Char(ix, charCursor), aux)

          case 24 ⇒ // char #7 is CTRL char
            failIllegalCharacter(ix + 7, octa.toByte.toInt)
          case 25 ⇒ // char #6 is CTRL char
            failIllegalCharacter(ix + 6, (octa >>> 8).toByte.toInt)
          case 26 ⇒ // char #5 is CTRL char
            failIllegalCharacter(ix + 5, (octa >>> 16).toByte.toInt)
          case 27 ⇒ // char #4 is CTRL char
            failIllegalCharacter(ix + 4, (octa >>> 24).toByte.toInt)
          case 28 ⇒ // char #3 is CTRL char
            failIllegalCharacter(ix + 3, (octa >>> 32).toByte.toInt)
          case 29 ⇒ // char #2 is CTRL char
            failIllegalCharacter(ix + 2, (octa >>> 40).toByte.toInt)
          case 30 ⇒ // char #1 is CTRL char
            failIllegalCharacter(ix + 1, (octa >>> 48).toByte.toInt)
          case 31 ⇒ // char #0 is CTRL char
            failIllegalCharacter(ix, (octa >>> 56).toInt)
        }
      } else parseUtf8StringSlow(ix, charCursor)

    if (index < len) {
      val ix = index + 1
      val c  = ia.unsafeByte(input, index) & 0xFFL
      (InputAccess.ForByteArray.unsafeByte(LookupTable, c) & 0xF: @switch) match {
        case WHITESPACE ⇒ pull(input, ix, receiver)
        case DQUOTE     ⇒ parseUtf8String(ix, 0)
        case MAP_START  ⇒ receiver.onMapStart(); ix
        case ARR_START  ⇒ receiver.onArrayStart(); ix
        case BREAK      ⇒ receiver.onBreak(); ix
        case LOWER_N    ⇒ parseNull(index)
        case LOWER_F    ⇒ parseFalse(ix)
        case LOWER_T    ⇒ parseTrue(index)
        case MINUS      ⇒ parseNumber(ix, 0, negative = true)
        case DIGIT0     ⇒ parseNumber0(ix)
        case DIGIT19    ⇒ parseNumber(ix, c - '0', negative = false)
        case _          ⇒ failInvalidJsonValue(ix)
      }
    } else {
      receiver.onEndOfInput()
      index
    }
  }
}

private[borer] object JsonParser {
  private final val WHITESPACE = 1
  private final val DQUOTE     = 2
  private final val MAP_START  = 3
  private final val ARR_START  = 4
  private final val BREAK      = 5
  private final val LOWER_N    = 6
  private final val LOWER_F    = 7
  private final val LOWER_T    = 8
  private final val MINUS      = 9
  private final val DIGIT0     = 10
  private final val DIGIT19    = 11

  private final val NUM_MINUS    = 1
  private final val NUM_PLUS     = 2
  private final val NUM_DIGIT0   = 3
  private final val NUM_DIGIT1   = 4
  private final val NUM_DIGIT2   = 5
  private final val NUM_DIGIT3   = 6
  private final val NUM_DIGIT4   = 7
  private final val NUM_DIGIT5   = 8
  private final val NUM_DIGIT6   = 9
  private final val NUM_DIGIT7   = 10
  private final val NUM_DIGIT8   = 11
  private final val NUM_DIGIT9   = 12
  private final val NUM_DOT      = 13
  private final val NUM_LETTER_E = 14

  private final val LookupTable: Array[Byte] = {
    val array = new Array[Byte](256)
    array(' '.toInt) = WHITESPACE
    array('\t'.toInt) = WHITESPACE
    array('\n'.toInt) = WHITESPACE
    array('\r'.toInt) = WHITESPACE
    array(','.toInt) = WHITESPACE
    array(':'.toInt) = WHITESPACE
    array('"'.toInt) = DQUOTE
    array('{'.toInt) = MAP_START
    array('['.toInt) = ARR_START
    array('}'.toInt) = BREAK
    array(']'.toInt) = BREAK
    array('n'.toInt) = LOWER_N
    array('f'.toInt) = LOWER_F
    array('t'.toInt) = LOWER_T
    array('-'.toInt) = MINUS | (NUM_MINUS << 4)
    array('+'.toInt) = NUM_PLUS << 4
    array('0'.toInt) = DIGIT0 | (NUM_DIGIT0 << 4)
    array('1'.toInt) = DIGIT19 | NUM_DIGIT1 << 4
    array('2'.toInt) = DIGIT19 | NUM_DIGIT2 << 4
    array('3'.toInt) = DIGIT19 | NUM_DIGIT3 << 4
    array('4'.toInt) = DIGIT19 | NUM_DIGIT4 << 4
    array('5'.toInt) = (DIGIT19 | NUM_DIGIT5 << 4).toByte
    array('6'.toInt) = (DIGIT19 | NUM_DIGIT6 << 4).toByte
    array('7'.toInt) = (DIGIT19 | NUM_DIGIT7 << 4).toByte
    array('8'.toInt) = (DIGIT19 | NUM_DIGIT8 << 4).toByte
    array('9'.toInt) = (DIGIT19 | NUM_DIGIT9 << 4).toByte
    array('.'.toInt) = (NUM_DOT << 4).toByte
    array('E'.toInt) = (NUM_LETTER_E << 4).toByte
    array('e'.toInt) = (NUM_LETTER_E << 4).toByte
    array
  }

  // powers of 10 which can be represented exactly in a `Double`
  private val double10pow = Array(1.0e0, 1.0e1, 1.0e2, 1.0e3, 1.0e4, 1.0e5, 1.0e6, 1.0e7, 1.0e8, 1.0e9, 1.0e10, 1.0e11,
    1.0e12, 1.0e13, 1.0e14, 1.0e15, 1.0e16, 1.0e17, 1.0e18, 1.0e19, 1.0e20, 1.0e21, 1.0e22)
}
