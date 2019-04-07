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

    def ensureCharsLen(len: Int): Unit =
      if (len > chars.length) {
        chars = util.Arrays.copyOf(chars, math.max(chars.length << 1, len))
      }

    def appendChar(ix: Long, strLen: Int, c: Char): Int = {
      val newLen = strLen + 1
      if (newLen > 0) {
        ensureCharsLen(newLen)
        chars(strLen) = c
        newLen
      } else failStringTooLong(ix)
    }

    @inline def getString(strLen: Int): String = if (strLen > 0) new String(chars, 0, strLen) else ""

    @inline def parseNull(ix: Long): Long =
      if (ix < ia.length(input) - 3) {
        if (ia.quadByteBigEndian(input, ix) == 0x6e756c6c) { // "null"
          receiver.onNull()
          ix + 4
        } else failInvalidJsonValue(ix)
      } else failUnexpectedEndOfInput(ix)

    @inline def parseFalse(ix: Long): Long =
      if (ix < ia.length(input) - 3) {
        if (ia.quadByteBigEndian(input, ix) == 0x616c7365) { // "alse"
          receiver.onBool(value = false)
          ix + 4
        } else failInvalidJsonValue(ix)
      } else failUnexpectedEndOfInput(ix)

    @inline def parseTrue(ix: Long): Long =
      if (ix < ia.length(input) - 3) {
        if (ia.quadByteBigEndian(input, ix) == 0x74727565) { // "true"
          receiver.onBool(value = true)
          ix + 4
        } else failInvalidJsonValue(ix)
      } else failUnexpectedEndOfInput(ix)

    @tailrec def parseNonLongNumber(ix: Long, strLen: Int): Long = {
      def result(): Long = {
        receiver.onNumberString(getString(strLen))
        ix
      }
      if (ix < ia.length(input)) {
        val c = ia.unsafeByte(input, ix).toChar
        (c: @switch) match {
          case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '.' | '+' | '-' | 'e' | 'E' ⇒
            parseNonLongNumber(ix + 1, appendChar(ix, strLen, c))
          case _ ⇒ result()
        }
      } else result()
    }

    @tailrec def parseNumber(ix: Long, strLen: Int, value: Long, negative: Boolean): Long = {
      @inline def result(): Long = {
        val long = if (negative) -value else value
        if (Util.isInt(long)) receiver.onInt(long.toInt) else receiver.onLong(long)
        ix
      }
      if (ix < ia.length(input)) {
        val c                         = ia.unsafeByte(input, ix).toChar
        def breakOutToNonLongNumber() = parseNonLongNumber(ix + 1, appendChar(ix, strLen, c))
        (c: @switch) match {
          case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ⇒
            // check whether by adding this digit we overflow the Long range or not
            if (value < Long.MaxValue / 10 || value == Long.MaxValue / 10 && c != '8' && c != '9') {
              val newValue = (value << 3) + (value << 1) + c - '0' // same as multiplication by 10
              parseNumber(ix + 1, appendChar(ix, strLen, c), newValue, negative)
            } else if (negative && value == Long.MaxValue / 10 && c == '8' && // special case: Long.MinValue
                       !(ix < (ia.length(input) - 1) && "0123456789.eE".contains(ia.unsafeByte(input, ix + 1).toChar))) {
              receiver.onLong(Long.MinValue)
              ix + 1
            } else breakOutToNonLongNumber()
          case '.' | 'e' | 'E' ⇒ breakOutToNonLongNumber()
          case _               ⇒ result()
        }
      } else result()
    }

    @inline def parseNumber0(ix: Long): Long = {
      @inline def result(): Long = {
        receiver.onInt(0)
        ix
      }
      if (ix < ia.length(input)) {
        val c = ia.unsafeByte(input, ix).toChar
        c match {
          case '.' | 'e' | 'E' ⇒ parseNonLongNumber(ix + 1, appendChar(ix, appendChar(ix, 0, '0'), c))
          case _               ⇒ result()
        }
      } else result()
    }

    def parseEscapeSeq(ix: Long, strLen: Int): Long = {
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
            if (i < ia.length(input) - 3) {
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
      aux = appendChar(ix, strLen, c)
      i
    }

    def parseMultiByteUtf8Char(ix: Long, strLen: Int): Long = {
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
          appendChar(i, strLen, ((c >> 10) + 0xD800 - (0x10000 >> 10)).toChar) // high surrogate
          appendChar(i, strLen + 1, (0xDC00 + (c & 0x3FF)).toChar)             // low surrogate
        } else appendChar(i, strLen, c.toChar)
        i + 1
      } else failIllegalCodepoint(i, c)
    }

    @tailrec def parseUtf8StringSlow(ix: Long, strLen: Int): Long = {
      val c = ia.safeByte(input, ix).toChar
      if (c < ' ') failIllegalCharacter(ix, c.toInt)
      if (c == '"') {
        receiver.onChars(strLen, chars)
        ix + 1
      } else if (c == '\\') {
        parseUtf8StringSlow(parseEscapeSeq(ix + 1, strLen), aux)
      } else if (c > 127) {
        parseUtf8StringSlow(parseMultiByteUtf8Char(ix, strLen), aux)
      } else {
        parseUtf8StringSlow(ix + 1, appendChar(ix, strLen, c))
      }
    }

    def append1(ix: Long, strLen: Int, octa: Long): Int =
      appendChar(ix, strLen, (octa >>> 56).toChar)

    def append2(ix: Long, strLen: Int, octa: Long): Int = {
      val newLen = strLen + 2
      if (newLen > 0) {
        ensureCharsLen(newLen)
        chars(strLen) = (octa >>> 56).toChar
        chars(strLen + 1) = ((octa >>> 48) & 0xFFL).toChar
        newLen
      } else throw new Error.Overflow(pos(ix), "JSON String too long")
    }

    def append3(ix: Long, strLen: Int, octa: Long): Int = {
      val newLen = strLen + 3
      if (newLen > 0) {
        ensureCharsLen(newLen)
        chars(strLen) = (octa >>> 56).toChar
        chars(strLen + 1) = ((octa >>> 48) & 0xFFL).toChar
        chars(strLen + 2) = ((octa >>> 40) & 0xFFL).toChar
        newLen
      } else throw new Error.Overflow(pos(ix), "JSON String too long")
    }

    def append4(ix: Long, strLen: Int, octa: Long): Int = {
      val newLen = strLen + 4
      if (newLen > 0) {
        ensureCharsLen(newLen)
        chars(strLen) = (octa >>> 56).toChar
        chars(strLen + 1) = ((octa >>> 48) & 0xFFL).toChar
        chars(strLen + 2) = ((octa >>> 40) & 0xFFL).toChar
        chars(strLen + 3) = ((octa >>> 32) & 0xFFL).toChar
        newLen
      } else throw new Error.Overflow(pos(ix), "JSON String too long")
    }

    def append5(ix: Long, strLen: Int, octa: Long): Int = {
      val newLen = strLen + 5
      if (newLen > 0) {
        ensureCharsLen(newLen)
        chars(strLen) = (octa >>> 56).toChar
        chars(strLen + 1) = ((octa >>> 48) & 0xFFL).toChar
        chars(strLen + 2) = ((octa >>> 40) & 0xFFL).toChar
        chars(strLen + 3) = ((octa >>> 32) & 0xFFL).toChar
        chars(strLen + 4) = ((octa >>> 24) & 0xFFL).toChar
        newLen
      } else throw new Error.Overflow(pos(ix), "JSON String too long")
    }

    def append6(ix: Long, strLen: Int, octa: Long): Int = {
      val newLen = strLen + 6
      if (newLen > 0) {
        ensureCharsLen(newLen)
        chars(strLen) = (octa >>> 56).toChar
        chars(strLen + 1) = ((octa >>> 48) & 0xFFL).toChar
        chars(strLen + 2) = ((octa >>> 40) & 0xFFL).toChar
        chars(strLen + 3) = ((octa >>> 32) & 0xFFL).toChar
        chars(strLen + 4) = ((octa >>> 24) & 0xFFL).toChar
        chars(strLen + 5) = ((octa >>> 16) & 0xFFL).toChar
        newLen
      } else throw new Error.Overflow(pos(ix), "JSON String too long")
    }

    def append7(ix: Long, strLen: Int, octa: Long): Int = {
      val newLen = strLen + 7
      if (newLen > 0) {
        ensureCharsLen(newLen)
        chars(strLen) = (octa >>> 56).toChar
        chars(strLen + 1) = ((octa >>> 48) & 0xFFL).toChar
        chars(strLen + 2) = ((octa >>> 40) & 0xFFL).toChar
        chars(strLen + 3) = ((octa >>> 32) & 0xFFL).toChar
        chars(strLen + 4) = ((octa >>> 24) & 0xFFL).toChar
        chars(strLen + 5) = ((octa >>> 16) & 0xFFL).toChar
        chars(strLen + 6) = ((octa >>> 8) & 0xFFL).toChar
        newLen
      } else throw new Error.Overflow(pos(ix), "JSON String too long")
    }

    def append8(ix: Long, strLen: Int, octa: Long): Int = {
      val newLen = strLen + 8
      if (newLen > 0) {
        ensureCharsLen(newLen)
        chars(strLen) = (octa >>> 56).toChar
        chars(strLen + 1) = ((octa >>> 48) & 0xFFL).toChar
        chars(strLen + 2) = ((octa >>> 40) & 0xFFL).toChar
        chars(strLen + 3) = ((octa >>> 32) & 0xFFL).toChar
        chars(strLen + 4) = ((octa >>> 24) & 0xFFL).toChar
        chars(strLen + 5) = ((octa >>> 16) & 0xFFL).toChar
        chars(strLen + 6) = ((octa >>> 8) & 0xFFL).toChar
        chars(strLen + 7) = (octa & 0xFFL).toChar
        newLen
      } else throw new Error.Overflow(pos(ix), "JSON String too long")
    }

    @tailrec def parseUtf8String(ix: Long, strLen: Int): Long =
      if (ix < ia.length(input) - 8) {
        // fetch 8 chars at the same time from the input, the first becoming the (left-most) MSB of the `octa` long
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

        // convert to contiguous number range for optimized jump locality
        (((code & 3) << 3) | (7 - (code >> 3)): @switch) match {
          case -1 ⇒ // 8 normal chars
            parseUtf8String(ix + 8, append8(ix, strLen, octa))

          case 0 ⇒ // char #7 is '"'
            receiver.onChars(append7(ix, strLen, octa), chars); ix + 8
          case 1 ⇒ // char #6 is '"'
            receiver.onChars(append6(ix, strLen, octa), chars); ix + 7
          case 2 ⇒ // char #5 is '"'
            receiver.onChars(append5(ix, strLen, octa), chars); ix + 6
          case 3 ⇒ // char #4 is '"'
            receiver.onChars(append4(ix, strLen, octa), chars); ix + 5
          case 4 ⇒ // char #3 is '"'
            receiver.onChars(append3(ix, strLen, octa), chars); ix + 4
          case 5 ⇒ // char #2 is '"'
            receiver.onChars(append2(ix, strLen, octa), chars); ix + 3
          case 6 ⇒ // char #1 is '"'
            receiver.onChars(append1(ix, strLen, octa), chars); ix + 2
          case 7 ⇒ // char #0 is '"'
            receiver.onChars(strLen, chars); ix + 1

          case 8 ⇒ // char #7 is '\'
            parseUtf8String(parseEscapeSeq(ix + 8, append7(ix, strLen, octa)), aux)
          case 9 ⇒ // char #6 is '\'
            parseUtf8String(parseEscapeSeq(ix + 7, append6(ix, strLen, octa)), aux)
          case 10 ⇒ // char #5 is '\'
            parseUtf8String(parseEscapeSeq(ix + 6, append5(ix, strLen, octa)), aux)
          case 11 ⇒ // char #4 is '\'
            parseUtf8String(parseEscapeSeq(ix + 5, append4(ix, strLen, octa)), aux)
          case 12 ⇒ // char #3 is '\'
            parseUtf8String(parseEscapeSeq(ix + 4, append3(ix, strLen, octa)), aux)
          case 13 ⇒ // char #2 is '\'
            parseUtf8String(parseEscapeSeq(ix + 3, append2(ix, strLen, octa)), aux)
          case 14 ⇒ // char #1 is '\'
            parseUtf8String(parseEscapeSeq(ix + 2, append1(ix, strLen, octa)), aux)
          case 15 ⇒ // char #0 is '\'
            parseUtf8String(parseEscapeSeq(ix + 1, strLen), aux)

          case 16 ⇒ // char #7 is 8-bit
            parseUtf8String(parseMultiByteUtf8Char(ix + 7, append7(ix, strLen, octa)), aux)
          case 17 ⇒ // char #6 is 8-bit
            parseUtf8String(parseMultiByteUtf8Char(ix + 6, append6(ix, strLen, octa)), aux)
          case 18 ⇒ // char #5 is 8-bit
            parseUtf8String(parseMultiByteUtf8Char(ix + 5, append5(ix, strLen, octa)), aux)
          case 19 ⇒ // char #4 is 8-bit
            parseUtf8String(parseMultiByteUtf8Char(ix + 4, append4(ix, strLen, octa)), aux)
          case 20 ⇒ // char #3 is 8-bit
            parseUtf8String(parseMultiByteUtf8Char(ix + 3, append3(ix, strLen, octa)), aux)
          case 21 ⇒ // char #2 is 8-bit
            parseUtf8String(parseMultiByteUtf8Char(ix + 2, append2(ix, strLen, octa)), aux)
          case 22 ⇒ // char #1 is 8-bit
            parseUtf8String(parseMultiByteUtf8Char(ix + 1, append1(ix, strLen, octa)), aux)
          case 23 ⇒ // char #0 is 8-bit
            parseUtf8String(parseMultiByteUtf8Char(ix, strLen), aux)

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
      } else parseUtf8StringSlow(ix, strLen)

    if (index < ia.length(input)) {
      val ix = index + 1
      (ia.unsafeByte(input, index): @switch) match {
        case ' ' | '\t' | '\n' | '\r' | ',' | ':' ⇒ pull(input, ix, receiver)
        case '"'                                  ⇒ parseUtf8String(ix, 0)
        case '{'                                  ⇒ receiver.onMapStart(); ix
        case '['                                  ⇒ receiver.onArrayStart(); ix
        case '}' | ']'                            ⇒ receiver.onBreak(); ix
        case 'n'                                  ⇒ parseNull(index)
        case 'f'                                  ⇒ parseFalse(ix)
        case 't'                                  ⇒ parseTrue(index)
        case '-'                                  ⇒ parseNumber(ix, appendChar(ix, 0, '-'), 0, negative = true)
        case '0'                                  ⇒ parseNumber0(ix)
        case c @ ('1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9') ⇒
          parseNumber(ix, appendChar(ix, 0, c.toChar), (c - '0').toLong, negative = false)

        case _ ⇒ failInvalidJsonValue(ix)
      }
    } else {
      receiver.onEndOfInput()
      index
    }
  }
}
