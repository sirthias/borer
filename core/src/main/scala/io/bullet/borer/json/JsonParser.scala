/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.json

import java.util

import io.bullet.borer._

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

  /**
    * Reads the next data item from the input and sends it to the given [[Receiver]].
    * The given [[Receiver]] receives exactly one call to one of its methods.
    * The returned `Long` is the index of the next byte to consume from the input
    * (and can be used for the subsequent call to this method).
    */
  @tailrec def pull[Input](input: Input, index: Long, receiver: Receiver)(implicit ia: InputAccess[Input]): Long = {

    def pos(ix: Long)              = Position(input, ix)
    def invalidJsonValue(ix: Long) = throw new Error.InvalidJsonData(pos(ix), "Invalid JSON value")

    def appendChar(ix: Long, strLen: Int, c: Char): Int = {
      val newCursor = strLen + 1
      if (newCursor > 0) {
        if (newCursor > chars.length) {
          val newLen = math.max(chars.length << 1, newCursor)
          chars = util.Arrays.copyOf(chars, newLen)
        }
        chars(strLen) = c
        newCursor
      } else throw new Error.Overflow(pos(ix), "JSON String too long")
    }

    @inline def getString(strLen: Int): String = if (strLen > 0) new String(chars, 0, strLen) else ""

    // format: OFF
    @inline def parseNull(ix: Long): Long =
      if (ia.byteAt(input, ix) == 'u' && ia.byteAt(input, ix + 1) == 'l' && ia.byteAt(input, ix + 2) == 'l') {
        receiver.onNull()
        ix + 3
      } else invalidJsonValue(ix)

    @inline def parseFalse(ix: Long): Long =
      if (ia.byteAt(input, ix) == 'a' &&
        ia.byteAt(input, ix + 1) == 'l' &&
        ia.byteAt(input, ix + 2) == 's' &&
        ia.byteAt(input, ix + 3) == 'e') {
        receiver.onBool(value = false)
        ix + 4
      } else invalidJsonValue(ix)

    @inline def parseTrue(ix: Long): Long =
      if (ia.byteAt(input, ix) == 'r' &&
        ia.byteAt(input, ix + 1) == 'u' &&
        ia.byteAt(input, ix + 2) == 'e') {
        receiver.onBool(value = true)
        ix + 3
      } else invalidJsonValue(ix)
    // format: ON

    @tailrec def parseNonLongNumber(ix: Long, strLen: Int): Long = {
      def result(): Long = {
        receiver.onNumberString(getString(strLen))
        ix
      }
      if (ix < ia.length(input)) {
        val c = ia.byteAt(input, ix).toChar
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
        val c                         = ia.byteAt(input, ix).toChar
        def breakOutToNonLongNumber() = parseNonLongNumber(ix + 1, appendChar(ix, strLen, c))
        (c: @switch) match {
          case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ⇒
            // check whether by adding this digit we overflow the Long range or not
            if (value < Long.MaxValue / 10 || value == Long.MaxValue / 10 && c != '8' && c != '9') {
              val newValue = (value << 3) + (value << 1) + c - '0' // same as multiplication by 10
              parseNumber(ix + 1, appendChar(ix, strLen, c), newValue, negative)
            } else if (negative && value == Long.MaxValue / 10 && c == '8' && // special case: Long.MinValue
                       !(ix < (ia.length(input) - 1) && "0123456789.eE".contains(ia.byteAt(input, ix + 1).toChar))) {
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
        val c = ia.byteAt(input, ix).toChar
        c match {
          case '.' | 'e' | 'E' ⇒ parseNonLongNumber(ix + 1, appendChar(ix, appendChar(ix, 0, '0'), c))
          case _               ⇒ result()
        }
      } else result()
    }

    @tailrec def parseUtf8String(ix: Long, strLen: Int): Long = {
      import Error.{InvalidJsonData ⇒ IJD}
      def failIllegalCP(i: Long, c: Int) =
        throw new IJD(pos(i), s"Illegal Unicode Code point [${Integer.toHexString(c)}]")
      def failIllegalUtf8(i: Long) = throw new IJD(pos(i), "Illegal UTF-8 character encoding")
      var c                        = ia.byteAt(input, ix) & 0xFF
      if ((JsonParser.CharMask(c >> 6) & (1L << (c & 0x3F))) == 0) {
        parseUtf8String(ix + 1, appendChar(ix, strLen, c.toChar))
      } else if (c == '"') {
        receiver.onChars(chars, 0, strLen)
        ix + 1
      } else if (c == '\\') {
        var i = ix + 1
        ia.byteAt(input, i).toInt match {
          case x @ ('"' | '/' | '\\') ⇒ c = x
          case 'b'                    ⇒ c = '\b'
          case 'f'                    ⇒ c = '\f'
          case 'n'                    ⇒ c = '\n'
          case 'r'                    ⇒ c = '\r'
          case 't'                    ⇒ c = '\t'
          case 'u' ⇒
            i += 4
            c = Util.hexValue(ia.byteAt(input, i - 3).toChar)
            c = (c << 4) + Util.hexValue(ia.byteAt(input, i - 2).toChar)
            c = (c << 4) + Util.hexValue(ia.byteAt(input, i - 1).toChar)
            c = (c << 4) + Util.hexValue(ia.byteAt(input, i).toChar)
            if (0xD800 <= c && c < 0xE000) failIllegalCP(ix, c)
          case _ ⇒ throw new IJD(pos(ix), "Illegal JSON escape sequence")
        }
        parseUtf8String(i + 1, appendChar(i, strLen, c.toChar))
      } else if (c > 128) { // multi-byte UTF-8 char
        @inline def trailingByte(i: Long): Int = {
          val x = ia.byteAt(input, i) & 0xFF
          if ((x >> 6) != 2) failIllegalUtf8(i)
          x & 0x3F
        }
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
          val newStrLen =
            if (c > 0xFFFF) { // surrogate pair required?
              appendChar(i, strLen, ((c >> 10) + 0xD800 - (0x10000 >> 10)).toChar) // high surrogate
              appendChar(i, strLen + 1, (0xDC00 + (c & 0x3FF)).toChar)             // low surrogate
            } else appendChar(i, strLen, c.toChar)
          parseUtf8String(i + 1, newStrLen)
        } else failIllegalCP(i, c)
      } else throw new IJD(pos(ix), s"Illegal JSON String character [${Integer.toHexString(c)}]")
    }

    if (index < ia.length(input)) {
      val ix = index + 1
      (ia.byteAt(input, index): @switch) match {
        case ' ' | '\t' | '\n' | '\r' | ',' | ':' ⇒ pull(input, ix, receiver)
        case '"'                                  ⇒ parseUtf8String(ix, 0)
        case '{'                                  ⇒ receiver.onMapStart(); ix
        case '['                                  ⇒ receiver.onArrayStart(); ix
        case '}' | ']'                            ⇒ receiver.onBreak(); ix
        case 'n'                                  ⇒ parseNull(ix)
        case 'f'                                  ⇒ parseFalse(ix)
        case 't'                                  ⇒ parseTrue(ix)
        case '-'                                  ⇒ parseNumber(ix, appendChar(ix, 0, '-'), 0, negative = true)
        case '0'                                  ⇒ parseNumber0(ix)
        case c @ ('1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9') ⇒
          parseNumber(ix, appendChar(ix, 0, c.toChar), (c - '0').toLong, negative = false)

        case _ ⇒ invalidJsonValue(ix)
      }
    } else {
      receiver.onEndOfInput()
      index
    }
  }
}

private object JsonParser {

  /**
    * Bit mask holding a 1-bit for all special characters
    */
  private final val CharMask: Array[Long] = {
    val mask                = new Array[Long](4)
    def flag(c: Char): Unit = mask(c >> 6) |= (1L << (c & 0x3f))
    ('\u0000' to '\u001f').foreach(flag)
    flag('"')
    flag('\\')
    mask(2) = -1L // all chars > 127
    mask(3) = -1L // need special handling
    mask
  }
}
