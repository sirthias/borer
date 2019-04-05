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

  private[this] var chars: Array[Char] = new Array[Char](16)
  private[this] var charsCursor: Int   = _

  /**
    * Reads the next data item from the input and sends it to the given [[Receiver]].
    * The given [[Receiver]] receives exactly one call to one of its methods.
    * The returned `Long` is the index of the next byte to consume from the input
    * (and can be used for the subsequent call to this method).
    */
  @tailrec def pull[Input](input: Input, index: Long, receiver: Receiver)(implicit ia: InputAccess[Input]): Long = {

    def pos(ix: Long)              = Position(input, ix)
    def invalidJsonValue(ix: Long) = throw Error.InvalidJsonData(pos(ix), "Invalid JSON value")

    @inline def clearChars(): Unit = charsCursor = 0

    def appendChar(ix: Long, c: Char): Long = {
      val newCursor = charsCursor + 1
      if (newCursor > 0) {
        if (newCursor > chars.length) {
          val newLen = math.max(chars.length << 1, newCursor)
          chars = util.Arrays.copyOf(chars, newLen)
        }
        chars(charsCursor) = c
        charsCursor = newCursor
        ix
      } else throw Error.Overflow(pos(ix), "JSON String too long")
    }

    @inline def getString: String = if (charsCursor > 0) new String(chars, 0, charsCursor) else ""

    // format: OFF
    @inline def parseNull(ix: Long): Long =
      if (ia.hasByteAtIndex(input, ix + 2) &&
        ia.byteAt(input, ix) == 'u' &&
        ia.byteAt(input, ix + 1) == 'l' &&
        ia.byteAt(input, ix + 2) == 'l') {
        receiver.onNull()
        ix + 3
      } else invalidJsonValue(ix)

    @inline def parseFalse(ix: Long): Long =
      if (ia.hasByteAtIndex(input, ix + 3) &&
        ia.byteAt(input, ix) == 'a' &&
        ia.byteAt(input, ix + 1) == 'l' &&
        ia.byteAt(input, ix + 2) == 's' &&
        ia.byteAt(input, ix + 3) == 'e') {
        receiver.onBool(value = false)
        ix + 4
      } else invalidJsonValue(ix)

    @inline def parseTrue(ix: Long): Long =
      if (ia.hasByteAtIndex(input, ix + 2) &&
        ia.byteAt(input, ix) == 'r' &&
        ia.byteAt(input, ix + 1) == 'u' &&
        ia.byteAt(input, ix + 2) == 'e') {
        receiver.onBool(value = true)
        ix + 3
      } else invalidJsonValue(ix)
    // format: ON

    @tailrec def parseNonLongNumber(ix: Long): Long = {
      def result(): Long = {
        receiver.onNumberString(getString)
        ix
      }

      if (ia.hasByteAtIndex(input, ix)) {
        val c = ia.byteAt(input, ix).toChar
        (c: @switch) match {
          case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '.' | '+' | '-' | 'e' | 'E' ⇒
            parseNonLongNumber(appendChar(ix, c) + 1)
          case _ ⇒ result()
        }
      } else result()
    }

    @tailrec def parseNumber(ix: Long, value: Long, negative: Boolean): Long = {
      @inline def result(): Long = {
        val long = if (negative) -value else value
        if (Util.isInt(long)) receiver.onInt(long.toInt) else receiver.onLong(long)
        ix
      }
      if (ia.hasByteAtIndex(input, ix)) {
        val c = ia.byteAt(input, ix).toChar
        (c: @switch) match {
          case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ⇒
            val next = appendChar(ix, c) + 1
            // check whether by adding this digit we overflow the Long range or not
            if (value < Long.MaxValue / 10 || value == Long.MaxValue / 10 && c != '8' && c != '9') {
              val newValue = (value << 3) + (value << 1) + c - '0' // same as multiplication by 10
              parseNumber(next, newValue, negative)
            } else if (negative && value == Long.MaxValue / 10 && c == '8' && // special case: Long.MinValue
                       !(ia.hasByteAtIndex(input, ix + 1) && "0123456789.eE".contains(ia.byteAt(input, ix + 1).toChar))) {
              receiver.onLong(Long.MinValue)
              ix + 1
            } else parseNonLongNumber(next)
          case '.' | 'e' | 'E' ⇒ parseNonLongNumber(appendChar(ix, c) + 1)
          case _               ⇒ result()
        }
      } else result()
    }

    @inline def parseNumber0(ix: Long): Long = {
      @inline def result(): Long = {
        receiver.onInt(0)
        ix
      }
      if (ia.hasByteAtIndex(input, ix)) {
        val c = ia.byteAt(input, ix).toChar
        c match {
          case '.' | 'e' | 'E' ⇒
            clearChars()
            parseNonLongNumber(appendChar(appendChar(ix, '0'), c) + 1)
          case _ ⇒ result()
        }
      } else result()
    }

    @tailrec def parseUtf8String(ix: Long): Long = {
      import Error.{InvalidJsonData ⇒ IJD}
      if (ia.hasByteAtIndex(input, ix)) {
        var c = ia.byteAt(input, ix) & 0xFF
        if ((c >> 7) == 0) { // single byte UTF-8 char
          // simple bloom-filter that quick-matches all special chars and one good char (as collateral),
          // specifically it matches: \u0000 - \u001f, '"', '\' and 'b'
          if (((1L << (c - 32)) & ((31 - c) >> 31) & 0xeffffffffffffffbL) == 0) {
            c match {
              case '"' ⇒ receiver.onChars(chars, 0, charsCursor); ix + 1
              case 'b' ⇒ parseUtf8String(appendChar(ix, 'b') + 1)
              case '\\' ⇒
                var i = ix + 1
                if (ia.hasByteAtIndex(input, i)) {
                  ia.byteAt(input, i).toInt match {
                    case x @ ('"' | '/' | '\\') ⇒ c = x
                    case 'b'                    ⇒ c = '\b'
                    case 'f'                    ⇒ c = '\f'
                    case 'n'                    ⇒ c = '\n'
                    case 'r'                    ⇒ c = '\r'
                    case 't'                    ⇒ c = '\t'
                    case 'u' ⇒
                      i += 4
                      if (ia.hasByteAtIndex(input, i)) {
                        c = Util.hexValue(ia.byteAt(input, i - 3).toChar)
                        c = (c << 4) + Util.hexValue(ia.byteAt(input, i - 2).toChar)
                        c = (c << 4) + Util.hexValue(ia.byteAt(input, i - 1).toChar)
                        c = (c << 4) + Util.hexValue(ia.byteAt(input, i).toChar)
                        if (0xD800 <= c && c < 0xE000) {
                          throw IJD(pos(ix), s"Illegal Unicode Code point [${Integer.toHexString(c)}]")
                        }
                      } else throw IJD(pos(ix), "Unterminated String at end of Input")
                    case _ ⇒ throw IJD(pos(ix), "Illegal JSON escape sequence")
                  }
                  parseUtf8String(appendChar(i, c.toChar) + 1)
                } else throw IJD(pos(ix), "Unterminated String at end of Input")
              case x ⇒ throw IJD(pos(ix), s"Illegal JSON String character [${Integer.toHexString(x)}]")
            }
          } else parseUtf8String(appendChar(ix, c.toChar) + 1)
        } else if (ia.hasByteAtIndex(input, ix + 1)) { // multi-byte UTF-8 char
          @inline def trailingByte(i: Long): Int = {
            val x = ia.byteAt(input, i) & 0xFF
            if ((x >> 6) != 2) throw IJD(pos(i), "Illegal UTF-8 character encoding")
            x & 0x3F
          }
          var i = ix + 1
          if ((c >> 6) == 3) { // 2, 3 or 4-byte UTF-8 char
            if ((c >> 5) == 7) { // 3 or 4-byte UTF-8 char
              if ((c >> 3) == 0x1E) { // 4-byte UTF-8 char
                c = (c & 7) << 6 | trailingByte(i)
                i += 1
              } else { // 3-byte UTF-8 char
                if ((c >> 4) != 0xE) throw IJD(pos(i), "Illegal UTF-8 character encoding")
                c = c & 0xF
              }
              c = (c << 6) | trailingByte(i)
              i += 1
            } else { // 2-byte UTF-8 char
              if ((c >> 5) != 6) throw IJD(pos(i), "Illegal UTF-8 character encoding")
              c = c & 0x1F
            }
            c = (c << 6) | trailingByte(i)
          } else throw IJD(pos(i), "Illegal UTF-8 character encoding")
          if (c < 0xD800 || 0xE000 <= c) parseUtf8String {
            if (c > 0xFFFF) { // surrogate pair required?
              appendChar(i, ((c >> 10) + 0xD800 - (0x10000 >> 10)).toChar) // high surrogate
              appendChar(i + 1, (0xDC00 + (c & 0x3FF)).toChar)             // low surrogate
            } else appendChar(i, c.toChar) + 1
          } else throw IJD(pos(i), s"Illegal Unicode Code point [${Integer.toHexString(c)}]")
        } else throw IJD(pos(ix), "Unterminated String at end of Input")
      } else throw IJD(pos(ix), "Unterminated String at end of Input")
    }

    if (ia.hasByteAtIndex(input, index)) {
      val ix = index + 1
      (ia.byteAt(input, index): @switch) match {
        case ' ' | '\t' | '\n' | '\r' | ',' | ':' ⇒ pull(input, ix, receiver)
        case 'n'                                  ⇒ parseNull(ix)
        case 'f'                                  ⇒ parseFalse(ix)
        case 't'                                  ⇒ parseTrue(ix)
        case '{'                                  ⇒ receiver.onMapStart(); ix
        case '['                                  ⇒ receiver.onArrayStart(); ix
        case '}' | ']'                            ⇒ receiver.onBreak(); ix

        case '-' ⇒
          clearChars()
          parseNumber(appendChar(ix, '-'), 0, negative = true)

        case '0' ⇒ parseNumber0(ix)

        case c @ ('1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9') ⇒
          clearChars()
          parseNumber(appendChar(ix, c.toChar), (c - '0').toLong, negative = false)

        case '"' ⇒
          clearChars()
          parseUtf8String(ix)

        case _ ⇒ invalidJsonValue(ix)
      }
    } else {
      receiver.onEndOfInput()
      index
    }
  }
}
