/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.json

import java.lang.{Double ⇒ JDouble}
import java.math.{BigDecimal ⇒ JBigDecimal, BigInteger ⇒ JBigInteger}
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

  // indicates the buffered value(s) that the next pull will produce, if any
  // >= 0: character (byte)
  // -1: no value buffered
  private[this] var buffered: Int = -1

  private[this] var chars: Array[Char] = new Array[Char](16)
  private[this] var charsCursor: Int   = _

  /**
    * Reads the next JSON data item from the input and sends it to the given [[Receiver]].
    * The given [[Receiver]] receives exactly one call to one of its methods,
    * whose result is also the return value of this call to `pull`.
    */
  @tailrec def pull(input: Input, receiver: Receiver[Input]): Input = {

    def invalidJsonValue(in: Input) = throw Error.InvalidJsonData(in, "Invalid JSON value")

    // format: OFF
    @inline def parseNull(inp: Input): Input = {
      var in = inp
      if (in.hasBytes(3) &&
        { in = in.readByte(); in.lastByte } == 'u' &&
        { in = in.readByte(); in.lastByte } == 'l' &&
        { in = in.readByte(); in.lastByte } == 'l') {
        receiver.onNull(in)
      } else invalidJsonValue(in)
    }

    @inline def parseFalse(inp: Input): Input = {
      var in = inp
      if (in.hasBytes(4) &&
        { in = in.readByte(); in.lastByte } == 'a' &&
        { in = in.readByte(); in.lastByte } == 'l' &&
        { in = in.readByte(); in.lastByte } == 's' &&
        { in = in.readByte(); in.lastByte } == 'e') {
        receiver.onBool(in, value = false)
      } else invalidJsonValue(in)
    }

    @inline def parseTrue(inp: Input): Input = {
      var in = inp
      if (in.hasBytes(3) &&
        { in = in.readByte(); in.lastByte } == 'r' &&
        { in = in.readByte(); in.lastByte } == 'u' &&
        { in = in.readByte(); in.lastByte } == 'e') {
        receiver.onBool(in, value = true)
      } else invalidJsonValue(in)
    }
    // format: ON

    @tailrec def parseNonLongNumber(inp: Input, decimal: Boolean): Input = {
      def result(in: Input) =
        if (decimal) {
          // Unfortunately, once we are here, there don't appear to be any great options any more
          // with regard to high performance logic. So, we focus on correctness and conciseness instead here.
          // Improvement ideas are always welcome!
          val s      = getString
          val double = JDouble.parseDouble(s)
          // Is there a better way to figure out whether the double parsing was lossless?
          if (!double.isNaN && !double.isInfinite && Util.doubleToString(double) == s) {
            val float = double.toFloat
            if (float.toDouble == double) receiver.onFloat(in, float)
            else receiver.onDouble(in, double)
          } else receiver.onBigDecimal(in, new JBigDecimal(s))
        } else {
          val value = new JBigInteger(getString)
          value.bitLength match {
            case n if n < 32            ⇒ receiver.onInt(in, value.intValue)
            case n if n < 64            ⇒ receiver.onLong(in, value.longValue)
            case 64 if value.signum > 0 ⇒ receiver.onOverLong(in, negative = false, value.longValue)
            case 64                     ⇒ receiver.onOverLong(in, negative = true, ~value.longValue)
            case _                      ⇒ receiver.onBigInteger(in, value)
          }
        }

      if (inp.hasBytes(1)) {
        val in = inp.readByte()
        val c  = in.lastByte.toChar
        (c: @switch) match {
          case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '.' | '+' | '-' | 'e' | 'E' ⇒
            parseNonLongNumber(appendChar(in, c), decimal = decimal || c == '.')
          case _ ⇒
            buffered = c.toInt
            result(in)
        }
      } else result(inp)
    }

    @tailrec def parseNumber(inp: Input, value: Long, negative: Boolean): Input = {
      @inline def result(in: Input) = {
        val long = if (negative) -value else value
        if (Util.isInt(long)) receiver.onInt(in, long.toInt) else receiver.onLong(in, long)
      }
      if (inp.hasBytes(1)) {
        val in = inp.readByte()
        val c  = in.lastByte.toChar
        (c: @switch) match {
          case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ⇒
            appendChar(in, c)
            if (value <= Long.MaxValue / 10 - 1) { // largest value that cannot overflow below
              val newValue = (value << 3) + (value << 1) + c - '0' // same as multiplication by 10
              parseNumber(in, newValue, negative)
            } else parseNonLongNumber(in, decimal = false) // can't be sure to not overflow, so break out
          case '.' | 'e' | 'E' ⇒
            parseNonLongNumber(appendChar(in, c), decimal = c == '.')
          case _ ⇒
            buffered = c.toInt
            result(in)
        }
      } else result(inp)
    }

    @inline def parseNumber0(inp: Input): Input =
      if (inp.hasBytes(1)) {
        val in = inp.readByte()
        val c  = in.lastByte.toChar
        (c: @switch) match {
          case '.' | 'e' | 'E' ⇒
            clearChars()
            parseNonLongNumber(appendChar(appendChar(in, '0'), c), decimal = c == '.')
          case _ ⇒
            buffered = c.toInt
            receiver.onInt(inp, 0)
        }
      } else receiver.onInt(inp, 0)

    @tailrec def parseUtf8String(inp: Input): Input = {
      if (inp.hasBytes(1)) {
        var in: Input = inp.readByte()
        var c         = in.lastByte & 0xFF
        if ((c >> 7) == 0) { // single byte UTF-8 char
          // simple bloom-filter that quick-matches all special chars and one good char (as collateral),
          // specifically it matches: \u0000 - \u001f, '"', '\' and 'b'
          if (((1L << (c - 32)) & ((31 - c) >> 31) & 0xeffffffffffffffbL) == 0) {
            c match {
              case '"' ⇒ receiver.onString(in, getString)
              case 'b' ⇒ parseUtf8String(appendChar(in, 'b'))
              case '\\' ⇒
                if (in.hasBytes(1)) {
                  in = in.readByte()
                  in.lastByte.toInt match {
                    case x @ ('"' | '/' | '\\') ⇒ c = x
                    case 'b'                    ⇒ c = '\b'
                    case 'f'                    ⇒ c = '\f'
                    case 'n'                    ⇒ c = '\n'
                    case 'r'                    ⇒ c = '\r'
                    case 't'                    ⇒ c = '\t'
                    case 'u' ⇒
                      if (in.hasBytes(4)) {
                        c = { in = in.readByte(); Util.hexValue(in.lastByte.toChar) }
                        c = (c << 4) + { in = in.readByte(); Util.hexValue(in.lastByte.toChar) }
                        c = (c << 4) + { in = in.readByte(); Util.hexValue(in.lastByte.toChar) }
                        c = (c << 4) + { in = in.readByte(); Util.hexValue(in.lastByte.toChar) }
                        if (0xD800 <= c && c < 0xE000) {
                          throw Error.InvalidJsonData(in, s"Illegal Unicode Code point [${Integer.toHexString(c)}]")
                        }
                      } else throw Error.InvalidJsonData(in, "Unterminated String at end of Input")
                    case _ ⇒ throw Error.InvalidJsonData(in, "Illegal JSON escape sequence")
                  }
                  parseUtf8String(appendChar(in, c.toChar))
                } else throw Error.InvalidJsonData(in, "Unterminated String at end of Input")
              case x ⇒ throw Error.InvalidJsonData(in, s"Illegal JSON String character [${Integer.toHexString(x)}]")
            }
          } else parseUtf8String(appendChar(in, c.toChar))
        } else {
          @inline def trailingByte(in: Input): Int = {
            val x = in.lastByte & 0xFF
            if ((x >> 6) != 2) throw Error.InvalidJsonData(in, s"Illegal UTF-8 character encoding")
            x & 0x3F
          }
          in = in.readByte()
          if ((c >> 6) == 3) { // 2, 3 or 4-byte UTF-8 char
            if ((c >> 5) == 7) { // 3 or 4-byte UTF-8 char
              if ((c >> 3) == 0x1E) { // 4-byte UTF-8 char
                c = (c & 7) << 6 | trailingByte(in)
                in = in.readByte()
              } else { // 3-byte UTF-8 char
                if ((c >> 4) != 0xE) throw Error.InvalidJsonData(in, "Illegal UTF-8 character encoding")
                c = c & 0xF
              }
              c = (c << 6) | trailingByte(in)
              in = in.readByte()
            } else { // 2-byte UTF-8 char
              if ((c >> 5) != 6) throw Error.InvalidJsonData(in, "Illegal UTF-8 character encoding")
              c = c & 0x1F
            }
            c = (c << 6) | trailingByte(in)
          } else throw Error.InvalidJsonData(in, "Illegal UTF-8 character encoding")

          if (c < 0xD800 || 0xE000 <= c) {
            if (c > 0xFFFF) { // surrogate pair required?
              appendChar(in, ((c >> 10) + 0xD800 - (0x10000 >> 10)).toChar) // high surrogate
              appendChar(in, (0xDC00 + (c & 0x3FF)).toChar)                 // low surrogate
            } else appendChar(in, c.toChar)
            parseUtf8String(in)
          } else throw Error.InvalidJsonData(in, s"Illegal Unicode Code point [${Integer.toHexString(c)}]")
        }
      } else throw Error.InvalidJsonData(inp, "Unterminated String at end of Input")
    }

    var in = input
    val charCode: Int =
      if (buffered < 0) {
        if (in.hasBytes(1)) {
          in = in.readByte()
          in.lastByte.toInt
        } else -1
      } else {
        val c = buffered
        buffered = -1
        c
      }

    (charCode: @switch) match {
      case ' ' | '\t' | '\n' | '\r' | ',' | ':' ⇒ pull(in, receiver)
      case 'n'                                  ⇒ parseNull(in)
      case 'f'                                  ⇒ parseFalse(in)
      case 't'                                  ⇒ parseTrue(in)
      case '{'                                  ⇒ receiver.onMapStart(in)
      case '['                                  ⇒ receiver.onArrayStart(in)
      case '}' | ']'                            ⇒ receiver.onBreak(in)
      case -1                                   ⇒ receiver.onEndOfInput(in)

      case '-' ⇒
        clearChars()
        parseNumber(appendChar(in, '-'), 0, negative = true)

      case '0' ⇒ parseNumber0(in)

      case '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ⇒
        clearChars()
        parseNumber(appendChar(in, charCode.toChar), (charCode - '0').toLong, negative = false)

      case '"' ⇒
        clearChars()
        parseUtf8String(in)

      case _ ⇒ invalidJsonValue(in)
    }
  }

  @inline private def clearChars(): Unit = charsCursor = 0

  private def appendChar(in: Input, c: Char): Input = {
    val newCursor = charsCursor + 1
    if (newCursor > 0) {
      if (chars.length < newCursor) {
        val newLen = math.max(chars.length << 1, newCursor)
        chars = util.Arrays.copyOf(chars, newLen)
      }
      chars(charsCursor) = c
      charsCursor = newCursor
      in
    } else throw Error.Overflow(in, "JSON String too long")
  }

  @inline private def getString: String = if (charsCursor > 0) new String(chars, 0, charsCursor) else ""
}
