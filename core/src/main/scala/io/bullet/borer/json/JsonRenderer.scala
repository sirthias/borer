/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.json

import io.bullet.borer._
import io.bullet.borer.internal.Util

import scala.annotation.tailrec

/**
  * Encapsulates the basic JSON rendering logic.
  * Also performs inline UTF-8 encoding to raw bytes.
  *
  * This [[Receiver]] only renders data items that can be directly represented in JSON, specifically
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
  * These data items are not supported and throw an exception upon reception:
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
final private[borer] class JsonRenderer(var out: Output) extends Receiver.Renderer {

  private[this] var level: Int           = _ // valid range: 0 - 63
  private[this] var levelType: Long      = _ // keeps the type of each level as a bit map: 0 -> Array, 1 -> Map
  private[this] var levelCount: Long     = _ // for each level: last bit of element count
  private[this] var sepRequired: Boolean = _ // whether a separator required before the next element

  def onNull(): Unit =
    if (isNotMapKey) {
      out = count(sep(out).writeAsBytes('n', 'u', 'l', 'l'))
    } else failCannotBeMapKey("null")

  def onUndefined(): Unit =
    failUnsupported(out, "the `undefined` value")

  def onBoolean(value: Boolean): Unit =
    if (isNotMapKey) {
      out = count {
        if (value) (if (sepRequired) out.writeAsByte(separator) else out).writeAsBytes('t', 'r', 'u', 'e')
        else
          (if (sepRequired) out.writeAsBytes(separator, 'f') else out.writeAsByte('f')).writeAsBytes('a', 'l', 's', 'e')
      }
    } else failCannotBeMapKey("boolean values")

  def onInt(value: Int): Unit =
    onLong(value.toLong)

  def onLong(value: Long): Unit =
    if (isNotMapKey) {
      out = count(writeLong(sep(out), value))
    } else failCannotBeMapKey("integer values")

  def onOverLong(negative: Boolean, value: Long): Unit =
    if (isNotMapKey) {
      out = count {
        def writeOverLong(o: Output, v: Long) = {
          val q = (v >>> 1) / 5           // value / 10
          val r = v - (q << 3) - (q << 1) // value - 10*q
          writeLong(o, q).writeAsByte('0' + r.toInt)
        }
        if (negative) {
          val v = value + 1
          if (v == 0) out.writeStringAsAsciiBytes("-18446744073709551616")
          else writeOverLong(if (sepRequired) out.writeAsBytes(separator, '-') else out.writeAsByte('-'), v)
        } else writeOverLong(if (sepRequired) out.writeAsByte(separator) else out, value)
      }
    } else failCannotBeMapKey("an Overlong")

  def onFloat16(value: Float): Unit =
    failUnsupported(out, "Float16 values")

  def onFloat(value: Float): Unit =
    if (isNotMapKey) {
      if (!value.isNaN) {
        if (!value.isInfinity) {
          out = count(sep(out).writeStringAsAsciiBytes(Util.floatToString(value)))
        } else failUnsupported(out, "`Infinity` floating point values")
      } else failUnsupported(out, "`NaN` floating point values")
    } else failCannotBeMapKey("floating point values")

  def onDouble(value: Double): Unit =
    if (isNotMapKey) {
      if (!value.isNaN) {
        if (!value.isInfinity) {
          out = count(sep(out).writeStringAsAsciiBytes(Util.doubleToString(value)))
        } else failUnsupported(out, "`Infinity` floating point values")
      } else failUnsupported(out, "`NaN` floating point values")
    } else failCannotBeMapKey("floating point values")

  def onNumberString(value: String): Unit =
    if (isNotMapKey) {
      out = count(sep(out).writeStringAsAsciiBytes(value))
    } else failCannotBeMapKey("number strings")

  def onBytes[Bytes: ByteAccess](value: Bytes): Unit =
    failUnsupported(out, "byte strings")

  def onBytesStart(): Unit =
    failUnsupported(out, "byte string streams")

  def onString(value: String): Unit = {
    @tailrec def rec(out: Output, ix: Int): Output =
      if (ix < value.length) {
        @inline def escaped(c: Char) = out.writeAsBytes('\\', c)
        var index                    = ix
        val newOut =
          value.charAt(ix) match {
            case '"'  => escaped('"')
            case '\\' => escaped('\\')
            case c if c >= 0x20 => // we re-encode the character (or surrogate pair) from UTF-16 to UTF-8 right here
              if (c > 0x7F) {
                var codePoint = c.toInt
                (if (codePoint > 0x7FF) {
                   (if (0xD800 <= codePoint && codePoint < 0xE000) { // UTF-16 high surrogate (i.e. first of pair)
                      if (codePoint < 0xDC00) {
                        index += 1
                        if (index < value.length) {
                          codePoint = Character.toCodePoint(c, value.charAt(index))
                          out.writeBytes((0xF0 | (codePoint >> 18)).toByte, (0x80 | ((codePoint >> 12) & 0x3F)).toByte)
                        } else failValidation("Truncated UTF-16 surrogate pair at end of string \"" + value + '"')
                      } else
                        failValidation(
                          "Invalid UTF-16 surrogate pair at index " + codePoint + " of string \"" + value + '"')
                    } else out.writeAsByte(0xE0 | (codePoint >> 12))) // 3-byte UTF-8 codepoint
                     .writeAsByte(0x80 | ((codePoint >> 6) & 0x3F))
                 } else out.writeAsByte(0xC0 | (codePoint >> 6))) // 2-byte UTF-8 codepoint
                  .writeAsByte(0x80 | (codePoint & 0x3F))
              } else out.writeAsByte(c)

            case '\b' => escaped('b')
            case '\f' => escaped('f')
            case '\n' => escaped('n')
            case '\r' => escaped('r')
            case '\t' => escaped('t')
            case c =>
              out
                .writeAsBytes('\\', 'u', '0', '0')
                .writeBytes(lowerHexDigit(c.toInt >> 4).toByte, lowerHexDigit(c.toInt).toByte)
          }
        rec(newOut, index + 1)
      } else out

    out = count(rec(if (sepRequired) out.writeAsBytes(separator, '"') else out.writeAsByte('"'), 0).writeAsByte('"'))
  }

  def onChars(length: Int, buffer: Array[Char]): Unit =
    onString(new String(buffer, 0, length))

  def onText[Bytes](value: Bytes)(implicit ba: ByteAccess[Bytes]): Unit =
    failUnsupported(out, "text byte strings")

  def onTextStart(): Unit =
    failUnsupported(out, "text byte string streams")

  def onArrayHeader(length: Long): Unit =
    failUnsupported(out, "definite-length arrays")

  def onArrayStart(): Unit =
    if (isNotMapKey) {
      out = if (sepRequired) out.writeAsBytes(separator, '[') else out.writeAsByte('[')
      level += 1
      if (level < 64) {
        levelType <<= 1
        levelCount <<= 1
        sepRequired = false
      } else failUnsupported(out, "more than 64 JSON Array/Object nesting levels")
    } else failCannotBeMapKey("arrays")

  def onMapHeader(length: Long): Unit =
    failUnsupported(out, "definite-length maps")

  def onMapStart(): Unit =
    if (isNotMapKey) {
      out = if (sepRequired) out.writeAsBytes(separator, '{') else out.writeAsByte('{')
      level += 1
      if (level < 64) {
        levelType = (levelType << 1) | 1
        levelCount <<= 1
        sepRequired = false
      } else failUnsupported(out, "more than 64 JSON Array/Object nesting levels")
    } else failCannotBeMapKey("maps")

  def onBreak(): Unit = {
    val c = if ((levelType & 1) == 0) ']' else '}'
    if (level > 0) {
      level -= 1
      levelType >>>= 1
      levelCount >>>= 1
    } else failValidation("Received BREAK without corresponding ArrayStart or MapStart")
    out = count(out.writeAsByte(c)) // level-entering items are only counted when the level is exited, not when entered
  }

  def onTag(value: Tag): Unit         = failUnsupported(out, "CBOR tags")
  def onSimpleValue(value: Int): Unit = failUnsupported(out, "CBOR Simple Values")
  def onEndOfInput(): Unit            = ()

  @inline private def sep(out: Output): Output =
    if (sepRequired) out.writeAsByte(separator) else out

  @inline private def separator: Char = if ((levelType & levelCount & 1) != 0) ':' else ','

  @inline private def isNotMapKey: Boolean = (levelType & ~levelCount & 1) == 0

  @inline private def count(out: Output): Output = {
    levelCount ^= 1
    sepRequired = true
    out
  }

  private def writeLong(out: Output, value: Long): Output =
    if (value != 0) {
      if (value != Long.MinValue) {
        @inline def div10(i: Int) = {
          var q = (i << 3) + (i << 2)
          q += (q << 12) + (q << 8) + (q << 4) + i
          q >>>= 19
          q // 52429 * l / 524288 = l * 0.10000038146972656
        }

        // for small numbers we can use the "fast-path"
        def phase2(i: Int): Output = {
          val q = div10(i)
          val r = i - q * 10
          val newOut =
            if (q != 0) phase2(q)
            else if (value < 0) out.writeAsByte('-')
            else out
          newOut.writeAsByte('0' + r)
        }

        // for large numbers we bite the bullet of performing one division every two digits
        def phase1(l: Long): Output =
          if (l > 65535L) {
            val q  = l / 100
            val r  = (l - q * 100).toInt
            val rq = div10(r)
            phase1(q).writeBytes(('0' + rq).toByte, ('0' + r - rq * 10).toByte)
          } else phase2(l.toInt)

        phase1(math.abs(value))
      } else out.writeStringAsAsciiBytes("-9223372036854775808")
    } else out.writeAsByte('0')

  // fast branchless implementation returning the lower-case hex digit corresponding to the last 4 bits of the given Int
  @inline private def lowerHexDigit(int: Int): Int = {
    val i = int & 0x0F
    48 + i + (39 & ((9 - i) >> 31))
  }

  private def failUnsupported(out: Output, what: String) =
    throw new Borer.Error.Unsupported(out, s"The JSON renderer doesn't support $what")

  private def failCannotBeMapKey(what: String) =
    throw new Borer.Error.ValidationFailure(out, s"JSON does not support $what as a map key")

  private def failValidation(msg: String) =
    throw new Borer.Error.ValidationFailure(out, msg)
}

object JsonRenderer extends (Output => JsonRenderer) {
  def apply(out: Output) = new JsonRenderer(out)
}
