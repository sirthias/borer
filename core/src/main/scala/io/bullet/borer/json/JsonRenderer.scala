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
private[borer] final class JsonRenderer(var out: Output) extends Receiver.Renderer {

  private[this] var level: Int           = _ // valid range: 0 - 63
  private[this] var levelType: Long      = _ // keeps the type of each level as a bit map: 0 -> Array, 1 -> Map
  private[this] var sepRequired: Boolean = _ // whether a separator required before the next element
  private[this] var mapCount: Long       = _ // for each map level: 0 -> next element is key, 1 -> next element is value

  @inline private def isLevelMap: Boolean = ((levelType >> level) & 1) != 0

  def onNull(): Unit =
    out = count(sep(out).writeAsBytes('n', 'u', 'l', 'l'))

  def onUndefined(): Unit =
    unsupported(out, "the `undefined` value")

  def onBool(value: Boolean): Unit =
    out = count {
      val sep = separator
      if (value) (if (sep != '\u0000') out.writeAsByte(sep) else out).writeAsBytes('t', 'r', 'u', 'e')
      else (if (sep != '\u0000') out.writeAsBytes(sep, 'f') else out.writeAsByte('f')).writeAsBytes('a', 'l', 's', 'e')
    }

  def onInt(value: Int): Unit =
    onLong(value.toLong)

  def onLong(value: Long): Unit =
    out = count(writeLong(sep(out), value))

  def onOverLong(negative: Boolean, value: Long): Unit =
    out = count {
      def writeOverLong(o: Output, v: Long) = {
        val q = (v >>> 1) / 5 // value / 10
        val r = v - (q << 3) - (q << 1) // value - 10*q
        writeLong(o, q).writeAsByte('0' + r.toInt)
      }
      val sep = separator
      if (negative) {
        val v = value + 1
        if (v == 0) out.writeStringAsAsciiBytes("-18446744073709551616")
        else writeOverLong(if (sep != '\u0000') out.writeAsBytes(sep, '-') else out.writeAsByte('-'), v)
      } else writeOverLong(if (sep != '\u0000') out.writeAsByte(sep) else out, value)
    }

  def onFloat16(value: Float): Unit =
    unsupported(out, "Float16 values")

  def onFloat(value: Float): Unit =
    onDouble(value.toDouble)

  def onDouble(value: Double): Unit =
    if (!value.isNaN) {
      if (!value.isInfinity) {
        out = count(sep(out).writeStringAsAsciiBytes(Util.doubleToString(value)))
      } else unsupported(out, "`Infinity` floating point values")
    } else unsupported(out, "`NaN` floating point values")

  def onNumberString(value: String): Unit =
    out = count(sep(out).writeStringAsAsciiBytes(value))

  def onDecimal(integer: Long, fraction: Int): Unit =
    out = count(writeLong(writeLong(sep(out), integer).writeAsByte('.'), fraction.toLong))

  def onBytes[Bytes: ByteAccess](value: Bytes): Unit =
    unsupported(out, "byte strings")

  def onBytesStart(): Unit =
    unsupported(out, "byte string streams")

  def onString(value: String): Unit = {
    @tailrec def rec(out: Output, ix: Int): Output =
      if (ix < value.length) {
        def escaped(c: Char)  = out.writeAsBytes('\\', c)
        def fail(msg: String) = throw new Borer.Error.ValidationFailure(out, msg)

        value.charAt(ix) match {
          case '"'  ⇒ rec(escaped('"'), ix + 1)
          case '\\' ⇒ rec(escaped('\\'), ix + 1)
          case c if c >= 0x20 ⇒ // we re-encode the character (or surrogate pair) from UTF-16 to UTF-8 right here
            var index = ix
            val newOut =
              if (c > 0x7F) {
                var codePoint = c.toInt
                (if (codePoint > 0x7FF) {
                   (if (0xD800 <= codePoint && codePoint < 0xE000) { // UTF-16 high surrogate (i.e. first of pair)
                      if (codePoint < 0xDC00) {
                        index += 1
                        if (index < value.length) {
                          codePoint = Character.toCodePoint(c, value.charAt(index))
                          out.writeBytes((0xF0 | (codePoint >> 18)).toByte, (0x80 | ((codePoint >> 12) & 0x3F)).toByte)
                        } else fail("Truncated UTF-16 surrogate pair at end of string \"" + value + '"')
                      } else fail("Invalid UTF-16 surrogate pair at index " + codePoint + " of string \"" + value + '"')
                    } else out.writeAsByte(0xE0 | (codePoint >> 12))) // 3-byte UTF-8 codepoint
                     .writeAsByte(0x80 | ((codePoint >> 6) & 0x3F))
                 } else out.writeAsByte(0xC0 | (codePoint >> 6))) // 2-byte UTF-8 codepoint
                  .writeAsByte(0x80 | (codePoint & 0x3F))
              } else out.writeAsByte(c)
            rec(newOut, index + 1)

          case '\b' ⇒ rec(escaped('b'), ix + 1)
          case '\f' ⇒ rec(escaped('f'), ix + 1)
          case '\n' ⇒ rec(escaped('n'), ix + 1)
          case '\r' ⇒ rec(escaped('r'), ix + 1)
          case '\t' ⇒ rec(escaped('t'), ix + 1)
          case c ⇒
            val newOut = out
              .writeAsBytes('\\', 'u', '0', '0')
              .writeBytes(lowerHexDigit(c.toInt >> 4).toByte, lowerHexDigit(c.toInt).toByte)
            rec(newOut, ix + 1)
        }
      } else out

    val sep = separator
    out = count(rec(if (sep != '\u0000') out.writeAsBytes(sep, '"') else out.writeAsByte('"'), 0).writeAsByte('"'))
  }

  def onChars(length: Int, buffer: Array[Char]): Unit =
    onString(new String(buffer, 0, length))

  def onText[Bytes](value: Bytes)(implicit ba: ByteAccess[Bytes]): Unit =
    unsupported(out, "text byte strings")

  def onTextStart(): Unit =
    unsupported(out, "text byte string streams")

  def onArrayHeader(length: Long): Unit =
    unsupported(out, "definite-length arrays")

  def onArrayStart(): Unit = {
    val sep      = separator
    val newLevel = level + 1
    if (newLevel < 64) {
      levelType &= ~(1 << newLevel)
      level = newLevel
      sepRequired = false
      out = if (sep != '\u0000') out.writeAsBytes(sep, '[') else out.writeAsByte('[')
    } else unsupported(out, "more than 64 JSON Array/Object nesting levels")
  }

  def onMapHeader(length: Long): Unit =
    unsupported(out, "definite-length maps")

  def onMapStart(): Unit = {
    val sep      = separator
    val newLevel = level + 1
    if (newLevel < 64) {
      levelType |= 1 << newLevel
      level = newLevel
      sepRequired = false
      out = if (sep != '\u0000') out.writeAsBytes(sep, '{') else out.writeAsByte('{')
    } else unsupported(out, "more than 64 JSON Array/Object nesting levels")
  }

  def onBreak(): Unit = {
    val c = if (isLevelMap) '}' else ']'
    if (level > 0) level -= 1
    else throw new Borer.Error.InvalidJsonData(out, "Received BREAK without corresponding ArrayStart or MapStart")
    out = count(out.writeAsByte(c)) // level-entering items are only counted when the level is exited, not when entered
  }

  def onTag(value: Tag): Unit         = unsupported(out, "CBOR tags")
  def onSimpleValue(value: Int): Unit = unsupported(out, "CBOR Simple Values")
  def onEndOfInput(): Unit            = ()

  @inline private def separator: Char =
    if (sepRequired) {
      if (isLevelMap) {
        if (((mapCount >> level) & 1) != 0) ':' else ','
      } else ','
    } else '\u0000'

  @inline private def sep(out: Output): Output = {
    val s = separator
    if (s != '\u0000') out.writeAsByte(s) else out
  }

  private def count(out: Output): Output = {
    if (isLevelMap) mapCount ^= 1 << level
    sepRequired = true
    out
  }

  private def writeLong(out: Output, value: Long): Output =
    if (value != 0) {
      if (value != Long.MinValue) {
        def div10(i: Int) = {
          var q = (i << 3) + (i << 2)
          q += (q << 12) + (q << 8) + (q << 4) + i
          q >>>= 19
          q // 52429 * l / 524288 = l * 0.10000038146972656
        }
        def mul10(i: Int)   = (i << 3) + (i << 1)
        def mul100(l: Long) = (l << 6) + (l << 5) + (l << 2)

        // for small numbers we can use the "fast-path"
        def phase2(i: Int): Output = {
          val q = div10(i)
          val r = i - mul10(q)
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
            val r  = (l - mul100(q)).toInt
            val rq = div10(r)
            phase1(q).writeBytes(('0' + rq).toByte, ('0' + r - mul10(rq)).toByte)
          } else phase2(l.toInt)

        phase1(math.abs(value))
      } else out.writeStringAsAsciiBytes("-9223372036854775808")
    } else out.writeAsByte('0')

  // fast branchless implementation returning the lower-case hex digit corresponding to the last 4 bits of the given Int
  @inline private def lowerHexDigit(int: Int): Int = {
    val i = int & 0x0F
    48 + i + (39 & ((9 - i) >> 31))
  }

  private def unsupported(out: Output, what: String) =
    throw new Borer.Error.InvalidJsonData(out, s"The JSON renderer doesn't support $what")
}

object JsonRenderer extends (Output ⇒ JsonRenderer) {
  def apply(out: Output) = new JsonRenderer(out)
}
