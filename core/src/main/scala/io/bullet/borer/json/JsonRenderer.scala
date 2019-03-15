/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.json

import io.bullet.borer._
import java.util
import java.math.{BigDecimal ⇒ JBigDecimal, BigInteger ⇒ JBigInteger}
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
private[borer] final class JsonRenderer extends Receiver[Output] {

  private[this] var level: Int = 0

  // >= 0 if bounded, < 0 if unbounded (the count is then ~x)
  private[this] var _levelCount = new Array[Long](4)

  // if bounded: >= 0 if in array, < 0 if in map (the size is then ~x)
  // if unbounded: 0 -> array, -1 -> map
  private[this] var _levelSize = new Array[Long](4)

  _levelCount(0) = -1 // we treat level 0 as an unbounded array

  @inline private def isLevelMap: Boolean = _levelSize(level) < 0

  def onNull(out: Output): Output =
    count(sep(out).writeAsByte('n').writeAsByte('u').writeAsByte('l').writeAsByte('l'))

  def onUndefined(out: Output): Output =
    unsupported(out, "the `undefined` value")

  def onBool(out: Output, value: Boolean): Output =
    count {
      if (value) sep(out).writeAsByte('t').writeAsByte('r').writeAsByte('u').writeAsByte('e')
      else sep(out).writeAsByte('f').writeAsByte('a').writeAsByte('l').writeAsByte('s').writeAsByte('e')
    }

  def onInt(out: Output, value: Int): Output =
    onLong(out, value.toLong)

  def onLong(out: Output, value: Long): Output =
    count(writeLong(sep(out), value))

  def onOverLong(out: Output, negative: Boolean, value: Long): Output =
    count {
      if (negative) writeOverLong(sep(out).writeAsByte('-'), ~value)
      else writeOverLong(sep(out), value)
    }

  def onFloat16(out: Output, value: Float): Output =
    unsupported(out, "Float16 values")

  def onFloat(out: Output, value: Float): Output =
    onDouble(out, value.toDouble)

  def onDouble(out: Output, value: Double): Output =
    if (!value.isNaN) {
      if (!value.isInfinity) {
        count(sep(out).writeStringAsAsciiBytes(Util.doubleToString(value)))
      } else unsupported(out, "`Infinity` floating point values")
    } else unsupported(out, "`NaN` floating point values")

  def onBigInteger(out: Output, value: JBigInteger): Output =
    out.writeStringAsAsciiBytes(value.toString)

  def onBigDecimal(out: Output, value: JBigDecimal): Output =
    out.writeStringAsAsciiBytes(value.toString)

  def onBytes[Bytes](out: Output, value: Bytes)(implicit byteAccess: ByteAccess[Bytes]): Output =
    unsupported(out, "byte strings")

  def onBytesStart(out: Output): Output =
    unsupported(out, "byte string streams")

  def onString(out: Output, value: String): Output = {
    @tailrec def rec(out: Output, ix: Int): Output =
      if (ix < value.length) {
        def escaped(c: Char)  = out.writeAsByte('\\').writeAsByte(c)
        def fail(msg: String) = throw Borer.Error.ValidationFailure(out, msg)

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
                          out.writeAsByte(0xF0 | (codePoint >> 18)).writeAsByte(0x80 | ((codePoint >> 12) & 0x3F))
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
              .writeStringAsAsciiBytes("\\u00")
              .writeAsByte(lowerHexDigit(c.toInt >> 4))
              .writeAsByte(lowerHexDigit(c.toInt))
            rec(newOut, ix + 1)
        }
      } else out

    count(rec(sep(out).writeAsByte('"'), 0).writeAsByte('"'))
  }

  def onText[Bytes](out: Output, value: Bytes)(implicit ba: ByteAccess[Bytes]): Output =
    unsupported(out, "text byte strings")

  def onTextStart(out: Output): Output =
    unsupported(out, "text byte string streams")

  def onArrayHeader(out: Output, length: Long): Output =
    unsupported(out, "definite-length arrays")

  def onArrayStart(out: Output): Output =
    enterLevel(sep(out).writeAsByte('['), count = -1, size = 0)

  def onMapHeader(out: Output, length: Long): Output =
    unsupported(out, "definite-length maps")

  def onMapStart(out: Output): Output =
    enterLevel(sep(out).writeAsByte('{'), count = -1, size = -1)

  def onBreak(out: Output): Output = {
    val c = if (isLevelMap) '}' else ']'
    exitLevel()
    count(out.writeAsByte(c)) // level-entering items are only counted when the level is exited, not when entered
  }

  def onTag(out: Output, value: Tag): Output         = unsupported(out, "CBOR tags")
  def onSimpleValue(out: Output, value: Int): Output = unsupported(out, "CBOR Simple Values")
  def onEndOfInput(out: Output): Output              = out

  def target = this
  def copy   = throw new UnsupportedOperationException

  private def sep(out: Output): Output = {
    val count = _levelCount(level)
    if (count != (count >> 63)) { // short for (count != 0) && (count != -1)
      val size = _levelSize(level)
      out.writeAsByte(if ((size >= 0) || (count & 1) == (count >>> 63)) ',' else ':')
    } else out
  }

  @tailrec private def count(out: Output): Output = {
    val cnt = _levelCount(level)
    if (cnt >= 0) {
      // bounded array or map
      val newCount = cnt + 1
      val rawSize  = _levelSize(level)
      val size     = if (rawSize >= 0) rawSize else ~rawSize
      if (newCount == size) {
        val c = if (isLevelMap) '}' else ']'
        exitLevel()
        count(out.writeAsByte(c)) // level-entering items are only counted when the level is exited, not when entered
      } else {
        _levelCount(level) = newCount
        out
      }
    } else {
      _levelCount(level) = cnt - 1 // unbounded something
      out
    }
  }

  private def enterLevel(out: Output, count: Long, size: Long): Output = {
    val newLevel = level + 1
    if (newLevel == _levelCount.length) {
      val l2     = newLevel << 1
      val newLen = if (l2 >= 0) l2 else Int.MaxValue // overflow protection
      _levelCount = util.Arrays.copyOf(_levelCount, newLen)
      _levelSize = util.Arrays.copyOf(_levelSize, newLen)
    }
    level = newLevel
    _levelCount(newLevel) = count
    _levelSize(newLevel) = size
    out
  }

  @inline private def exitLevel(): Unit = level -= 1

  private def writeOverLong(out: Output, value: Long): Output = {
    val q = (value >>> 1) / 5
    val r = value - (q << 3) - (q << 1)
    writeLong(out, q).writeAsByte('0' + r.toInt)
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

        // for large numbers we bite the bullet of performing one division every two digits
        def phase1(l: Long): Output =
          if (l > 65535L) {
            val q  = l / 100
            val r  = (l - mul100(q)).toInt
            val rq = div10(r)
            phase1(q).writeAsByte('0' + rq).writeAsByte('0' + r - mul10(rq))
          } else phase2(l.toInt)

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

        phase1(math.abs(value))
      } else out.writeStringAsAsciiBytes("-9223372036854775808")
    } else out.writeAsByte('0')

  // fast branchless implementation returning the lower-case hex digit corresponding to the last 4 bits of the given Int
  @inline private def lowerHexDigit(int: Int): Int = {
    val i = int & 0x0F
    48 + i + (39 & ((9 - i) >> 31))
  }

  private def unsupported(out: Output, what: String) =
    throw Borer.Error.InvalidJsonData(out, s"The JSON renderer doesn't support $what")
}
