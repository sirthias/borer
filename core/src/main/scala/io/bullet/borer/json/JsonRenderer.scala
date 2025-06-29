/*
 * Copyright (c) 2019-2024 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.json

import io.bullet.borer.*
import io.bullet.borer.internal.{Renderer, Util}

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
final private[borer] class JsonRenderer(var out: Output, indent: Int) extends Renderer:

  private[this] var level: Int             = _ // valid range: 0 - 63
  private[this] var levelType: Long        = _ // keeps the type of each level as a bit map: 0 -> Array, 1 -> Map
  private[this] var levelCount: Long       = _ // for each level: last bit of element count
  private[this] var sepRequired: Boolean   = _ // whether a separator is required before the next element
  private[this] var currentIndent: Int     = _ // the number of space chars to indent with at the current level
  private[this] var indentPending: Boolean = _ // true if the next array or map element must be prefixed with an indent

  def onNull(): Unit =
    if (isNotMapKey)
      out = out.writeSep().writeAsBytes('n', 'u', 'l', 'l').count()
    else out.failCannotBeMapKey("null")

  def onUndefined(): Unit =
    out.failUnsupported("the `undefined` value")

  def onBoolean(value: Boolean): Unit =
    if (isNotMapKey)
      var o = out.writeSep()
      o =
        if (value) o.writeAsBytes('t', 'r', 'u', 'e')
        else o.writeAsByte('f').writeAsBytes('a', 'l', 's', 'e')
      out = o.count()
    else out.failCannotBeMapKey("boolean values")

  def onInt(value: Int): Unit =
    onLong(value.toLong)

  def onLong(value: Long): Unit =
    if (isNotMapKey)
      out = out.writeSep().writeLongStr(value).count()
    else out.failCannotBeMapKey("integer values")

  def onOverLong(negative: Boolean, value: Long): Unit =
    if (isNotMapKey)
      var o = out.writeSep()
      o = if (negative)
        val v = value + 1
        if (v == 0) o.writeStringAsAsciiBytes("-18446744073709551616")
        else o.writeAsByte('-').writeOverLong(v)
      else o.writeOverLong(value)
      out = o.count()
    else out.failCannotBeMapKey("an Overlong")

  def onFloat16(value: Float): Unit =
    out.failUnsupported("Float16 values")

  def onFloat(value: Float): Unit =
    if (isNotMapKey)
      if (!value.isNaN)
        if (!value.isInfinity)
          out = out.writeSep().writeStringAsAsciiBytes(Util.floatToString(value)).count()
        else out.failUnsupported("`Infinity` floating point values")
      else out.failUnsupported("`NaN` floating point values")
    else out.failCannotBeMapKey("floating point values")

  def onDouble(value: Double): Unit =
    if (isNotMapKey)
      if (!value.isNaN)
        if (!value.isInfinity)
          out = out.writeSep().writeStringAsAsciiBytes(Util.doubleToString(value)).count()
        else out.failUnsupported("`Infinity` floating point values")
      else out.failUnsupported("`NaN` floating point values")
    else out.failCannotBeMapKey("floating point values")

  def onNumberString(value: String): Unit =
    if (isNotMapKey)
      out = out.writeSep().writeStringAsAsciiBytes(value).count()
    else out.failCannotBeMapKey("number strings")

  def onBytes[Bytes: ByteAccess](value: Bytes): Unit =
    out.failUnsupported("byte strings")

  def onBytesStart(): Unit =
    out.failUnsupported("byte string streams")

  def onString(value: String): Unit =
    @tailrec def rec(out: Output, ix: Int): Output =
      if (ix < value.length)
        var index  = ix
        val newOut =
          value.charAt(ix) match
            case '"'            => out.writeEscaped('"')
            case '\\'           => out.writeEscaped('\\')
            case c if c >= 0x20 => // we re-encode the character (or surrogate pair) from UTF-16 to UTF-8 right here
              if (c > 0x7F)
                var codePoint = c.toInt
                (if (codePoint > 0x7FF) {
                   (if (0xD800 <= codePoint && codePoint < 0xE000) { // UTF-16 high surrogate (i.e. first of pair)
                      if (codePoint < 0xDC00) {
                        index += 1
                        if (index < value.length) {
                          codePoint = Character.toCodePoint(c, value.charAt(index))
                          out.writeBytes((0xF0 | (codePoint >> 18)).toByte, (0x80 | ((codePoint >> 12) & 0x3F)).toByte)
                        } else out.failValidation("Truncated UTF-16 surrogate pair at end of string")
                      } else out.failInvalidSurrogatePair(ix)
                    } else out.writeAsByte(0xE0 | (codePoint >> 12))) // 3-byte UTF-8 codepoint
                     .writeAsByte(0x80 | ((codePoint >> 6) & 0x3F))
                 } else out.writeAsByte(0xC0 | (codePoint >> 6))) // 2-byte UTF-8 codepoint
                  .writeAsByte(0x80 | (codePoint & 0x3F))
              else out.writeAsByte(c)

            case '\b' => out.writeEscaped('b')
            case '\f' => out.writeEscaped('f')
            case '\n' => out.writeEscaped('n')
            case '\r' => out.writeEscaped('r')
            case '\t' => out.writeEscaped('t')
            case c    => out.writeUnicodeLiteral(c)
        rec(newOut, index + 1)
      else out

    out = rec(out.writeSep().writeAsByte('"'), 0).writeAsByte('"').count()

  def onChars(buffer: Array[Char], length: Int): Unit =
    @tailrec def rec(out: Output, ix: Int): Output =
      if (ix < length)
        var index  = ix
        val newOut =
          buffer(ix) match
            case '"'            => out.writeEscaped('"')
            case '\\'           => out.writeEscaped('\\')
            case c if c >= 0x20 => // we re-encode the character (or surrogate pair) from UTF-16 to UTF-8 right here
              if (c > 0x7F)
                var codePoint = c.toInt
                (if (codePoint > 0x7FF) {
                   (if (0xD800 <= codePoint && codePoint < 0xE000) { // UTF-16 high surrogate (i.e. first of pair)
                      if (codePoint < 0xDC00) {
                        index += 1
                        if (index < length) {
                          codePoint = Character.toCodePoint(c, buffer(index))
                          out.writeBytes((0xF0 | (codePoint >> 18)).toByte, (0x80 | ((codePoint >> 12) & 0x3F)).toByte)
                        } else out.failValidation("Truncated UTF-16 surrogate pair at end of string")
                      } else out.failInvalidSurrogatePair(ix)
                    } else out.writeAsByte(0xE0 | (codePoint >> 12))) // 3-byte UTF-8 codepoint
                     .writeAsByte(0x80 | ((codePoint >> 6) & 0x3F))
                 } else out.writeAsByte(0xC0 | (codePoint >> 6))) // 2-byte UTF-8 codepoint
                  .writeAsByte(0x80 | (codePoint & 0x3F))
              else out.writeAsByte(c)

            case '\b' => out.writeEscaped('b')
            case '\f' => out.writeEscaped('f')
            case '\n' => out.writeEscaped('n')
            case '\r' => out.writeEscaped('r')
            case '\t' => out.writeEscaped('t')
            case c    => out.writeUnicodeLiteral(c)
        rec(newOut, index + 1)
      else out

    out = rec(out.writeSep().writeAsByte('"'), 0).writeAsByte('"').count()

  def onText[Bytes: ByteAccess](value: Bytes): Unit =
    out.failUnsupported("text byte strings")

  def onTextStart(): Unit =
    out.failUnsupported("text byte string streams")

  def onArrayHeader(length: Long): Unit =
    out.failUnsupported("definite-length arrays")

  def onArrayStart(): Unit =
    if (isNotMapKey)
      val o = out.writeSep()
      level += 1
      if (level < 64)
        levelType <<= 1
        levelCount <<= 1
        sepRequired = false
      else o.failUnsupported("more than 64 JSON Array/Object nesting levels")
      currentIndent += indent
      if (currentIndent > 0) o.writeAndMarkIndentPending('[') else o.writeAsByte('[')
    else out.failCannotBeMapKey("arrays")

  def onMapHeader(length: Long): Unit =
    out.failUnsupported("definite-length maps")

  def onMapStart(): Unit =
    if (isNotMapKey)
      val o = out.writeSep()
      level += 1
      if (level < 64)
        levelType = (levelType << 1) | 1
        levelCount <<= 1
        sepRequired = false
      else o.failUnsupported("more than 64 JSON Array/Object nesting levels")
      currentIndent += indent
      if (currentIndent > 0) o.writeAndMarkIndentPending('{') else o.writeAsByte('{')
    else out.failCannotBeMapKey("maps")

  def onBreak(): Unit =
    val c = if ((levelType & 1) == 0) ']' else '}'
    if (level > 0)
      level -= 1
      levelType >>>= 1
      levelCount >>>= 1
      currentIndent -= indent
    else out.failValidation("Received BREAK without corresponding ArrayStart or MapStart")
    val o = if (indent > 0 && !indentPending) out.writeAsByte('\n').writeIndent() else { indentPending = false; out }
    out = o.writeAsByte(c).count() // level-entering items are only counted when the level is exited, not when entered

  def onTag(value: Tag): Unit         = out.failUnsupported("CBOR tags")
  def onSimpleValue(value: Int): Unit = out.failUnsupported("CBOR Simple Values")
  def onEndOfInput(): Unit            = ()

  private inline def isNotMapKey: Boolean = (levelType & ~levelCount & 1) == 0

  extension (out: Output)

    private def writeEscaped(c: Char): Output = out.writeAsBytes('\\', c)

    private def writeUnicodeLiteral(c: Char): Output =
      out
        .writeAsBytes('\\', 'u', '0', '0')
        .writeBytes(lowerHexDigit(c.toInt >> 4).toByte, lowerHexDigit(c.toInt).toByte)

    private def writeSep(): Output =
      if (sepRequired) {
        if (indent == 0) {
          out.writeByte(
            // if ((levelType & levelCount & 1) == 0) ',' else ':'
            (','.toInt + ((':'.toInt - ','.toInt) & ~((levelType.toInt & levelCount.toInt & 1) - 1))).toByte
          )
        } else writeSepIndented()
      } else if (indent > 0 && indentPending) writePendingIndent()
      else out

    private def writeSepIndented(): Output =
      if ((levelType & levelCount & 1) == 0) {
        out.writeAsBytes(',', '\n').writeIndent()
      } else out.writeAsBytes(':', ' ')

    private def writeAndMarkIndentPending(c: Char): Output =
      indentPending = true
      out.writeAsByte(c)

    private def writePendingIndent(): Output =
      indentPending = false
      out.writeAsByte('\n').writeIndent()

    private def writeIndent(): Output =
      var o: Output = out
      var ci        = currentIndent
      while (ci >= 4) {
        o = o.writeAsBytes(' ', ' ', ' ', ' ')
        ci -= 4
      }
      if (ci >= 2) {
        o = o.writeAsBytes(' ', ' ')
        ci -= 2
      }
      if (ci > 0) o.writeAsByte(' ') else o

    private def writeOverLong(v: Long) =
      val q = (v >>> 1) / 5           // value / 10
      val r = v - (q << 3) - (q << 1) // value - 10*q
      out.writeLongStr(q).writeAsByte('0' + r.toInt)

    private inline def count(): Output =
      levelCount ^= 1
      sepRequired = true
      out

    private def writeLongStr(value: Long): Output =
      if (value != 0)
        if (value != Long.MinValue)
          inline def div10(i: Int) = i * 52429 >>> 19 // 52429 * l / 524288 = l * 0.10000038146972656

          // for small numbers we can use the "fast-path"
          def phase2(i: Int): Output =
            val q      = div10(i)
            val r      = i - q * 10
            val newOut =
              if (q != 0) phase2(q)
              else if (value < 0) out.writeAsByte('-')
              else out
            newOut.writeAsByte('0' + r)

          // for large numbers we bite the bullet of performing one division every two digits
          def phase1(l: Long): Output =
            if (l > 65535L)
              val q  = l / 100
              val r  = (l - q * 100).toInt
              val rq = div10(r)
              phase1(q).writeBytes(('0' + rq).toByte, ('0' + r - rq * 10).toByte)
            else phase2(l.toInt)

          phase1(math.abs(value))
        else out.writeStringAsAsciiBytes("-9223372036854775808")
      else out.writeAsByte('0')

    private def failUnsupported(what: String): Nothing =
      throw new Borer.Error.Unsupported(out, s"The JSON renderer doesn't support $what")

    private def failCannotBeMapKey(what: String): Nothing =
      throw new Borer.Error.ValidationFailure(out, s"JSON does not support $what as a map key")

    private def failInvalidSurrogatePair(ix: Int) =
      out.failValidation(s"Invalid UTF-16 surrogate pair at string index $ix")

    private def failValidation(msg: String): Nothing =
      throw new Borer.Error.ValidationFailure(out, msg)

  end extension

  // fast branchless implementation returning the lower-case hex digit corresponding to the last 4 bits of the given Int
  private inline def lowerHexDigit(int: Int): Int =
    val i = int & 0x0F
    48 + i + (39 & ((9 - i) >> 31))

object JsonRenderer extends ((Output, Borer.EncodingConfig) => JsonRenderer):
  def apply(out: Output, config: Borer.EncodingConfig) =
    new JsonRenderer(out, config.asInstanceOf[Json.EncodingConfig].indent)
