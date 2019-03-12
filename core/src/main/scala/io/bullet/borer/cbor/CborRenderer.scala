/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.cbor

import io.bullet.borer._
import java.nio.charset.StandardCharsets.UTF_8
import java.math.{BigDecimal ⇒ JBigDecimal, BigInteger ⇒ JBigInteger}

/**
  * Encapsulates basic CBOR encoding logic.
  * Has no internal state and can therefore be a singleton object.
  */
private[borer] object CborRenderer extends Receiver[Output] {

  def onNull(out: Output): Output =
    out.writeAsByte(0xF6)

  def onUndefined(out: Output): Output =
    out.writeAsByte(0xF7)

  def onBool(out: Output, value: Boolean): Output =
    out.writeAsByte(if (value) 0xF5 else 0xF4)

  def onInt(out: Output, value: Int): Output =
    onLong(out, value.toLong)

  def onLong(out: Output, value: Long): Output =
    if (value < 0) writeInteger(out, ~value, 0x20) else writeInteger(out, value, 0x00)

  def onOverLong(out: Output, negative: Boolean, value: Long): Output =
    out.writeAsByte(if (negative) 0x3B else 0x1B).writeLong(value)

  def onFloat16(out: Output, value: Float): Output =
    out.writeAsByte(0xF9).writeShort(Float16.floatToShort(value).toShort)

  def onFloat(out: Output, value: Float): Output =
    out.writeAsByte(0xFA).writeInt(java.lang.Float.floatToIntBits(value))

  def onDouble(out: Output, value: Double): Output =
    out.writeAsByte(0xFB).writeLong(java.lang.Double.doubleToLongBits(value))

  def onBigInteger(out: Output, value: JBigInteger): Output =
    value.bitLength match {
      case n if n < 32            ⇒ onInt(out, value.intValue)
      case n if n < 64            ⇒ onLong(out, value.longValue)
      case 64 if value.signum > 0 ⇒ onOverLong(out, negative = false, value.longValue)
      case 64                     ⇒ onOverLong(out, negative = true, ~value.longValue)
      case _ ⇒
        val bytes = value.toByteArray
        val tag   = if (value.signum < 0) { Util.inPlaceNegate(bytes); Tag.NegativeBigNum } else Tag.PositiveBigNum
        onBytes(onTag(out, tag), bytes)
    }

  def onBigDecimal(out: Output, value: JBigDecimal): Output =
    onBigInteger(
      if (value.scale != 0) onInt(onArrayHeader(onTag(out, Tag.DecimalFraction), 2), value.scale) else out,
      value.unscaledValue
    )

  def onBytes[Bytes](out: Output, value: Bytes)(implicit byteAccess: ByteAccess[Bytes]): Output =
    writeInteger(out, byteAccess.sizeOf(value), 0x40).writeBytes(value)

  def onBytesStart(out: Output): Output =
    out.writeAsByte(0x5F)

  def onString(out: Output, value: String): Output =
    onText(out, value getBytes UTF_8)

  def onText[Bytes](out: Output, value: Bytes)(implicit byteAccess: ByteAccess[Bytes]): Output =
    writeInteger(out, byteAccess.sizeOf(value), 0x60).writeBytes(value)

  def onTextStart(out: Output): Output =
    out.writeAsByte(0x7F)

  def onArrayHeader(out: Output, length: Long): Output =
    writeInteger(out, Util.requireNonNegative(length, "length"), 0x80)

  def onArrayStart(out: Output): Output =
    out.writeAsByte(0x9F)

  def onMapHeader(out: Output, length: Long): Output =
    writeInteger(out, Util.requireNonNegative(length, "length"), 0xA0)

  def onMapStart(out: Output): Output =
    out.writeAsByte(0xBF)

  def onBreak(out: Output): Output =
    out.writeAsByte(0xFF)

  def onTag(out: Output, value: Tag): Output =
    writeInteger(out, value.code, 0xC0)

  def onSimpleValue(out: Output, value: Int): Output =
    if (!SimpleValue.isLegal(value)) {
      val msg = s"$value must be in the range ${SimpleValue.legalRange}, but was $value"
      throw Borer.Error.InvalidCborData(out, msg)
    } else writeInteger(out, value.toLong, 0xE0)

  def onEndOfInput(out: Output): Output = out // no actual action here

  def target = this

  def copy = this

  private def writeInteger(out: Output, value: Long, majorType: Int): Output = {
    var v = value
    (if (v > 23) {
       if (v >> 8 != 0) {
         (if (v >> 16 != 0) {
            (if (v >> 32 != 0) {
               out
                 .writeAsByte(0x1B + majorType)
                 .writeInt((v >> 32).toInt)
             } else out.writeAsByte(0x1A + majorType))
              .writeShort((v >> 16).toShort)
          } else out.writeAsByte(0x19 + majorType))
           .writeByte((v >> 8).toByte)
       } else out.writeAsByte(0x18 + majorType)
     } else { v += majorType; out })
      .writeByte(v.toByte)
  }
}
