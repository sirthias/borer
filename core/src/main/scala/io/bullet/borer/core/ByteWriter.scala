/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.core

import Util.requireNonNegative

/**
  * Encapsulates basic CBOR encoding logic.
  * State-less and immutable.
  */
final class ByteWriter[Bytes](config: Writer.Config)(implicit byteAccess: ByteAccess[Bytes])
    extends Receiver[Output[Bytes], Bytes] {

  private type Out = Output[Bytes] // brevity alias

  def onNull(out: Out): Out =
    out.writeByte(0xF6.toByte)

  def onUndefined(out: Out): Out =
    out.writeByte(0xF7.toByte)

  def onBool(out: Out, value: Boolean): Out =
    out.writeByte(if (value) 0xF5.toByte else 0xF4.toByte)

  def onInt(out: Out, value: Int): Out =
    onLong(out, value.toLong)

  def onLong(out: Out, value: Long): Out =
    if (value < 0) writeInteger(out, ~value, 0x20) else writeInteger(out, value, 0x00)

  def onPosOverLong(out: Out, value: Long): Out =
    out.writeByte(0x1B.toByte).writeLong(value)

  def onNegOverLong(out: Out, value: Long): Out =
    out.writeByte(0x3B.toByte).writeLong(value)

  def onFloat16(out: Out, value: Float): Out =
    out.writeByte(0xF9.toByte).writeShort(Float16.floatToShort(value).toShort)

  def onFloat(out: Out, value: Float): Out =
    if (config.dontCompressFloatingPointValues || !Util.canBeRepresentedAsFloat16(value)) {
      out.writeByte(0xFA.toByte).writeInt(java.lang.Float.floatToIntBits(value))
    } else onFloat16(out, value)

  def onDouble(out: Out, value: Double): Out =
    if (config.dontCompressFloatingPointValues || !Util.canBeRepresentedAsFloat(value)) {
      out.writeByte(0xFB.toByte).writeLong(java.lang.Double.doubleToLongBits(value))
    } else onFloat(out, value.toFloat)

  def onBytes(out: Out, value: Bytes): Out =
    writeInteger(out, byteAccess.sizeOf(value), 0x40).writeBytes(value)

  def onByteArray(out: Out, value: Array[Byte]): Out =
    writeInteger(out, value.length.toLong, 0x40).writeBytes(value)

  def onBytesStart(out: Out): Out =
    out.writeByte(0x5F.toByte)

  def onText(out: Out, value: Bytes): Out =
    writeInteger(out, byteAccess.sizeOf(value), 0x60).writeBytes(value)

  def onTextByteArray(out: Out, value: Array[Byte]): Out =
    writeInteger(out, value.length.toLong, 0x60).writeBytes(value)

  def onTextStart(out: Out): Out =
    out.writeByte(0x7F.toByte)

  def onArrayHeader(out: Out, length: Long): Out =
    writeInteger(out, requireNonNegative(length, "length"), 0x80)

  def onArrayStart(out: Out): Out =
    out.writeByte(0x9F.toByte)

  def onMapHeader(out: Out, length: Long): Out =
    writeInteger(out, requireNonNegative(length, "length"), 0xA0)

  def onMapStart(out: Out): Out =
    out.writeByte(0xBF.toByte)

  def onBreak(out: Out): Out =
    out.writeByte(0xFF.toByte)

  def onTag(out: Out, value: Tag): Out =
    writeInteger(out, value.code, 0xC0)

  def onSimpleValue(out: Out, value: Int): Out =
    if (!SimpleValue.isLegal(value)) {
      val msg = s"$value must be in the range ${SimpleValue.legalRange}, but was $value"
      throw new Cbor.Error.InvalidCborData(out, msg)
    } else writeInteger(out, value.toLong, 0xE0)

  def onEndOfInput(out: Out): Out = out // no actual action here

  def target = this

  def copy = this

  private def writeInteger(out: Out, value: Long, majorType: Int): Out = {
    var v = value
    (if (v > 23) {
       if (v >> 8 != 0) {
         (if (v >> 16 != 0) {
            (if (v >> 32 != 0) {
               out
                 .writeByte((0x1B + majorType).toByte)
                 .writeInt((v >> 32).toInt)
             } else out.writeByte((0x1A + majorType).toByte))
              .writeShort((v >> 16).toShort)
          } else out.writeByte((0x19 + majorType).toByte))
           .writeByte((v >> 8).toByte)
       } else out.writeByte((0x18 + majorType).toByte)
     } else { v += majorType; out })
      .writeByte(v.toByte)
  }
}
