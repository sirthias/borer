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
  * Has no internal state and can therefore be a singleton object.
  */
object ByteWriter extends Receiver[Output] {

  def onNull(Output: Output): Output =
    Output.writeByte(0xF6.toByte)

  def onUndefined(Output: Output): Output =
    Output.writeByte(0xF7.toByte)

  def onBool(Output: Output, value: Boolean): Output =
    Output.writeByte(if (value) 0xF5.toByte else 0xF4.toByte)

  def onInt(Output: Output, value: Int): Output =
    onLong(Output, value.toLong)

  def onLong(Output: Output, value: Long): Output =
    if (value < 0) writeInteger(Output, ~value, 0x20) else writeInteger(Output, value, 0x00)

  def onPosOverLong(Output: Output, value: Long): Output =
    Output.writeByte(0x1B.toByte).writeLong(value)

  def onNegOverLong(Output: Output, value: Long): Output =
    Output.writeByte(0x3B.toByte).writeLong(value)

  def onFloat16(Output: Output, value: Float): Output =
    Output.writeByte(0xF9.toByte).writeShort(Float16.floatToShort(value).toShort)

  def onFloat(Output: Output, value: Float): Output =
    Output.writeByte(0xFA.toByte).writeInt(java.lang.Float.floatToIntBits(value))

  def onDouble(Output: Output, value: Double): Output =
    Output.writeByte(0xFB.toByte).writeLong(java.lang.Double.doubleToLongBits(value))

  def onBytes[Bytes](Output: Output, value: Bytes)(implicit byteAccess: ByteAccess[Bytes]): Output =
    writeInteger(Output, byteAccess.sizeOf(value), 0x40).writeBytes(value)

  def onBytesStart(Output: Output): Output =
    Output.writeByte(0x5F.toByte)

  def onText[Bytes](Output: Output, value: Bytes)(implicit byteAccess: ByteAccess[Bytes]): Output =
    writeInteger(Output, byteAccess.sizeOf(value), 0x60).writeBytes(value)

  def onTextStart(Output: Output): Output =
    Output.writeByte(0x7F.toByte)

  def onArrayHeader(Output: Output, length: Long): Output =
    writeInteger(Output, requireNonNegative(length, "length"), 0x80)

  def onArrayStart(Output: Output): Output =
    Output.writeByte(0x9F.toByte)

  def onMapHeader(Output: Output, length: Long): Output =
    writeInteger(Output, requireNonNegative(length, "length"), 0xA0)

  def onMapStart(Output: Output): Output =
    Output.writeByte(0xBF.toByte)

  def onBreak(Output: Output): Output =
    Output.writeByte(0xFF.toByte)

  def onTag(Output: Output, value: Tag): Output =
    writeInteger(Output, value.code, 0xC0)

  def onSimpleValue(Output: Output, value: Int): Output =
    if (!SimpleValue.isLegal(value)) {
      val msg = s"$value must be in the range ${SimpleValue.legalRange}, but was $value"
      throw new Cbor.Error.InvalidCborData(Output, msg)
    } else writeInteger(Output, value.toLong, 0xE0)

  def onEndOfInput(Output: Output): Output = Output // no actual action here

  def target = this

  def copy = this

  private def writeInteger(Output: Output, value: Long, majorType: Int): Output = {
    var v = value
    (if (v > 23) {
       if (v >> 8 != 0) {
         (if (v >> 16 != 0) {
            (if (v >> 32 != 0) {
               Output
                 .writeByte((0x1B + majorType).toByte)
                 .writeInt((v >> 32).toInt)
             } else Output.writeByte((0x1A + majorType).toByte))
              .writeShort((v >> 16).toShort)
          } else Output.writeByte((0x19 + majorType).toByte))
           .writeByte((v >> 8).toByte)
       } else Output.writeByte((0x18 + majorType).toByte)
     } else { v += majorType; Output })
      .writeByte(v.toByte)
  }
}
