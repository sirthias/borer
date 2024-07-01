/*
 * Copyright (c) 2019-2024 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.input

import java.nio.{ByteBuffer, ByteOrder}
import io.bullet.borer.{ByteAccess, Input}

trait FromByteBufferInput:

  given FromByteBufferProvider: Input.Provider[ByteBuffer] with
    type Bytes = Array[Byte]
    def byteAccess: ByteAccess.ForByteArray.type     = ByteAccess.ForByteArray
    def apply(value: ByteBuffer): Input[Array[Byte]] = fromByteBuffer(value)

  def fromByteBuffer(value: ByteBuffer): Input[Array[Byte]] =
    if (value.order() != ByteOrder.BIG_ENDIAN)
      throw new IllegalArgumentException(
        """borer requires all input ByteBuffers to have BIG ENDIAN byte order for correct parsing,
          |but the given one is configured as LITTLE ENDIAN.
          |Please set `ByteOrder.BIG_ENDIAN` before parsing!
          |Note that you can switch the byte order back to LITTLE ENDIAN after parsing if needed.""".stripMargin)
    new FromByteBuffer(value)

  final private class FromByteBuffer(buffer: ByteBuffer) extends Input[Array[Byte]]:

    def cursor: Long = buffer.position().toLong

    def unread(numberOfBytes: Int): this.type =
      buffer.position(buffer.position() - numberOfBytes)
      this

    def readByte(): Byte = buffer.get()

    def readBytePadded(pp: Input.PaddingProvider[Array[Byte]]): Byte =
      if (buffer.hasRemaining) readByte()
      else pp.padByte()

    def readDoubleByteBigEndian(): Char = buffer.getChar

    def readDoubleByteBigEndianPadded(pp: Input.PaddingProvider[Array[Byte]]): Char =
      val remaining = buffer.remaining
      if (remaining >= 2) readDoubleByteBigEndian()
      else pp.padDoubleByte(remaining)

    def readQuadByteBigEndian(): Int = buffer.getInt()

    def readQuadByteBigEndianPadded(pp: Input.PaddingProvider[Array[Byte]]): Int =
      val remaining = buffer.remaining
      if (remaining >= 4) readQuadByteBigEndian()
      else pp.padQuadByte(remaining)

    def readOctaByteBigEndian(): Long = buffer.getLong()

    def readOctaByteBigEndianPadded(pp: Input.PaddingProvider[Array[Byte]]): Long =
      val remaining = buffer.remaining
      if (remaining >= 8) readOctaByteBigEndian()
      else pp.padOctaByte(remaining)

    def readBytes(length: Long, pp: Input.PaddingProvider[Array[Byte]]): Array[Byte] =
      val remaining = buffer.remaining.toLong
      val len       = math.min(remaining, length).toInt
      val bytes =
        if (len > 0)
          val bytes = new Array[Byte](len)
          buffer.get(bytes, 0, len)
          bytes
        else ByteAccess.ForByteArray.empty
      if (length <= remaining) bytes
      else pp.padBytes(bytes, length - remaining)
