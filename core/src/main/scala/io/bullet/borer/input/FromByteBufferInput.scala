/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.input

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

import io.bullet.borer.{ByteAccess, Input}
import io.bullet.borer.Input.Provider

trait FromByteBufferInput {

  implicit object ByteBufferProvider extends Provider[ByteBuffer] {
    type Bytes = Array[Byte]
    type In    = FromByteBuffer
    def byteAccess               = ByteAccess.ForByteArray
    def apply(value: ByteBuffer) = new FromByteBuffer(value)
  }

  final class FromByteBuffer(buffer: ByteBuffer) extends Input[Array[Byte]] {

    // the number of bytes we've already read beyond the limit of the underlying buffer
    private[this] var paddedCount = 0

    def cursor: Long = (buffer.position() + paddedCount).toLong

    def moveCursor(offset: Int): this.type = {
      val targetPos = buffer.position() + paddedCount + offset
      val limit     = buffer.limit()
      paddedCount = if (targetPos <= limit) {
        buffer.position(targetPos)
        0
      } else {
        buffer.position(limit)
        targetPos - limit
      }
      this
    }

    def prepareRead(length: Long): Boolean = length <= buffer.remaining

    def readByte(): Byte = buffer.get()

    def readBytePadded(pp: Input.PaddingProvider[Array[Byte]]): Byte =
      if (buffer.hasRemaining) readByte()
      else pp.padByte()

    def readDoubleByteBigEndian(): Char = buffer.getChar

    def readDoubleByteBigEndianPadded(pp: Input.PaddingProvider[Array[Byte]]): Char = {
      val remaining = buffer.remaining
      if (remaining >= 2) readDoubleByteBigEndian()
      else pp.padDoubleByte(remaining)
    }

    def readQuadByteBigEndian(): Int = buffer.getInt()

    def readQuadByteBigEndianPadded(pp: Input.PaddingProvider[Array[Byte]]): Int = {
      val remaining = buffer.remaining
      if (remaining >= 4) readQuadByteBigEndian()
      else pp.padQuadByte(remaining)
    }

    def readOctaByteBigEndian(): Long = buffer.getLong()

    def readOctaByteBigEndianPadded(pp: Input.PaddingProvider[Array[Byte]]): Long = {
      val remaining = buffer.remaining
      if (remaining >= 8) readOctaByteBigEndian()
      else pp.padOctaByte(remaining)
    }

    def readBytes(length: Long, pp: Input.PaddingProvider[Array[Byte]]): Array[Byte] = {
      val remaining = buffer.remaining.toLong
      val len       = math.min(remaining, length).toInt
      val bytes =
        if (len > 0) {
          val bytes = new Array[Byte](len)
          buffer.get(bytes, 0, len)
          bytes
        } else ByteAccess.ForByteArray.empty
      if (length <= remaining) bytes
      else pp.padBytes(bytes, length - remaining)
    }

    def precedingBytesAsAsciiString(length: Int): String = {
      val limit = buffer.limit()
      val pos   = buffer.position()
      moveCursor(-length)
      buffer.limit(pos)
      val result = StandardCharsets.ISO_8859_1.decode(buffer).toString
      buffer.limit(limit)
      result
    }
  }
}
