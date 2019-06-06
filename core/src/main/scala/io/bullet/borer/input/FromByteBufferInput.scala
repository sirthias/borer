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

import io.bullet.borer.{Borer, ByteAccess, Input}
import io.bullet.borer.Input.{Position, Wrapper}

trait FromByteBufferInput {

  implicit object ByteBufferWrapper extends Wrapper[ByteBuffer] {
    type In = FromByteBuffer
    def apply(value: ByteBuffer) = new FromByteBuffer(value)
  }

  final class FromByteBuffer(buffer: ByteBuffer) extends Input {
    type Bytes    = Array[Byte]
    type Position = Input.Position

    // the number of bytes we've already read beyond the limit of the underlying buffer
    private[this] var paddedCount = 0

    @inline def cursor: Long = (buffer.position() + paddedCount).toLong
    @inline def byteAccess   = ByteAccess.ForByteArray

    @inline def resetTo(cursor: Long) = {
      val intLimit  = buffer.limit()
      val longLimit = intLimit.toLong
      paddedCount = if (cursor <= longLimit) {
        buffer.position(cursor.toInt)
        0
      } else {
        buffer.position(intLimit)
        val x = cursor - longLimit
        if (x > Int.MaxValue) throw new IllegalArgumentException
        x.toInt
      }
      this
    }

    @inline def moveCursor(offset: Int): this.type = {
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

    @inline def releaseBeforeCursor(): this.type = this

    def position(cursor: Long): Position = Position(this, cursor)

    @inline def prepareRead(length: Long): Boolean = length <= buffer.remaining

    def readByte(): Byte = buffer.get()

    @inline def readByteOrFF(): Byte =
      if (!buffer.hasRemaining) {
        paddedCount += 1
        -1
      } else readByte()

    def readDoubleByteBigEndian(): Char = buffer.getChar

    @inline def readDoubleByteBigEndianPaddedFF(): Char = {
      val remaining = buffer.remaining
      if (remaining >= 2) readDoubleByteBigEndian()
      else readDoubleByteBigEndianPaddedFF(remaining)
    }

    def readQuadByteBigEndian(): Int = buffer.getInt()

    @inline def readQuadByteBigEndianPaddedFF(): Int = {
      val remaining = buffer.remaining
      if (remaining >= 4) readQuadByteBigEndian()
      else readQuadByteBigEndianPaddedFF(remaining)
    }

    def readOctaByteBigEndian(): Long = buffer.getLong()

    @inline def readOctaByteBigEndianPaddedFF(): Long = {
      val remaining = buffer.remaining
      if (remaining >= 8) readOctaByteBigEndian()
      else readOctaByteBigEndianPaddedFF(remaining)
    }

    @inline def readBytes(length: Long): Array[Byte] = {
      val len = length.toInt
      if (length == len) {
        if (len > 0) {
          val bytes = new Array[Byte](len)
          buffer.get(bytes, 0, len)
          bytes
        } else ByteAccess.ForByteArray.empty
      } else throw new Borer.Error.Overflow(position(cursor), "Byte-array input is limited to size 2GB")
    }

    @inline def precedingBytesAsAsciiString(length: Int): String = {
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
