/*
 * Copyright (c) 2019-2021 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.compat

import java.nio.ByteBuffer

import _root_.scodec.bits.ByteVector
import io.bullet.borer.{ByteAccess, _}

object scodec {

  /**
   * [[ByteAccess]] for [[ByteVector]].
   */
  implicit object ByteVectorByteAccess extends ByteAccess[ByteVector] {

    type Out = ByteVectorOutput

    def isEmpty(bytes: ByteVector): Boolean = bytes.isEmpty

    def sizeOf(bytes: ByteVector): Long = bytes.size

    def fromByteArray(byteArray: Array[Byte]): ByteVector = ByteVector(byteArray)

    def toByteArray(bytes: ByteVector): Array[Byte] = bytes.toArray

    def copyToByteArray(bytes: ByteVector, byteArray: Array[Byte], startIndex: Int): ByteVector = {
      val len = byteArray.length - startIndex
      bytes.copyToArray(byteArray, startIndex)
      if (len < bytes.size) bytes.drop(len.toLong) else empty
    }

    def copyToByteBuffer(bytes: ByteVector, byteBuffer: ByteBuffer): ByteVector = {
      val copied = bytes.copyToBuffer(byteBuffer)
      if (copied < bytes.size) bytes.drop(copied.toLong) else empty
    }

    def concat(a: ByteVector, b: ByteVector) =
      if (a.nonEmpty) {
        if (b.nonEmpty) {
          a ++ b
        } else a
      } else b

    def convert[B](value: B)(implicit byteAccess: ByteAccess[B]) =
      value match {
        case x: ByteVector => x
        case x             => ByteVector(byteAccess.toByteArray(x))
      }

    def empty = ByteVector.empty
  }

  /**
   * Encoding and Decoding for [[ByteVector]].
   */
  implicit val ByteVectorCodec = Codec[ByteVector](_ writeBytes _, _.readBytes())

  /**
   * [[Input]] around [[ByteVector]].
   */
  implicit object ByteVectorProvider extends Input.Provider[ByteVector] {
    type Bytes = ByteVector
    type In    = FromByteVector
    def byteAccess               = ByteVectorByteAccess
    def apply(value: ByteVector) = new FromByteVector(value)
  }

  final class FromByteVector(byteVector: ByteVector) extends Input[ByteVector] {
    private[this] var _cursor: Long = _

    def cursor: Long = _cursor

    def unread(numberOfBytes: Int): this.type = {
      _cursor -= numberOfBytes
      this
    }

    def readByte(): Byte = {
      val c = _cursor
      _cursor = c + 1
      byteVector(c)
    }

    def readBytePadded(pp: Input.PaddingProvider[ByteVector]): Byte =
      if (_cursor < byteVector.length) readByte()
      else pp.padByte()

    def readDoubleByteBigEndian(): Char = {
      val c = _cursor
      _cursor = c + 2
      ((byteVector(c) << 8) | byteVector(c + 1) & 0xFF).toChar
    }

    def readDoubleByteBigEndianPadded(pp: Input.PaddingProvider[ByteVector]): Char = {
      val remaining = byteVector.length - _cursor
      if (remaining >= 2) readDoubleByteBigEndian()
      else pp.padDoubleByte(remaining.toInt)
    }

    def readQuadByteBigEndian(): Int = {
      val c = _cursor
      _cursor = c + 4
      byteVector(c) << 24 |
      (byteVector(c + 1) & 0xFF) << 16 |
      (byteVector(c + 2) & 0xFF) << 8 |
      byteVector(c + 3) & 0xFF
    }

    def readQuadByteBigEndianPadded(pp: Input.PaddingProvider[ByteVector]): Int = {
      val remaining = byteVector.length - _cursor
      if (remaining >= 4) readQuadByteBigEndian()
      else pp.padQuadByte(remaining.toInt)
    }

    def readOctaByteBigEndian(): Long = {
      val c = _cursor
      _cursor = c + 8
      byteVector(c).toLong << 56 |
      (byteVector(c + 1) & 0xFFL) << 48 |
      (byteVector(c + 2) & 0xFFL) << 40 |
      (byteVector(c + 3) & 0xFFL) << 32 |
      (byteVector(c + 4) & 0xFFL) << 24 |
      (byteVector(c + 5) & 0xFFL) << 16 |
      (byteVector(c + 6) & 0xFFL) << 8 |
      byteVector(c + 7) & 0xFFL
    }

    def readOctaByteBigEndianPadded(pp: Input.PaddingProvider[ByteVector]): Long = {
      val remaining = byteVector.length - _cursor
      if (remaining >= 8) readOctaByteBigEndian()
      else pp.padOctaByte(remaining.toInt)
    }

    def readBytes(length: Long, pp: Input.PaddingProvider[ByteVector]) = {
      val remaining = byteVector.length - _cursor
      val len       = math.min(remaining, length)
      val bytes =
        if (len > 0) {
          val c = _cursor
          _cursor = c + len
          byteVector.slice(c, _cursor)
        } else ByteVector.empty
      if (length <= remaining) bytes
      else pp.padBytes(bytes, length - remaining)
    }
  }

  implicit object ByteVectorOutputProvider extends Output.ToTypeProvider[ByteVector] {
    type Out = ByteVectorOutput
    def apply(bufferSize: Int, allowBufferCaching: Boolean) = new ByteVectorOutput(bufferSize, allowBufferCaching)
  }

  /**
   * Mutable [[Output]] implementation for serializing to [[ByteVector]].
   */
  final class ByteVectorOutput(bufferSize: Int, allowBufferCaching: Boolean) extends Output {
    // The scodec ByteVector doesn't appear to come with an efficient builder for it,
    // so rather than wrapping each incoming Byte in an extra ByteVector instance we simply
    // write into a ByteArray and only construct a ByteVector instance at the very end
    private[this] val delegate = new Output.ToByteArray(bufferSize, allowBufferCaching)

    type Self   = ByteVectorOutput
    type Result = ByteVector

    def writeByte(byte: Byte)                          = ret(delegate.writeByte(byte))
    def writeBytes(a: Byte, b: Byte)                   = ret(delegate.writeBytes(a, b))
    def writeBytes(a: Byte, b: Byte, c: Byte)          = ret(delegate.writeBytes(a, b, c))
    def writeBytes(a: Byte, b: Byte, c: Byte, d: Byte) = ret(delegate.writeBytes(a, b, c, d))
    def writeBytes[Bytes: ByteAccess](bytes: Bytes)    = ret(delegate.writeBytes(bytes))

    def result() = ByteVector(delegate.result())

    private def ret(x: delegate.Self): Self = this
  }
}
