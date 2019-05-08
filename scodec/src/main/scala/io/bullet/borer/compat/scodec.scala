/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.compat

import java.nio.charset.StandardCharsets
import java.util

import _root_.scodec.bits.ByteVector
import io.bullet.borer.{ByteAccess, _}

object scodec {

  /**
    * [[ByteAccess]] for [[ByteVector]].
    */
  implicit object ByteVectorByteAccess extends ByteAccess[ByteVector] {

    type Out = ByteVectorOutput

    @inline def newOutput = new ByteVectorOutput

    @inline def sizeOf(bytes: ByteVector): Long = bytes.size

    @inline def fromByteArray(byteArray: Array[Byte]): ByteVector = ByteVector(byteArray)

    @inline def toByteArray(bytes: ByteVector): Array[Byte] = bytes.toArray

    def concat(a: ByteVector, b: ByteVector) =
      if (a.nonEmpty) {
        if (b.nonEmpty) {
          a ++ b
        } else a
      } else b

    @inline def convert[B](value: B)(implicit byteAccess: ByteAccess[B]) =
      value match {
        case x: ByteVector => x
        case x             => ByteVector(byteAccess.toByteArray(x))
      }

    @inline def empty = ByteVector.empty
  }

  /**
    * Encoding and Decoding for [[ByteVector]].
    */
  implicit val ByteVectorCodec = Codec[ByteVector](_ writeBytes _, _.readBytes())

  /**
    * [[Input]] around [[ByteVector]].
    */
  implicit object ByteVectorWrapper extends Input.Wrapper[ByteVector] {
    type In = FromByteVector
    def apply(value: ByteVector) = new FromByteVector(value)
  }

  final class FromByteVector(byteVector: ByteVector) extends Input {
    type Bytes    = ByteVector
    type Position = Input.Position

    protected var _cursor: Long = _

    @inline def cursor: Long = _cursor
    @inline def byteAccess   = ByteVectorByteAccess

    def position(marker: Long): Position = Input.Position(this, marker)

    @inline def prepareRead(length: Long): Boolean = _cursor + length <= byteVector.length

    def readByte(): Byte = {
      val c = _cursor
      _cursor = c + 1
      byteVector(c)
    }

    @inline def readByteOrFF(): Byte = {
      def readPadded(): Byte = {
        _cursor += 1
        -1
      }
      if (_cursor < byteVector.length) readByte() else readPadded()
    }

    def readDoubleByteBigEndian(): Char = {
      val c = _cursor
      _cursor = c + 2
      ((byteVector(c) << 8) | byteVector(c + 1) & 0xFF).toChar
    }

    @inline def readDoubleByteBigEndianPaddedFF(): Char = {
      def readPadded(): Char = {
        val c = _cursor
        _cursor = c + 2
        byteVector.length - c match {
          case 1 => (byteVector(c) << 8 | 0xFF).toChar
          case _ => '\uffff'
        }
      }
      if (_cursor < byteVector.length - 1) readDoubleByteBigEndian() else readPadded()
    }

    def readQuadByteBigEndian(): Int = {
      val c = _cursor
      _cursor = c + 4
      byteVector(c) << 24 |
      (byteVector(c + 1) & 0xFF) << 16 |
      (byteVector(c + 2) & 0xFF) << 8 |
      byteVector(c + 3) & 0xFF
    }

    @inline def readQuadByteBigEndianPaddedFF(): Int = {
      def readPadded(): Int = {
        val c = _cursor
        val res = byteVector.length - c match {
          case 1 => readByte() << 24 | 0xFFFFFF
          case 2 => readDoubleByteBigEndian() << 16 | 0xFFFF
          case 3 => readDoubleByteBigEndian() << 16 | (readByte() & 0xFF) << 8 | 0xFF
          case _ => -1
        }
        _cursor = c + 4
        res
      }
      if (_cursor < byteVector.length - 3) readQuadByteBigEndian() else readPadded()
    }

    def readOctaByteBigEndian(): Long = {
      val c = _cursor
      _cursor = c + 8
      byteVector(c).toLong << 56 |
      (byteVector(c + 1) & 0XFFL) << 48 |
      (byteVector(c + 2) & 0XFFL) << 40 |
      (byteVector(c + 3) & 0XFFL) << 32 |
      (byteVector(c + 4) & 0XFFL) << 24 |
      (byteVector(c + 5) & 0XFFL) << 16 |
      (byteVector(c + 6) & 0XFFL) << 8 |
      byteVector(c + 7) & 0XFFL
    }

    @inline def readOctaByteBigEndianPaddedFF(): Long = {
      def readPadded(): Long = {
        val c = _cursor
        val res = byteVector.length - c match {
          case 1 => readByte().toLong << 56 | 0XFFFFFFFFFFFFFFL
          case 2 => readDoubleByteBigEndian().toLong << 48 | 0XFFFFFFFFFFFFL
          case 3 => readDoubleByteBigEndian().toLong << 48 | (readByte() & 0XFFL) << 40 | 0XFFFFFFFFFFL
          case 4 => readQuadByteBigEndian().toLong << 32 | 0XFFFFFFFFL
          case 5 => readQuadByteBigEndian().toLong << 32 | (readByte() & 0XFFL) << 24 | 0XFFFFFFL
          case 6 => readQuadByteBigEndian().toLong << 32 | (readDoubleByteBigEndian() & 0XFFFFL) << 16 | 0XFFFFL
          case 7 =>
            readQuadByteBigEndian().toLong << 32 | (readDoubleByteBigEndian() & 0XFFFFL) << 16 | (readByte() & 0XFFL) << 8 | 0XFFL
          case _ => -1
        }
        _cursor = c + 8
        res
      }
      if (_cursor < byteVector.length - 7) readOctaByteBigEndian() else readPadded()
    }

    @inline def readBytes(length: Long): Bytes =
      if (length > 0) {
        val end = _cursor + length
        if (end >= 0) {
          val c = _cursor
          _cursor = end
          byteVector.slice(c, end)
        } else throw new Borer.Error.Overflow(position(cursor), "ByteVector input is limited to size 2GB")
      } else ByteVector.empty

    @inline def moveCursor(offset: Int): this.type = {
      _cursor += offset
      this
    }

    @inline def precedingBytesAsAsciiString(length: Int): String =
      new String(byteVector.toArray, StandardCharsets.ISO_8859_1)
  }

  /**
    * Mutable [[Output]] implementation for serializing to [[ByteVector]].
    */
  final class ByteVectorOutput extends Output {
    // The scodec ByteVector doesn't appear to come with an efficient builder for it,
    // so rather than wrapping each incoming Byte in an extra ByteVector instance we simply
    // write into a ByteArray and only construct a ByteVector instance at the very end
    private[this] var buffer       = new Array[Byte](64)
    private[this] var _cursor: Int = _

    type Self   = ByteVectorOutput
    type Result = ByteVector

    @inline def cursor: Int = _cursor

    def writeByte(byte: Byte): this.type = {
      val crs       = _cursor
      val newCursor = crs + 1
      if (newCursor > 0) {
        ensureLength(newCursor)
        buffer(crs) = byte
        _cursor = newCursor
        this
      } else overflow()
    }

    def writeBytes(a: Byte, b: Byte): this.type = {
      val crs       = _cursor
      val newCursor = crs + 2
      if (newCursor > 0) {
        ensureLength(newCursor)
        buffer(crs) = a
        buffer(crs + 1) = b
        _cursor = newCursor
        this
      } else overflow()
    }

    def writeBytes(a: Byte, b: Byte, c: Byte): this.type = {
      val crs       = _cursor
      val newCursor = crs + 3
      if (newCursor > 0) {
        ensureLength(newCursor)
        buffer(crs) = a
        buffer(crs + 1) = b
        buffer(crs + 2) = c
        _cursor = newCursor
        this
      } else overflow()
    }

    def writeBytes(a: Byte, b: Byte, c: Byte, d: Byte): this.type = {
      val crs       = _cursor
      val newCursor = crs + 4
      if (newCursor > 0) {
        ensureLength(newCursor)
        buffer(crs) = a
        buffer(crs + 1) = b
        buffer(crs + 2) = c
        buffer(crs + 3) = d
        _cursor = newCursor
        this
      } else overflow()
    }

    def writeBytes[Bytes](bytes: Bytes)(implicit byteAccess: ByteAccess[Bytes]): this.type =
      bytes match {
        case x: ByteVector => append(x)
        case x             => append(byteAccess.toByteArray(x))
      }

    def append(bytes: ByteVector): this.type = {
      val l = bytes.size
      if (l <= Int.MaxValue) {
        val newCursor = _cursor + l.toInt
        if (newCursor > 0) {
          ensureLength(newCursor)
          bytes.copyToArray(buffer, _cursor)
          _cursor = newCursor
          this
        } else overflow()
      } else overflow()
    }

    def append(bytes: Array[Byte]): this.type = {
      val l         = bytes.length
      val newCursor = _cursor + l
      if (newCursor > 0) {
        ensureLength(newCursor)
        System.arraycopy(bytes, 0, buffer, _cursor, l)
        _cursor = newCursor
        this
      } else overflow()
    }

    @inline def result(): ByteVector = ByteVector.view(buffer, 0, _cursor)

    @inline private def ensureLength(minSize: Int): Unit =
      if (buffer.length < minSize) {
        val newLen = math.max(buffer.length << 1, minSize)
        buffer = util.Arrays.copyOf(buffer, newLen)
      }

    private def overflow() = throw new Borer.Error.Overflow(this, "Cannot output to byte array with > 2^31 bytes")
  }
}
