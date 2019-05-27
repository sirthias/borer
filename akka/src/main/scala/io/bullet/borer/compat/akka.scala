/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.compat

import java.nio.charset.StandardCharsets
import java.nio.ByteBuffer

import io.bullet.borer.{ByteAccess, _}
import _root_.akka.util.ByteString

object akka {

  /**
    * [[ByteAccess]] for [[ByteString]].
    */
  implicit final object ByteStringByteAccess extends ByteAccess[ByteString] {

    type Out = ByteStringOutput

    def isEmpty(bytes: ByteString): Boolean = bytes.isEmpty

    def sizeOf(bytes: ByteString): Long = bytes.length.toLong

    def fromByteArray(byteArray: Array[Byte]): ByteString = ByteString(byteArray)

    def toByteArray(bytes: ByteString): Array[Byte] = bytes.toArray

    def inputFrom(bytes: ByteString) = new FromByteString(bytes)

    def concat(a: ByteString, b: ByteString) =
      if (a.nonEmpty) {
        if (b.nonEmpty) {
          val len = a.length + b.length
          if (len >= 0) {
            a ++ b
          } else sys.error("Cannot concatenate two ByteStrings with a total size > 2^31 bytes")
        } else a
      } else b

    def copyToByteArray(bytes: ByteString, byteArray: Array[Byte], startIndex: Int): ByteString = {
      val len = byteArray.length - startIndex
      bytes.copyToArray(byteArray, startIndex)
      if (len < bytes.size) bytes.drop(len) else empty
    }

    def copyToByteBuffer(bytes: ByteString, byteBuffer: ByteBuffer): ByteString = {
      val copied = bytes.copyToBuffer(byteBuffer)
      if (copied < bytes.size) bytes.drop(copied) else empty
    }

    def convert[B](value: B)(implicit byteAccess: ByteAccess[B]) =
      value match {
        case x: ByteString => x
        case x             => fromByteArray(byteAccess.toByteArray(x))
      }

    def empty = ByteString.empty
  }

  /**
    * Encoding and Decoding for [[ByteString]].
    */
  implicit val ByteStringCodec = Codec[ByteString](_ writeBytes _, _.readBytes())

  /**
    * [[Input]] around [[ByteString]].
    */
  implicit final object ByteStringWrapper extends Input.Wrapper[ByteString] {
    type In = FromByteString
    def apply(value: ByteString) = new FromByteString(value)
  }

  final class FromByteString(byteString: ByteString) extends Input {
    type Bytes    = ByteString
    type Position = Input.Position

    protected var _cursor: Int = _

    @inline def cursor: Long = _cursor.toLong
    @inline def byteAccess   = ByteStringByteAccess

    def position(cursor: Long): Position = Input.Position(this, cursor)

    @inline def prepareRead(length: Long): Boolean = _cursor + length <= byteString.length

    def readByte(): Byte = {
      val c = _cursor
      _cursor = c + 1
      byteString(c)
    }

    @inline def readByteOrFF(): Byte = {
      def readPadded(): Byte = {
        _cursor += 1
        -1
      }
      if (_cursor < byteString.length) readByte() else readPadded()
    }

    def readDoubleByteBigEndian(): Char = {
      val c = _cursor
      _cursor = c + 2
      ((byteString(c) << 8) | byteString(c + 1) & 0xFF).toChar
    }

    @inline def readDoubleByteBigEndianPaddedFF(): Char = {
      def readPadded(): Char = {
        val c = _cursor
        _cursor = c + 2
        byteString.length - c match {
          case 1 => (byteString(c) << 8 | 0xFF).toChar
          case _ => '\uffff'
        }
      }
      if (_cursor < byteString.length - 1) readDoubleByteBigEndian() else readPadded()
    }

    def readQuadByteBigEndian(): Int = {
      val c = _cursor
      _cursor = c + 4
      byteString(c) << 24 |
      (byteString(c + 1) & 0xFF) << 16 |
      (byteString(c + 2) & 0xFF) << 8 |
      byteString(c + 3) & 0xFF
    }

    @inline def readQuadByteBigEndianPaddedFF(): Int = {
      def readPadded(): Int = {
        val c = _cursor
        val res = byteString.length - c match {
          case 1 => readByte() << 24 | 0xFFFFFF
          case 2 => readDoubleByteBigEndian() << 16 | 0xFFFF
          case 3 => readDoubleByteBigEndian() << 16 | (readByte() & 0xFF) << 8 | 0xFF
          case _ => -1
        }
        _cursor = c + 4
        res
      }
      if (_cursor < byteString.length - 3) readQuadByteBigEndian() else readPadded()
    }

    def readOctaByteBigEndian(): Long = {
      val c = _cursor
      _cursor = c + 8
      byteString(c).toLong << 56 |
      (byteString(c + 1) & 0XFFL) << 48 |
      (byteString(c + 2) & 0XFFL) << 40 |
      (byteString(c + 3) & 0XFFL) << 32 |
      (byteString(c + 4) & 0XFFL) << 24 |
      (byteString(c + 5) & 0XFFL) << 16 |
      (byteString(c + 6) & 0XFFL) << 8 |
      byteString(c + 7) & 0XFFL
    }

    @inline def readOctaByteBigEndianPaddedFF(): Long = {
      def readPadded(): Long = {
        val c = _cursor
        val res = byteString.length - c match {
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
      if (_cursor < byteString.length - 7) readOctaByteBigEndian() else readPadded()
    }

    @inline def readBytes(length: Long): Bytes =
      if (length > 0) {
        val len = length.toInt
        val end = _cursor + len
        if (len == length && end >= 0) {
          val c = _cursor
          _cursor = end
          byteString.slice(c, end)
        } else throw new Borer.Error.Overflow(position(cursor), "ByteString input is limited to size 2GB")
      } else ByteString.empty

    @inline def moveCursor(offset: Int): this.type = {
      _cursor += offset
      this
    }

    @inline def precedingBytesAsAsciiString(length: Int): String =
      byteString.slice(_cursor - length, _cursor).decodeString(StandardCharsets.ISO_8859_1)
  }

  implicit object ByteStringOutputProvider extends Output.Provider[ByteString] {
    type Out = ByteStringOutput
    def apply(bufferSize: Int) = new ByteStringOutput
  }

  /**
    * Mutable [[Output]] implementation for serializing to [[ByteString]].
    */
  final class ByteStringOutput extends Output {
    private[this] var builder = ByteString.newBuilder

    type Self   = ByteStringOutput
    type Result = ByteString

    def cursor: Int = builder.length

    def writeByte(byte: Byte): this.type = {
      builder += byte
      this
    }

    def writeBytes(a: Byte, b: Byte): this.type = {
      builder += a
      builder += b
      this
    }

    def writeBytes(a: Byte, b: Byte, c: Byte): this.type = {
      builder += a
      builder += b
      builder += c
      this
    }

    def writeBytes(a: Byte, b: Byte, c: Byte, d: Byte): this.type = {
      builder += a
      builder += b
      builder += c
      builder += d
      this
    }

    def writeBytes[Bytes: ByteAccess](bytes: Bytes): this.type = {
      builder ++= ByteStringByteAccess.convert(bytes)
      this
    }

    def result(): ByteString = builder.result()
  }
}
