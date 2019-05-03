/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.compat

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
        case x: ByteVector ⇒ x
        case x             ⇒ ByteVector(byteAccess.toByteArray(x))
      }

    @inline def empty = ByteVector.empty
  }

  /**
    * Encoding and Decoding for [[ByteVector]].
    */
  implicit val ByteVectorCodec = Codec[ByteVector](_ writeBytes _, _.readBytes())

  /**
    * [[InputAccess]] for [[ByteVector]].
    */
  implicit object ByteVectorInputAccess extends InputAccess[ByteVector] {
    type Bytes = ByteVector

    def byteAccess = ByteVectorByteAccess

    @inline def length(input: ByteVector): Long = input.size

    def unsafeByte(input: ByteVector, index: Long): Byte = input(index)

    def doubleByteBigEndian(input: ByteVector, index: Long): Int =
      (input(index) & 0xFF) << 8 |
        (input(index + 1) & 0xFF)

    def quadByteBigEndian(input: ByteVector, index: Long): Int =
      (input(index) & 0xFF) << 24 |
        (input(index + 1) & 0xFF) << 16 |
        (input(index + 2) & 0xFF) << 8 |
        (input(index + 3) & 0xFF)

    def octaByteBigEndian(input: ByteVector, index: Long): Long = {
      (input(index) & 0xffl) << 56 |
      (input(index + 1) & 0xffl) << 48 |
      (input(index + 2) & 0xffl) << 40 |
      (input(index + 3) & 0xffl) << 32 |
      (input(index + 4) & 0xffl) << 24 |
      (input(index + 5) & 0xffl) << 16 |
      (input(index + 6) & 0xffl) << 8 |
      (input(index + 7) & 0xffl)
    }

    def bytes(input: ByteVector, index: Long, length: Long): ByteVector =
      if (length > 0) {
        val end = index + length
        if (end >= 0) input.slice(index, end)
        else throw new Borer.Error.Overflow(Position(input, index), "ByteVector input is limited to 2^63 bytes")
      } else ByteVector.empty
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
        case x: ByteVector ⇒ append(x)
        case x             ⇒ append(byteAccess.toByteArray(x))
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
