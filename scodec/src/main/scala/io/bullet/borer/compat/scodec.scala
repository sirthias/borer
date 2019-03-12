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

    def newOutput = new ByteVectorOutput

    def sizeOf(bytes: ByteVector): Long = bytes.size

    def fromByteArray(byteArray: Array[Byte]): ByteVector = ByteVector(byteArray)

    def toByteArray(bytes: ByteVector): Array[Byte] = bytes.toArray

    def concat(a: ByteVector, b: ByteVector) =
      if (a.nonEmpty) {
        if (b.nonEmpty) {
          a ++ b
        } else a
      } else b

    def convert[B](value: B)(implicit byteAccess: ByteAccess[B]) =
      value match {
        case x: ByteVector ⇒ x
        case x             ⇒ ByteVector(byteAccess.toByteArray(x))
      }

    def empty = ByteVector.empty
  }

  /**
    * Encoding and Decoding for [[ByteVector]].
    */
  implicit val ByteVectorCodec = Codec.of[ByteVector](_ writeBytes _, _.readBytes())

  /**
    * Mutable [[Input]] implementation for deserializing from [[ByteVector]]
    */
  implicit class ByteVectorInput(input: ByteVector) extends Input with java.lang.Cloneable {
    private[this] var _cursor: Long          = _
    private[this] var _lastByte: Byte        = _
    private[this] var _lastBytes: ByteVector = _

    type Self  = ByteVectorInput
    type Bytes = ByteVector

    def byteAccess = ByteVectorByteAccess

    def cursor: Long          = _cursor
    def lastByte: Byte        = _lastByte
    def lastBytes: ByteVector = _lastBytes

    def hasBytes(length: Long): Boolean = {
      val off = length + _cursor
      0 <= off && off <= input.length
    }

    def readByte(): Self = {
      _lastByte = input(_cursor)
      _cursor += 1
      this
    }

    def readBytes(length: Long): Self = {
      if (length > 0) {
        val newCursor = _cursor + length
        if (newCursor >= 0) {
          _lastBytes = input.slice(_cursor, newCursor)
          _cursor = newCursor
        } else throw Borer.Error.Overflow(_cursor, "ByteVector input is limited to 2^63 bytes")
      } else _lastBytes = ByteVector.empty
      this
    }

    def copy: ByteVectorInput = super.clone().asInstanceOf[ByteVectorInput]
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

    def cursor: Int = _cursor

    def writeByte(byte: Byte): this.type = {
      val newCursor = _cursor + 1
      if (newCursor > 0) {
        ensureLength(newCursor)
        buffer(_cursor) = byte
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

    def result(): ByteVector = ByteVector.view(buffer, 0, _cursor)

    private def ensureLength(minSize: Int): Unit =
      if (buffer.length < minSize) {
        val newLen = math.max(buffer.length << 1, minSize)
        buffer = util.Arrays.copyOf(buffer, newLen)
      }

    private def overflow() = throw Borer.Error.Overflow(this, "Cannot output to byte array with > 2^31 bytes")
  }
}
