/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import java.util

/**
  * Abstraction over serialization output.
  *
  * The implementation be either mutable or immutable.
  */
trait Output {
  type Self <: Output
  type Result

  def writeByte(byte: Byte): Self
  def writeBytes[Bytes: ByteAccess](bytes: Bytes): Self

  def result(): Result
}

object Output {

  implicit final class OutputOps(val underlying: Output) extends AnyVal {
    def writeShort(value: Short): Output = underlying.writeByte((value >> 8).toByte).writeByte(value.toByte)
    def writeInt(value: Int): Output     = writeShort((value >> 16).toShort).writeShort(value.toShort)
    def writeLong(value: Long): Output   = writeInt((value >> 32).toInt).writeInt(value.toInt)
  }

  /**
    * Default, mutable implementation for serializing to plain byte arrays.
    */
  final class ToByteArray extends Output {
    private[this] var buffer       = new Array[Byte](64)
    private[this] var _cursor: Int = _

    type Self   = ToByteArray
    type Result = Array[Byte]

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

    def writeBytes[Bytes](bytes: Bytes)(implicit byteAccess: ByteAccess[Bytes]): this.type = {
      val byteArray = byteAccess.toByteArray(bytes)
      val l         = byteArray.length
      val newCursor = _cursor + l
      if (newCursor > 0) {
        ensureLength(newCursor)
        System.arraycopy(byteArray, 0, buffer, _cursor, l)
        _cursor = newCursor
        this
      } else overflow()
    }

    def result(): Array[Byte] =
      if (_cursor < buffer.length) {
        val result = new Array[Byte](_cursor)
        System.arraycopy(buffer, 0, result, 0, _cursor)
        result
      } else buffer

    private def ensureLength(minSize: Int): Unit =
      if (buffer.length < minSize) {
        val newLen = math.max(buffer.length << 1, minSize)
        buffer = util.Arrays.copyOf(buffer, newLen)
      }

    private def overflow() = throw new Cbor.Error.Overflow(this, "Cannot output to byte array with > 2^31 bytes")
  }
}
