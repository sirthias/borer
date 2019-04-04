/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import java.util

import scala.annotation.tailrec

/**
  * Abstraction over serialization output.
  *
  * The implementation be either mutable or immutable.
  */
trait Output {
  type Self <: Output
  type Result <: AnyRef

  def writeByte(byte: Byte): Self
  def writeBytes(a: Byte, b: Byte): Self
  def writeBytes(a: Byte, b: Byte, c: Byte): Self
  def writeBytes(a: Byte, b: Byte, c: Byte, d: Byte): Self

  def writeBytes[Bytes: ByteAccess](bytes: Bytes): Self

  def result(): Result
}

object Output {

  implicit final class OutputOps(val underlying: Output) extends AnyVal {
    @inline def writeShort(value: Short): Output = underlying.writeBytes((value >> 8).toByte, value.toByte)
    @inline def writeInt(value: Int): Output =
      underlying.writeBytes((value >> 24).toByte, (value >> 16).toByte, (value >> 8).toByte, value.toByte)
    @inline def writeLong(value: Long): Output = writeInt((value >> 32).toInt).writeInt(value.toInt)

    @inline def writeAsByte(i: Int): Output = underlying.writeByte(i.toByte)

    @inline def writeAsByte(c: Char): Output                    = underlying.writeByte(c.toByte)
    @inline def writeAsBytes(a: Char, b: Char): Output          = underlying.writeBytes(a.toByte, b.toByte)
    @inline def writeAsBytes(a: Char, b: Char, c: Char): Output = underlying.writeBytes(a.toByte, b.toByte, c.toByte)
    @inline def writeAsBytes(a: Char, b: Char, c: Char, d: Char): Output =
      underlying.writeBytes(a.toByte, b.toByte, c.toByte, d.toByte)

    def writeStringAsAsciiBytes(s: String): Output = {
      @tailrec def rec(out: Output, ix: Int): Output =
        s.length - ix match {
          case 0 ⇒ out
          case 1 ⇒ writeAsByte(s.charAt(ix))
          case 2 ⇒ writeAsBytes(s.charAt(ix), s.charAt(ix + 1))
          case 3 ⇒ writeAsBytes(s.charAt(ix), s.charAt(ix + 1), s.charAt(ix + 2))
          case _ ⇒ rec(writeAsBytes(s.charAt(ix), s.charAt(ix + 1), s.charAt(ix + 2), s.charAt(ix + 3)), ix + 4)
        }
      rec(underlying, 0)
    }
  }

  /**
    * Default, mutable implementation for serializing to plain byte arrays.
    */
  final class ToByteArray extends Output {
    private[this] var buffer       = new Array[Byte](64)
    private[this] var _cursor: Int = _

    type Self   = ToByteArray
    type Result = Array[Byte]

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

    def writeBytes[Bytes](bytes: Bytes)(implicit byteAccess: ByteAccess[Bytes]): this.type = {
      val byteArray = byteAccess.toByteArray(bytes)
      val l         = byteArray.length
      val crs       = _cursor
      val newCursor = crs + l
      if (newCursor > 0) {
        ensureLength(newCursor)
        System.arraycopy(byteArray, 0, buffer, crs, l)
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

    @inline private def ensureLength(minSize: Int): Unit =
      if (buffer.length < minSize) {
        val newLen = math.max(buffer.length << 1, minSize)
        buffer = util.Arrays.copyOf(buffer, newLen)
      }

    private def overflow() = throw Borer.Error.Overflow(this, "Cannot output to byte array with > 2^31 bytes")
  }
}
