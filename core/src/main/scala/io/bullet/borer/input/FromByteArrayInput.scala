/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.input

import java.nio.charset.StandardCharsets

import io.bullet.borer.{Borer, ByteAccess, Input}
import io.bullet.borer.Input.{Position, Wrapper}
import io.bullet.borer.internal.ByteArrayAccess

trait FromByteArrayInput {

  implicit object ByteArrayWrapper extends Wrapper[Array[Byte]] {
    type In = FromByteArray
    def apply(value: Array[Byte]) = new FromByteArray(value)
  }

  final class FromByteArray(byteArray: Array[Byte]) extends Input {

    type Bytes    = Array[Byte]
    type Position = Input.Position

    protected var _cursor: Int = _

    @inline def cursor: Long = _cursor.toLong
    @inline def byteAccess   = ByteAccess.ForByteArray

    @inline def resetTo(cursor: Long) = {
      _cursor = cursor.toInt
      this
    }

    @inline def moveCursor(offset: Int): this.type = {
      _cursor += offset
      this
    }

    @inline def releaseBeforeCursor(): this.type = this

    def position(cursor: Long): Position = Position(this, cursor)

    @inline def prepareRead(length: Long): Boolean = _cursor + length <= byteArray.length

    def readByte(): Byte = {
      val c = _cursor
      _cursor = c + 1
      byteArray(c)
    }

    @inline def readByteOrFF(): Byte =
      if (_cursor >= byteArray.length) {
        _cursor += 1
        -1
      } else readByte()

    def readDoubleByteBigEndian(): Char = {
      val c = _cursor
      _cursor = c + 2
      ByteArrayAccess.instance.doubleByteBigEndian(byteArray, c)
    }

    @inline def readDoubleByteBigEndianPaddedFF(): Char = {
      val remaining = byteArray.length - _cursor
      if (remaining >= 2) readDoubleByteBigEndian()
      else readDoubleByteBigEndianPaddedFF(remaining)
    }

    def readQuadByteBigEndian(): Int = {
      val c = _cursor
      _cursor = c + 4
      ByteArrayAccess.instance.quadByteBigEndian(byteArray, c)
    }

    @inline def readQuadByteBigEndianPaddedFF(): Int = {
      val remaining = byteArray.length - _cursor
      if (remaining >= 4) readQuadByteBigEndian()
      else readQuadByteBigEndianPaddedFF(remaining)
    }

    def readOctaByteBigEndian(): Long = {
      val c = _cursor
      _cursor = c + 8
      ByteArrayAccess.instance.octaByteBigEndian(byteArray, c)
    }

    @inline def readOctaByteBigEndianPaddedFF(): Long = {
      val remaining = byteArray.length - _cursor
      if (remaining >= 8) readOctaByteBigEndian()
      else readOctaByteBigEndianPaddedFF(remaining)
    }

    @inline def readBytes(length: Long): Bytes = {
      val len = length.toInt
      if (length == len) {
        if (len > 0) {
          val result = new Array[Byte](len)
          val c      = _cursor
          _cursor = c + len
          System.arraycopy(byteArray, c, result, 0, len)
          result
        } else Array.emptyByteArray
      } else throw new Borer.Error.Overflow(position(cursor), "Byte-array input is limited to size 2GB")
    }

    @inline def precedingBytesAsAsciiString(length: Int): String =
      new String(byteArray, _cursor - length, length, StandardCharsets.ISO_8859_1)
  }
}
