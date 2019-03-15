/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

/**
  * Abstraction over deserialization input.
  *
  * The implementation be either mutable or immutable.
  */
trait Input {
  type Self <: Input
  type Bytes

  def byteAccess: ByteAccess[Bytes]

  def lastByte: Byte
  def lastBytes: Bytes

  def hasBytes(length: Long): Boolean

  def readByte(): Self
  def readBytes(length: Long): Self

  def copy: Self
}

object Input {

  /**
    * Default, mutable implementation for deserializing from plain byte arrays.
    */
  implicit final class FromByteArray(input: Array[Byte]) extends Input with java.lang.Cloneable {
    private[this] var _cursor: Int            = _
    private[this] var _lastByte: Byte         = _
    private[this] var _lastBytes: Array[Byte] = _

    type Self  = FromByteArray
    type Bytes = Array[Byte]

    def byteAccess = ByteAccess.ForByteArray

    @inline def cursor: Int            = _cursor
    @inline def lastByte: Byte         = _lastByte
    @inline def lastBytes: Array[Byte] = _lastBytes

    @inline def hasBytes(length: Long): Boolean = {
      val off = length + _cursor
      0 <= off && off <= input.length
    }

    @inline def readByte(): Self = {
      _lastByte = input(_cursor)
      _cursor += 1
      this
    }

    def readBytes(length: Long): Self =
      if (length >> 31 == 0) {
        if (length > 0) {
          val len = length.toInt
          _lastBytes = new Array[Byte](len)
          System.arraycopy(input, _cursor, _lastBytes, 0, len)
          _cursor += len
        } else _lastBytes = Array.emptyByteArray
        this
      } else throw Borer.Error.Overflow(_cursor, "Byte-array input is limited to size 2GB")

    def copy: FromByteArray = super.clone().asInstanceOf[FromByteArray]
  }
}
