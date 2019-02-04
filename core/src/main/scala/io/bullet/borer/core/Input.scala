/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.core

/**
  * Abstraction over deserialization input.
  *
  * The implementation be either mutable or immutable.
  *
  * @tparam Bytes The abstraction for byte chunks that this [[Input]] works with.
  */
trait Input[+Bytes] {
  type Self <: Input[Bytes]

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
  implicit class FromByteArray(input: Array[Byte]) extends Input[Array[Byte]] with java.lang.Cloneable {
    private[this] var _cursor: Int            = _
    private[this] var _lastByte: Byte         = _
    private[this] var _lastBytes: Array[Byte] = _

    type Self = FromByteArray

    def cursor: Int            = _cursor
    def lastByte: Byte         = _lastByte
    def lastBytes: Array[Byte] = _lastBytes

    def hasBytes(length: Long): Boolean = {
      val off = length + _cursor
      0 <= off && off <= input.length
    }

    def readByte(): Self = {
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
      } else throw new Cbor.Error.Overflow(_cursor, "Byte-array input is limited to size 2GB")

    def copy: FromByteArray = super.clone().asInstanceOf[FromByteArray]
  }
}
