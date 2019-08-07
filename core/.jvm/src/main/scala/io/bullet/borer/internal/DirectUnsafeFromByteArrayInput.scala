/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.internal

import io.bullet.borer.Input
import io.bullet.borer.input.FromByteArrayInput

object DirectUnsafeFromByteArrayInput {

  def apply(input: Input[_]): DirectUnsafeFromByteArrayInput =
    input match {
      case fromByteArrayInput: FromByteArrayInput#FromByteArray =>
        Unsafe.byteArrayAccess match {
          case lebaa: Unsafe.LittleEndianByteArrayAccess =>
            new DirectUnsafeFromByteArrayInput(fromByteArrayInput.byteArray, lebaa)
          case _ => null
        }
      case _ => null
    }
}

/**
  * Almost verbatim copy of [[Input.FromByteArray]] but without any virtual calls.
  */
final class DirectUnsafeFromByteArrayInput(byteArray: Array[Byte], baa: Unsafe.LittleEndianByteArrayAccess)
    extends Input[Array[Byte]] {
  private[this] var _cursor: Int = _

  def cursor: Long = _cursor.toLong

  def unread(numberOfBytes: Int): this.type = {
    _cursor -= numberOfBytes
    this
  }

  def readByte(): Byte = {
    val c = _cursor
    _cursor = c + 1
    byteArray(c)
  }

  def readBytePadded(pp: Input.PaddingProvider[Array[Byte]]): Byte =
    if (_cursor >= byteArray.length) pp.padByte() else readByte()

  def readDoubleByteBigEndian(): Char = {
    val c = _cursor
    _cursor = c + 2
    baa.doubleByteBigEndian(byteArray, c)
  }

  def readDoubleByteBigEndianPadded(pp: Input.PaddingProvider[Array[Byte]]): Char = {
    val remaining = byteArray.length - _cursor
    if (remaining >= 2) readDoubleByteBigEndian()
    else pp.padDoubleByte(remaining)
  }

  def readQuadByteBigEndian(): Int = {
    val c = _cursor
    _cursor = c + 4
    baa.quadByteBigEndian(byteArray, c)
  }

  def readQuadByteBigEndianPadded(pp: Input.PaddingProvider[Array[Byte]]): Int = {
    val remaining = byteArray.length - _cursor
    if (remaining >= 4) readQuadByteBigEndian()
    else pp.padQuadByte(remaining)
  }

  def readOctaByteBigEndian(): Long = {
    val c = _cursor
    _cursor = c + 8
    baa.octaByteBigEndian(byteArray, c)
  }

  def readOctaByteBigEndianPadded(pp: Input.PaddingProvider[Array[Byte]]): Long = {
    val remaining = byteArray.length - _cursor
    if (remaining >= 8) readOctaByteBigEndian()
    else pp.padOctaByte(remaining)
  }

  def readBytes(length: Long, pp: Input.PaddingProvider[Array[Byte]]): Array[Byte] = {
    val remaining = (byteArray.length - _cursor).toLong
    val len       = math.min(remaining, length).toInt
    val bytes =
      if (len > 0) {
        val result = new Array[Byte](len)
        val c      = _cursor
        _cursor = c + len
        System.arraycopy(byteArray, c, result, 0, len)
        result
      } else Array.emptyByteArray
    if (length <= remaining) bytes
    else pp.padBytes(bytes, length - remaining)
  }
}
