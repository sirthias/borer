/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.internal

abstract class ByteArrayAccess {

  def doubleByteBigEndian(byteArray: Array[Byte], ix: Int): Char

  def quadByteBigEndian(byteArray: Array[Byte], ix: Int): Int

  def octaByteBigEndian(byteArray: Array[Byte], ix: Int): Long

}

object ByteArrayAccess {

  final val instance: ByteArrayAccess = {
    val unsafe = io.bullet.borer.Unsafe.byteArrayAccess
    if (unsafe ne null) unsafe else new Default
  }

  final class Default extends ByteArrayAccess {

    def doubleByteBigEndian(byteArray: Array[Byte], ix: Int): Char =
      ((byteArray(ix) << 8) | byteArray(ix + 1) & 0xFF).toChar

    def quadByteBigEndian(byteArray: Array[Byte], ix: Int): Int =
      byteArray(ix) << 24 |
        (byteArray(ix + 1) & 0xFF) << 16 |
        (byteArray(ix + 2) & 0xFF) << 8 |
        byteArray(ix + 3) & 0xFF

    def octaByteBigEndian(byteArray: Array[Byte], ix: Int): Long = {
      byteArray(ix).toLong << 56 |
      (byteArray(ix + 1) & 0XFFL) << 48 |
      (byteArray(ix + 2) & 0XFFL) << 40 |
      (byteArray(ix + 3) & 0XFFL) << 32 |
      (byteArray(ix + 4) & 0XFFL) << 24 |
      (byteArray(ix + 5) & 0XFFL) << 16 |
      (byteArray(ix + 6) & 0XFFL) << 8 |
      byteArray(ix + 7) & 0XFFL
    }
  }
}
