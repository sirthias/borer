/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.internal

private[borer] class ByteArrayAccess {

  def putLongBigEndian(buf: Array[Byte], offset: Int, long: Long): Unit = {
    buf(offset) = (long >> 56).toByte
    buf(offset + 1) = (long << 8 >> 56).toByte
    buf(offset + 2) = (long << 16 >> 56).toByte
    buf(offset + 3) = (long << 24 >> 56).toByte
    buf(offset + 4) = (long << 32 >> 56).toByte
    buf(offset + 5) = (long << 40 >> 56).toByte
    buf(offset + 6) = (long << 48 >> 56).toByte
    buf(offset + 7) = long.toByte
  }

}

object ByteArrayAccess {

  implicit final val instance: ByteArrayAccess = {
    val unsafeInputAccess = io.bullet.borer.Unsafe.byteArrayAccess
    if (unsafeInputAccess ne null) unsafeInputAccess
    else new ByteArrayAccess
  }
}
