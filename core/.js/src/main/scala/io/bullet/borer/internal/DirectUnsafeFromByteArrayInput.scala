/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.internal

import io.bullet.borer.Input

object DirectUnsafeFromByteArrayInput {
  def apply(input: Input[_]): DirectUnsafeFromByteArrayInput = null
}

final class DirectUnsafeFromByteArrayInput private extends Input[Array[Byte]] {
  def cursor: Long                                                                 = fail()
  def unread(numberOfBytes: Int): this.type                                        = fail()
  def readByte(): Byte                                                             = fail()
  def readBytePadded(pp: Input.PaddingProvider[Array[Byte]]): Byte                 = fail()
  def readDoubleByteBigEndian(): Char                                              = fail()
  def readDoubleByteBigEndianPadded(pp: Input.PaddingProvider[Array[Byte]]): Char  = fail()
  def readQuadByteBigEndian(): Int                                                 = fail()
  def readQuadByteBigEndianPadded(pp: Input.PaddingProvider[Array[Byte]]): Int     = fail()
  def readOctaByteBigEndian(): Long                                                = fail()
  def readOctaByteBigEndianPadded(pp: Input.PaddingProvider[Array[Byte]]): Long    = fail()
  def readBytes(length: Long, pp: Input.PaddingProvider[Array[Byte]]): Array[Byte] = fail()
  private def fail()                                                               = throw new UnsupportedOperationException
}
