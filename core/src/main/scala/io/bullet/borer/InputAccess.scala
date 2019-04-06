/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

/**
  * Type class for byte access to an `Input`
  */
trait InputAccess[Input] {
  type Bytes

  def byteAccess: ByteAccess[Bytes]

  /**
    * Returns the number of bytes available from the given [[Input]].
    */
  def length(input: Input): Long

  /**
    * Returns the [[Input]] byte at the given index
    * or throws an [[IndexOutOfBoundsException]].
    */
  def byteAt(input: Input, index: Long): Byte

  /**
    * Returns the `length` [[Bytes]] of the [[Input]] at the given index
    * or throws an [[IndexOutOfBoundsException]].
    */
  def bytesAt(input: Input, index: Long, length: Long): Bytes
}

object InputAccess {

  private[borer] def asAny[Input](implicit ia: InputAccess[Input]) = ia.asInstanceOf[InputAccess[Any]]

  /**
    * `InputAccess` for plain byte arrays.
    */
  implicit final object ForByteArray extends InputAccess[Array[Byte]] {
    type Bytes = Array[Byte]

    def byteAccess = ByteAccess.ForByteArray

    @inline def length(input: Array[Byte]): Long = input.length.toLong

    @inline def byteAt(input: Array[Byte], index: Long): Byte = input(index.toInt)

    def bytesAt(input: Array[Byte], index: Long, length: Long): Array[Byte] =
      if (length >> 31 == 0) {
        if (length != 0) {
          val len    = length.toInt
          val result = new Array[Byte](len)
          System.arraycopy(input, index.toInt, result, 0, len)
          result
        } else Array.emptyByteArray
      } else throw new Borer.Error.Overflow(Position(input, index), "Byte-array input is limited to size 2GB")
  }
}
