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
  def safeByte(input: Input, index: Long): Byte

  /**
    * Returns the [[Input]] byte at the given index,
    * potentially without any range checks.
    */
  def unsafeByte(input: Input, index: Long): Byte

  /**
    * Returns the two [[Input]] bytes at the given index
    * with the latter one becoming the (right-most) LSB.
    * (big endian/network byte order)
    */
  def doubleByteBigEndian(input: Input, index: Long): Int

  /**
    * Returns the four [[Input]] bytes at the given index
    * with the last one becoming the (right-most) LSB.
    * (big endian/network byte order)
    */
  def quadByteBigEndian(input: Input, index: Long): Int

  /**
    * Returns the four [[Input]] bytes at the given index
    * with the first one becoming the (left-most) MSB.
    * (big endian/network byte order)
    */
  def octaByteBigEndian(input: Input, index: Long): Long

  /**
    * Returns the `length` [[Bytes]] of the [[Input]] at the given index
    * or throws an [[IndexOutOfBoundsException]].
    */
  def bytes(input: Input, index: Long, length: Long): Bytes
}

object InputAccess {

  /**
    * `InputAccess` for plain byte arrays.
    */
  implicit final val ForByteArray: InputAccess[Array[Byte]] { type Bytes = Array[Byte] } = {
    val unsafeInputAccess = Unsafe.byteArrayInputAccess
    if (unsafeInputAccess eq null) {
      new InputAccess[Array[Byte]] {
        type Bytes = Array[Byte]

        def byteAccess = ByteAccess.ForByteArray

        def length(input: Array[Byte]): Long = input.length.toLong

        def safeByte(input: Array[Byte], index: Long): Byte = input(index.toInt)

        def unsafeByte(input: Array[Byte], index: Long): Byte = input(index.toInt)

        def doubleByteBigEndian(input: Array[Byte], index: Long): Int = {
          val i = index.toInt
          (input(i) & 0xFF) << 8 |
          (input(i + 1) & 0xFF)
        }

        def quadByteBigEndian(input: Array[Byte], index: Long): Int = {
          val i = index.toInt
          (input(i) & 0xFF) << 24 |
          (input(i + 1) & 0xFF) << 16 |
          (input(i + 2) & 0xFF) << 8 |
          (input(i + 3) & 0xFF)
        }

        def octaByteBigEndian(input: Array[Byte], index: Long): Long = {
          val i = index.toInt
          (input(i) & 0xFFL) << 56 |
          (input(i + 1) & 0xFFL) << 48 |
          (input(i + 2) & 0xFFL) << 40 |
          (input(i + 3) & 0xFFL) << 32 |
          (input(i + 4) & 0xFFL) << 24 |
          (input(i + 5) & 0xFFL) << 16 |
          (input(i + 6) & 0xFFL) << 8 |
          (input(i + 7) & 0xFFL)
        }

        def bytes(input: Array[Byte], index: Long, length: Long): Array[Byte] =
          if (length >> 31 == 0) {
            if (length != 0) {
              val len    = length.toInt
              val result = new Array[Byte](len)
              System.arraycopy(input, index.toInt, result, 0, len)
              result
            } else Array.emptyByteArray
          } else throw new Borer.Error.Overflow(Position(input, index), "Byte-array input is limited to size 2GB")
      }
    } else unsafeInputAccess
  }
}
