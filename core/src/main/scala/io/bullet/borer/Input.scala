/*
 * Copyright (c) 2019-2024 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import io.bullet.borer.input.*

/**
 * Mutable abstraction wrapping some source of bytes to serve as parser input.
 */
trait Input[Bytes]:

  /**
   * The index of the next byte to be read.
   */
  def cursor: Long

  /**
   * "Unreads" the given number of bytes, which is guaranteed to be in the range [1, 255].
   * This is the same as moving the cursor the given number of positions back.
   *
   * NOTE: This method will never be used to move the cursor beyond the beginning of the input.
   *       As such, no range check is required by the implementation.
   *       Also the maximum number of bytes that is unread, _in total_, will never exceed 255.
   *       So any input will never have to cache more that the last 255 bytes from the head of the input.
   *
   * Also: Decoding CBOR never needs unreading, so if your use case doesn't have to support JSON
   *       then it's fine to simply "implement" this method with `???`.
   */
  def unread(numberOfBytes: Int): this.type

  /**
   * Returns the next byte, if possible without any range checks.
   * Advances the cursor by 1.
   */
  def readByte(): Byte

  /**
   * Returns the next byte if not out-of-range, otherwise the one returned by the given [[Input.PaddingProvider]].
   */
  def readBytePadded(pp: Input.PaddingProvider[Bytes]): Byte

  /**
   * Returns the next two bytes as an unsigned 16-bit value,
   * with the first becoming the more-significant byte (i.e. big endian/network byte order),
   * if possible without any range checks.
   * Advances the cursor by 2.
   */
  def readDoubleByteBigEndian(): Char

  /**
   * Returns the next two bytes as an unsigned 16-bit value,
   * with the first becoming the more-significant byte (i.e. big endian/network byte order).
   * If the input has less than 2 bytes left the given [[Input.PaddingProvider]] is called to perform the padding
   * and its result returned.
   */
  def readDoubleByteBigEndianPadded(pp: Input.PaddingProvider[Bytes]): Char

  /**
   * Returns the next four bytes as an [[Int]],
   * with the first becoming the most-significant byte (i.e. big endian/network byte order),
   * if possible without any range checks.
   * Advances the cursor by 4.
   */
  def readQuadByteBigEndian(): Int

  /**
   * Returns the next four bytes as an [[Int]],
   * with the first becoming the most-significant byte (i.e. big endian/network byte order).
   * If the input has less than 4 bytes left the given [[Input.PaddingProvider]] is called to perform the padding
   * and its result returned.
   */
  def readQuadByteBigEndianPadded(pp: Input.PaddingProvider[Bytes]): Int

  /**
   * Returns the eight eight bytes as a [[Long]],
   * with the first becoming the most-significant byte (i.e. big endian/network byte order),
   * if possible without any range checks.
   * Advances the cursor by 8.
   */
  def readOctaByteBigEndian(): Long

  /**
   * Returns the next eight bytes as a [[Long]],
   * with the first becoming the most-significant byte (i.e. big endian/network byte order).
   * If the input has less than 8 bytes left the given [[Input.PaddingProvider]] is called to perform the padding
   * and its result returned.
   */
  def readOctaByteBigEndianPadded(pp: Input.PaddingProvider[Bytes]): Long

  /**
   * Returns the next `length` bytes as [[Bytes]] if the input still has this many bytes available.
   * Otherwise the given [[Input.PaddingProvider]] is called to perform the padding and its result returned.
   */
  def readBytes(length: Long, pp: Input.PaddingProvider[Bytes]): Bytes

object Input
    extends FromByteArrayInput with FromByteBufferInput with FromInputStreamInput with FromFileInput
    with FromIteratorInput:

  abstract class PaddingProvider[Bytes]:
    def padByte(): Byte
    def padDoubleByte(remaining: Int): Char
    def padQuadByte(remaining: Int): Int
    def padOctaByte(remaining: Int): Long
    def padBytes(rest: Bytes, missing: Long): Bytes

  extension [Bytes](underlying: Input[Bytes])
    def position(cursor: Long): Input.Position = Input.Position(underlying, cursor)

  // #provider
  /**
   * Responsible for converting an instance of [[T]]
   * to a respective [[Input]] instance.
   */
  trait Provider[T]:
    type Bytes
    def byteAccess: ByteAccess[Bytes]
    def apply(value: T): Input[Bytes]
  // #provider

  /**
   * The trivial provider for an already existing [[Input]].
   */
  given [B](using ba: ByteAccess[B]): Provider[Input[B]] with
    type Bytes = B
    def byteAccess: ByteAccess[B]            = ba
    def apply(value: Input[B]): Input[Bytes] = value

  case class Position(input: Input[_], index: Long):
    override def toString = s"input position $index"
