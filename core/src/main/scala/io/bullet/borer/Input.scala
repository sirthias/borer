/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import io.bullet.borer.input.{FromByteArrayInput, FromByteBufferInput, FromFileInput}

/**
  * Mutable abstraction wrapping some source of bytes to serve as parser input.
  */
trait Input {

  type Bytes
  type Position

  def byteAccess: ByteAccess[Bytes]

  /**
    * The index of the next byte to be read.
    */
  def cursor: Long

  /**
    * Resets the cursor to the given value.
    * Will never be called with a value smaller than the argument of any preceding `releaseBefore` call.
    */
  def resetTo(cursor: Long): this.type

  /**
    * Moves the cursor by the given offset, i.e. is expected to be equivalent to `resetTo(cursor + offset)`,
    * but potentially more efficient (since it entails only one virtual dispatch rather than two).
    * Like `resetTo` this method will never be called to reset the cursor beyond a value that was previously
    * already released with a call to `releaseBefore`.
    */
  def moveCursor(offset: Int): this.type

  /**
    * Informs the underlying [[Input]] implementation that all input parts before
    * the current cursor value can be safely dropped/freed/released.
    */
  def releaseBeforeCursor(): this.type

  /**
    * Returns a [[Position]] instance for the given cursor value.
    */
  def position(cursor: Long): Position

  /**
    * Prepares all underlying structures for reading the given number of bytes and
    * returns true if a subsequent call to any of the `read...` methods will not be out-of-range,
    * otherwise false.
    */
  def prepareRead(length: Long): Boolean

  /**
    * Returns the next byte, if possible without any range checks.
    * Advances the cursor by 1.
    */
  def readByte(): Byte

  /**
    * Returns the next byte if not out-of-range, otherwise the given `byte`.
    * Advances the cursor by 1, even if this means moving it beyond the end of the input.
    */
  def readByteOrFF(): Byte

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
    * If the input has less than 2 bytes left the missing bytes are filled with the given padding shifted right.
    * Advances the cursor by 2, even if this means moving it beyond the end of the input.
    */
  def readDoubleByteBigEndianPaddedFF(): Char

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
    * If the input has less than 4 bytes left the missing bytes are filled with the given padding shifted right.
    * Advances the cursor by 4, even if this means moving it beyond the end of the input.
    */
  def readQuadByteBigEndianPaddedFF(): Int

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
    * If the input has less than 8 bytes left the missing bytes are filled with the given padding shifted right.
    * Advances the cursor by 8, even if this means moving it beyond the end of the input.
    */
  def readOctaByteBigEndianPaddedFF(): Long

  /**
    * Returns the next `length` bytes as [[Bytes]], if possible without any range checks.
    * Will never try to read beyond the end of the buffer (because it'll always be "protected" with a preceding
    * `prepareRead`) and therefore never has to return a padded result.
    */
  def readBytes(length: Long): Bytes

  /**
    * Returns the given number of bytes _before_ the current cursor position as an ASCII string.
    * Does not move the cursor.
    */
  def precedingBytesAsAsciiString(length: Int): String
}

object Input extends FromByteArrayInput with FromByteBufferInput with FromFileInput {

  implicit final class InputOps(val underlying: Input) extends AnyVal {

    def readDoubleByteBigEndianPaddedFF(remaining: Int): Char = {
      val result = if (remaining < 1) '\uffff' else ((underlying.readByte() << 8) | 0xFF).toChar
      underlying.moveCursor(2 - remaining)
      result
    }

    def readQuadByteBigEndianPaddedFF(remaining: Int): Int = {
      import underlying.{readByte => byte, readDoubleByteBigEndian => doub}
      // format: OFF
      val result = remaining match {
        case 0 =>                                                  0xFFFFFFFF
        case 1 =>                         (byte().toInt   << 24) | 0xFFFFFF
        case 2 => (doub().toInt << 16)                           | 0xFFFF
        case 3 => (doub().toInt << 16) | ((byte() & 0xFF) <<  8) | 0xFF
        case _ => throw new IllegalStateException
      }
      // format: ON
      underlying.moveCursor(4 - remaining)
      result
    }

    def readOctaByteBigEndianPaddedFF(remaining: Int): Long = {
      import underlying.{readByte => byte, readDoubleByteBigEndian => doub, readQuadByteBigEndian => quad}
      // format: OFF
      val result = remaining match {
        case 0 =>                                                                                 0XFFFFFFFFFFFFFFFFL
        case 1 =>                                                      (byte().toLong    << 56) | 0XFFFFFFFFFFFFFFL
        case 2 =>                         (doub().toLong      << 48)                            | 0XFFFFFFFFFFFFL
        case 3 =>                         (doub().toLong      << 48) | ((byte() & 0XFFL) << 40) | 0XFFFFFFFFFFL
        case 4 => (quad().toLong << 32) |                                                         0XFFFFFFFFL
        case 5 => (quad().toLong << 32) |                              ((byte() & 0XFFL) << 24) | 0XFFFFFFL
        case 6 => (quad().toLong << 32) | ((doub() & 0XFFFFL) << 16) |                            0XFFFFL
        case 7 => (quad().toLong << 32) | ((doub() & 0XFFFFL) << 16) | ((byte() & 0XFFL) <<  8) | 0XFFL
        case _ => throw new IllegalStateException
      }
      // format: ON
      underlying.moveCursor(8 - remaining)
      result
    }

    def readMultiByteUtf8Codepoint(b1: Int): Int = {
      val byteCount = Integer.numberOfLeadingZeros(~b1) - 25
      val quad      = underlying.readQuadByteBigEndianPaddedFF()
      val b2        = quad >> 24
      val codepoint = (byteCount | 0x80) ^ (b2 & 0xC0) match {
        case 1 =>
          if ((b1 & 0x1E) == 0) failIllegalUtf8(-5)
          (b1 << 6) ^ b2 ^ 0xF80
        case 2 =>
          val b3 = quad << 8 >> 24
          val cp = (b1 << 12) ^ (b2 << 6) ^ b3 ^ 0xFFFE1F80
          if ((b1 == 0xE0 && (b2 & 0xE0) == 0x80) || (b3 & 0xC0) != 0x80 || ((cp >> 11) == 0x1b)) failIllegalUtf8(-5)
          cp
        case 3 =>
          val b3 = quad << 8 >> 24
          val b4 = quad << 16 >> 24
          val cp = (b1 << 18) ^ (b2 << 12) ^ (b3 << 6) ^ b4 ^ 0x381F80
          if ((b3 & 0xC0) != 0x80 || (b4 & 0xC0) != 0x80 || cp < 0x010000 || cp > 0x10FFFF) failIllegalUtf8(-5)
          cp
        case _ => failIllegalUtf8(-5)
      }
      underlying.moveCursor(byteCount - 4)
      codepoint
    }

    private def failIllegalUtf8(offset: Int) = {
      val pos = underlying.position(underlying.cursor + offset.toLong)
      throw new Borer.Error.InvalidInputData(pos, "Illegal UTF-8 character encoding")
    }
  }

  /**
    * Responsible for wrapping an instance of [[T]] in a respective [[Input]].
    */
  trait Wrapper[T] {
    type In <: Input
    def apply(value: T): In
  }

  final case class Position(input: Input, index: Long) {
    override def toString = s"input position $index"
  }
}
