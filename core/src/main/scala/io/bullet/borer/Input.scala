/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import java.nio.charset.StandardCharsets

import io.bullet.borer.internal.ByteArrayAccess

/**
  * Mutable abstraction wrapping some source of bytes to serve as parser input.
  */
trait Input {

  type Bytes
  type Position

  def byteAccess: ByteAccess[Bytes]

  /**
    * The index of the next byte to be read.
    * Only serves as argument (potentially with a "small" offset applied) for a potential future call to `position`.
    */
  def cursor: Long

  /**
    * Turns the given marker (the result of some previous call to `posMarker`,
    * potentially with a "small" offset applied) into a [[Position]] instance.
    */
  def position(marker: Long): Position

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
    * Returns the next byte if not out-of-range, otherwise 0xFF.
    * Advances the cursor by 1 or to the end of the input.
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
    * If the input has less than 2 bytes left the missing bytes are filled with 0xFF.
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
    * If the input has less than 4 bytes left the missing bytes are filled with 0xFF.
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
    * If the input has less than 8 bytes left the missing bytes are filled with 0xFF.
    * Advances the cursor by 8, even if this means moving it beyond the end of the input.
    */
  def readOctaByteBigEndianPaddedFF(): Long

  /**
    * Returns the next `length` bytes as [[Bytes]], if possible without any range checks.
    */
  def readBytes(length: Long): Bytes

  /**
    * Moves the cursor the given number of bytes back or forward in order to be able to re-read bytes
    * that have already been read before or "correct/refine" a previous `moveCursor` call.
    *
    * NOTE: Count will always be >= -8 and <= 1 and never move the cursor outside the range of the preceding
    * `readXXX` call. In particular, this means that this method will never be used to move the cursor
    * back beyond the beginning of the input or forward to a byte that hasn't been read before.
    * Therefore an implementation does not have to apply any range check.
    */
  def moveCursor(offset: Int): this.type

  /**
    * Returns the given number of bytes _before_ the current cursor position as an ASCII string.
    * Does not move the cursor.
    */
  def precedingBytesAsAsciiString(length: Int): String
}

object Input {

  trait Wrapper[T] {
    type In <: Input
    def apply(value: T): In
  }

  final case class Position(input: Input, index: Long) {
    override def toString = s"input position $index"
  }

  implicit object ByteArrayWrapper extends Wrapper[Array[Byte]] {
    type In = FromByteArray
    def apply(value: Array[Byte]) = new FromByteArray(value)
  }

  final class FromByteArray(byteArray: Array[Byte]) extends Input {
    import ByteArrayAccess.{instance => baa}

    type Bytes    = Array[Byte]
    type Position = Input.Position

    protected var _cursor: Int = _

    @inline def cursor: Long = _cursor.toLong
    @inline def byteAccess   = ByteAccess.ForByteArray

    def position(marker: Long): Position = Position(this, marker)

    @inline def prepareRead(length: Long): Boolean = _cursor + length <= byteArray.length

    def readByte(): Byte = {
      val c = _cursor
      _cursor = c + 1
      byteArray(c)
    }

    @inline def readByteOrFF(): Byte = {
      def readPadded(): Byte = {
        _cursor += 1
        -1
      }
      if (_cursor < byteArray.length) readByte() else readPadded()
    }

    def readDoubleByteBigEndian(): Char = {
      val c = _cursor
      _cursor = c + 2
      baa.doubleByteBigEndian(byteArray, c)
    }

    @inline def readDoubleByteBigEndianPaddedFF(): Char = {
      def readPadded(): Char = {
        val c = _cursor
        _cursor = c + 2
        byteArray.length - c match {
          case 1 ⇒ (byteArray(c) << 8 | 0xFF).toChar
          case _ ⇒ '\uffff'
        }
      }
      if (_cursor < byteArray.length - 1) readDoubleByteBigEndian() else readPadded()
    }

    def readQuadByteBigEndian(): Int = {
      val c = _cursor
      _cursor = c + 4
      baa.quadByteBigEndian(byteArray, c)
    }

    @inline def readQuadByteBigEndianPaddedFF(): Int = {
      def readPadded(): Int = {
        val c = _cursor
        _cursor = c + 4
        byteArray.length - c match {
          case 1 ⇒ byteArray(c) << 24 | 0xFFFFFF
          case 2 ⇒ baa.doubleByteBigEndian(byteArray, c) << 16 | 0xFFFF
          case 3 ⇒ baa.doubleByteBigEndian(byteArray, c) << 16 | (byteArray(c + 2) & 0xFF) << 8 | 0xFF
          case _ ⇒ -1
        }
      }
      if (_cursor < byteArray.length - 3) readQuadByteBigEndian() else readPadded()
    }

    def readOctaByteBigEndian(): Long = {
      val c = _cursor
      _cursor = c + 8
      baa.octaByteBigEndian(byteArray, c)
    }

    @inline def readOctaByteBigEndianPaddedFF(): Long = {
      def readPadded(): Long = {
        val c = _cursor
        _cursor = c + 8
        byteArray.length - c match {
          case 1 ⇒ byteArray(c).toLong << 56 | 0xffffffffffffffl
          case 2 ⇒ baa.doubleByteBigEndian(byteArray, c).toLong << 48 | 0xffffffffffffl
          case 3 ⇒ baa.doubleByteBigEndian(byteArray, c).toLong << 48 | (byteArray(c + 2) & 0xffl) << 40 | 0xffffffffffl
          case 4 ⇒ baa.quadByteBigEndian(byteArray, c).toLong << 32 | 0xffffffffl
          case 5 ⇒ baa.quadByteBigEndian(byteArray, c).toLong << 32 | (byteArray(c + 4) & 0xffl) << 24 | 0xffffffl
          case 6 ⇒
            baa.quadByteBigEndian(byteArray, c).toLong << 32 |
              (baa.doubleByteBigEndian(byteArray, c + 4) & 0xffffl) << 16 |
              0xffffl
          case 7 ⇒
            baa.quadByteBigEndian(byteArray, c).toLong << 32 |
              (baa.doubleByteBigEndian(byteArray, c + 4) & 0xffffl) << 16 |
              (byteArray(c + 6) & 0xffl) << 8 | 0xffl
          case _ => -1
        }
      }
      if (_cursor < byteArray.length - 7) readOctaByteBigEndian() else readPadded()
    }

    @inline def readBytes(length: Long): Bytes = {
      val len = length.toInt
      if (length == len) {
        if (len != 0) {
          val result = new Array[Byte](len)
          val c      = _cursor
          _cursor = c + len
          System.arraycopy(byteArray, c, result, 0, len)
          result
        } else Array.emptyByteArray
      } else throw new Borer.Error.Overflow(position(cursor), "Byte-array input is limited to size 2GB")
    }

    @inline def moveCursor(offset: Int): this.type = {
      _cursor += offset
      this
    }

    @inline def precedingBytesAsAsciiString(length: Int): String =
      new String(byteArray, _cursor - length, length, StandardCharsets.ISO_8859_1)
  }

}
