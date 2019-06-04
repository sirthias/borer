/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import java.nio.charset.StandardCharsets
import java.nio.ByteBuffer

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
  def readByteOr(byte: Byte): Byte

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
  def readDoubleByteBigEndianPadded(padding: Char): Char

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
  def readQuadByteBigEndianPadded(padding: Int): Int

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
  def readOctaByteBigEndianPadded(padding: Long): Long

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

object Input {

  implicit final class InputOps(val underlying: Input) extends AnyVal {

    def readByteOrFF(): Byte = underlying.readByteOr(-1)
    def readByteOr00(): Byte = underlying.readByteOr(0)

    def readDoubleByteBigEndianPaddedFF(): Char = underlying.readDoubleByteBigEndianPadded('\uffff')
    def readDoubleByteBigEndianPadded00(): Char = underlying.readDoubleByteBigEndianPadded('\u0000')

    def readDoubleByteBigEndianPadded(remaining: Int, padding: Char): Char = {
      val result = if (remaining < 1) padding else ((underlying.readByte() << 8) | (padding >> 8)).toChar
      underlying.moveCursor(2 - remaining)
      result
    }

    def readQuadByteBigEndianPaddedFF(): Int = underlying.readQuadByteBigEndianPadded(-1)
    def readQuadByteBigEndianPadded00(): Int = underlying.readQuadByteBigEndianPadded(0)

    def readQuadByteBigEndianPadded(remaining: Int, padding: Int): Int = {
      import underlying.{readByte => byte, readDoubleByteBigEndian => doub}
      // format: OFF
      val result = remaining match {
        case 0 =>                                                   padding >>>  0
        case 1 =>                         (byte().toInt   << 24) | (padding >>>  8)
        case 2 => (doub().toInt << 16)                           | (padding >>> 16)
        case 3 => (doub().toInt << 16) | ((byte() & 0xFF) <<  8) | (padding >>> 24)
        case _ => throw new IllegalStateException
      }
      // format: ON
      underlying.moveCursor(4 - remaining)
      result
    }

    def readOctaByteBigEndianPaddedFF(): Long = underlying.readOctaByteBigEndianPadded(-1)
    def readOctaByteBigEndianPadded00(): Long = underlying.readOctaByteBigEndianPadded(0)

    def readOctaByteBigEndianPadded(remaining: Int, padding: Long): Long = {
      import underlying.{readByte => byte, readDoubleByteBigEndian => doub, readQuadByteBigEndian => quad}
      // format: OFF
      val result = remaining match {
        case 0 =>                                                                                  padding >>>  0
        case 1 =>                                                      (byte().toLong    << 56) | (padding >>>  8)
        case 2 =>                         (doub().toLong      << 48)                            | (padding >>> 16)
        case 3 =>                         (doub().toLong      << 48) | ((byte() & 0XFFL) << 40) | (padding >>> 24)
        case 4 => (quad().toLong << 32) |                                                         (padding >>> 32)
        case 5 => (quad().toLong << 32) |                              ((byte() & 0XFFL) << 24) | (padding >>> 40)
        case 6 => (quad().toLong << 32) | ((doub() & 0XFFFFL) << 16) |                            (padding >>> 48)
        case 7 => (quad().toLong << 32) | ((doub() & 0XFFFFL) << 16) | ((byte() & 0XFFL) <<  8) | (padding >>> 56)
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

  implicit object ByteArrayWrapper extends Wrapper[Array[Byte]] {
    type In = FromByteArray
    def apply(value: Array[Byte]) = new FromByteArray(value)
  }

  final class FromByteArray(private[borer] var byteArray: Array[Byte]) extends Input {
    import ByteArrayAccess.{instance => baa}

    type Bytes    = Array[Byte]
    type Position = Input.Position

    protected var _cursor: Int = _

    @inline def cursor: Long = _cursor.toLong
    @inline def byteAccess   = ByteAccess.ForByteArray

    @inline def resetTo(cursor: Long) = {
      _cursor = cursor.toInt
      this
    }

    @inline def moveCursor(offset: Int): this.type = {
      _cursor += offset
      this
    }

    @inline def releaseBeforeCursor(): this.type = this

    def position(cursor: Long): Position = Position(this, cursor)

    @inline def prepareRead(length: Long): Boolean = _cursor + length <= byteArray.length

    def readByte(): Byte = {
      val c = _cursor
      _cursor = c + 1
      byteArray(c)
    }

    @inline def readByteOr(byte: Byte): Byte =
      if (_cursor >= byteArray.length) {
        _cursor += 1
        byte
      } else readByte()

    def readDoubleByteBigEndian(): Char = {
      val c = _cursor
      _cursor = c + 2
      baa.doubleByteBigEndian(byteArray, c)
    }

    @inline def readDoubleByteBigEndianPadded(padding: Char): Char = {
      val remaining = byteArray.length - _cursor
      if (remaining >= 2) readDoubleByteBigEndian()
      else readDoubleByteBigEndianPadded(remaining, padding)
    }

    def readQuadByteBigEndian(): Int = {
      val c = _cursor
      _cursor = c + 4
      baa.quadByteBigEndian(byteArray, c)
    }

    @inline def readQuadByteBigEndianPadded(padding: Int): Int = {
      val remaining = byteArray.length - _cursor
      if (remaining >= 4) readQuadByteBigEndian()
      else readQuadByteBigEndianPadded(remaining, padding)
    }

    def readOctaByteBigEndian(): Long = {
      val c = _cursor
      _cursor = c + 8
      baa.octaByteBigEndian(byteArray, c)
    }

    @inline def readOctaByteBigEndianPadded(padding: Long): Long = {
      val remaining = byteArray.length - _cursor
      if (remaining >= 8) readOctaByteBigEndian()
      else readOctaByteBigEndianPadded(remaining, padding)
    }

    @inline def readBytes(length: Long): Bytes = {
      val len = length.toInt
      if (length == len) {
        if (len > 0) {
          val result = new Array[Byte](len)
          val c      = _cursor
          _cursor = c + len
          System.arraycopy(byteArray, c, result, 0, len)
          result
        } else Array.emptyByteArray
      } else throw new Borer.Error.Overflow(position(cursor), "Byte-array input is limited to size 2GB")
    }

    @inline def precedingBytesAsAsciiString(length: Int): String =
      new String(byteArray, _cursor - length, length, StandardCharsets.ISO_8859_1)
  }

  implicit object ByteBufferWrapper extends Wrapper[ByteBuffer] {
    type In = FromByteBuffer
    def apply(value: ByteBuffer) = new FromByteBuffer(value)
  }

  final class FromByteBuffer(buffer: ByteBuffer) extends Input {
    type Bytes    = Array[Byte]
    type Position = Input.Position

    // the number of bytes we've already read beyond the limit of the underlying buffer
    private[this] var paddedCount = 0

    @inline def cursor: Long = (buffer.position() + paddedCount).toLong
    @inline def byteAccess   = ByteAccess.ForByteArray

    @inline def resetTo(cursor: Long) = {
      val intLimit  = buffer.limit()
      val longLimit = intLimit.toLong
      paddedCount = if (cursor <= longLimit) {
        buffer.position(cursor.toInt)
        0
      } else {
        buffer.position(intLimit)
        val x = cursor - longLimit
        if (x > Int.MaxValue) throw new IllegalArgumentException
        x.toInt
      }
      this
    }

    @inline def moveCursor(offset: Int): this.type = {
      val targetPos = buffer.position() + paddedCount + offset
      val limit     = buffer.limit()
      paddedCount = if (targetPos <= limit) {
        buffer.position(targetPos)
        0
      } else {
        buffer.position(limit)
        targetPos - limit
      }
      this
    }

    @inline def releaseBeforeCursor(): this.type = this

    def position(cursor: Long): Position = Position(this, cursor)

    @inline def prepareRead(length: Long): Boolean = length <= buffer.remaining

    def readByte(): Byte = buffer.get()

    @inline def readByteOr(byte: Byte): Byte =
      if (!buffer.hasRemaining) {
        paddedCount += 1
        byte
      } else readByte()

    def readDoubleByteBigEndian(): Char = buffer.getChar

    @inline def readDoubleByteBigEndianPadded(padding: Char): Char = {
      val remaining = buffer.remaining
      if (remaining >= 2) readDoubleByteBigEndian()
      else readDoubleByteBigEndianPadded(remaining, padding)
    }

    def readQuadByteBigEndian(): Int = buffer.getInt()

    @inline def readQuadByteBigEndianPadded(padding: Int): Int = {
      val remaining = buffer.remaining
      if (remaining >= 4) readQuadByteBigEndian()
      else readQuadByteBigEndianPadded(remaining, padding)
    }

    def readOctaByteBigEndian(): Long = buffer.getLong()

    @inline def readOctaByteBigEndianPadded(padding: Long): Long = {
      val remaining = buffer.remaining
      if (remaining >= 8) readOctaByteBigEndian()
      else readOctaByteBigEndianPadded(remaining, padding)
    }

    @inline def readBytes(length: Long): Array[Byte] = {
      val len = length.toInt
      if (length == len) {
        if (len > 0) {
          val bytes = new Array[Byte](len)
          buffer.get(bytes, 0, len)
          bytes
        } else ByteAccess.ForByteArray.empty
      } else throw new Borer.Error.Overflow(position(cursor), "Byte-array input is limited to size 2GB")
    }

    @inline def precedingBytesAsAsciiString(length: Int): String = {
      val limit = buffer.limit()
      val pos   = buffer.position()
      moveCursor(-length)
      buffer.limit(pos)
      val result = StandardCharsets.ISO_8859_1.decode(buffer).toString
      buffer.limit(limit)
      result
    }
  }
}
