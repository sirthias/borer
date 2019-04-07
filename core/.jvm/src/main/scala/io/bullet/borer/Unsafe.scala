/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import java.nio.ByteOrder
import java.lang.{Long ⇒ JLong, Short ⇒ JShort}
import java.security.PrivilegedExceptionAction

import io.bullet.borer.internal.CharArrayOut
import sun.misc.{Unsafe ⇒ SMUnsafe}

import scala.util.control.NonFatal

object Unsafe {

  private final val UNSAFE: SMUnsafe = {
    try {
      SMUnsafe.getUnsafe
    } catch {
      case NonFatal(_) ⇒
        try {
          java.security.AccessController.doPrivileged {
            new PrivilegedExceptionAction[SMUnsafe] {
              def run() = {
                val field = classOf[SMUnsafe].getDeclaredField("theUnsafe")
                field.setAccessible(true)
                field.get(null).asInstanceOf[SMUnsafe]
              }
            }
          }
        } catch {
          case NonFatal(_) ⇒
            try {
              val constructor = classOf[SMUnsafe].getDeclaredConstructor()
              constructor.setAccessible(true)
              constructor.newInstance()
            } catch {
              case NonFatal(_) ⇒ null
            }
        }
    }
  }

  // the offset to the first element in a byte array.
  private final val BYTE_ARRAY_BASE_OFFSET = if (UNSAFE ne null) UNSAFE.arrayBaseOffset(classOf[Array[Byte]]) else 0

  // the offset to the first element in a char array.
  private final val CHAR_ARRAY_BASE_OFFSET = if (UNSAFE ne null) UNSAFE.arrayBaseOffset(classOf[Array[Char]]) else 0

  def byteArrayInputAccess: InputAccess[Array[Byte]] { type Bytes = Array[Byte] } =
    if (UNSAFE ne null) {
      ByteOrder.nativeOrder() match {
        case ByteOrder.LITTLE_ENDIAN ⇒ byteArrayInputAccessOnLittleEndian()
        case ByteOrder.BIG_ENDIAN    ⇒ byteArrayInputAccessOnBigEndian()
      }
    } else null

  private def byteArrayInputAccessOnLittleEndian(): InputAccess[Array[Byte]] { type Bytes = Array[Byte] } =
    new InputAccess[Array[Byte]] {
      type Bytes = Array[Byte]

      def byteAccess = ByteAccess.ForByteArray

      def length(input: Array[Byte]): Long = input.length.toLong

      def safeByte(input: Array[Byte], index: Long): Byte =
        input(index.toInt) // we actually need the range check

      def unsafeByte(input: Array[Byte], index: Long): Byte =
        UNSAFE.getByte(input, index + BYTE_ARRAY_BASE_OFFSET)

      def doubleByteBigEndian(input: Array[Byte], index: Long): Int =
        JShort.reverseBytes(UNSAFE.getShort(input, index + BYTE_ARRAY_BASE_OFFSET)) & 0xFFFF

      @inline def quadByteBigEndian(input: Array[Byte], index: Long): Int =
        Integer.reverseBytes(UNSAFE.getInt(input, index + BYTE_ARRAY_BASE_OFFSET))

      @inline def octaByteBigEndian(input: Array[Byte], index: Long): Long =
        JLong.reverseBytes(UNSAFE.getLong(input, index + BYTE_ARRAY_BASE_OFFSET))

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

  private def byteArrayInputAccessOnBigEndian(): InputAccess[Array[Byte]] { type Bytes = Array[Byte] } =
    new InputAccess[Array[Byte]] {
      type Bytes = Array[Byte]

      def byteAccess = ByteAccess.ForByteArray

      def length(input: Array[Byte]): Long = input.length.toLong

      def safeByte(input: Array[Byte], index: Long): Byte =
        input(index.toInt) // we actually need the range check

      def unsafeByte(input: Array[Byte], index: Long): Byte =
        UNSAFE.getByte(input, index + BYTE_ARRAY_BASE_OFFSET)

      def doubleByteBigEndian(input: Array[Byte], index: Long): Int =
        UNSAFE.getShort(input, index + BYTE_ARRAY_BASE_OFFSET) & 0xFFFF

      @inline def quadByteBigEndian(input: Array[Byte], index: Long): Int =
        UNSAFE.getInt(input, index + BYTE_ARRAY_BASE_OFFSET)

      @inline def octaByteBigEndian(input: Array[Byte], index: Long): Long =
        UNSAFE.getLong(input, index + BYTE_ARRAY_BASE_OFFSET)

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

  def charArrayOut: CharArrayOut =
    if (UNSAFE ne null) {
      ByteOrder.nativeOrder() match {
        case ByteOrder.LITTLE_ENDIAN ⇒ charArrayOutOnLittleEndian()
        case ByteOrder.BIG_ENDIAN    ⇒ charArrayOutOnBigEndian()
      }
    } else null

  private def charArrayOutOnLittleEndian(): CharArrayOut =
    new CharArrayOut {
      def writeChar(buf: Array[Char], index: Int, c: Char): Unit =
        UNSAFE.putChar(buf, (index.toLong << 1) + CHAR_ARRAY_BASE_OFFSET, c)

      def write2(buf: Array[Char], index: Int, octa: Long): Unit =
        UNSAFE.putInt(
          buf,
          (index.toLong << 1) + CHAR_ARRAY_BASE_OFFSET,
          (octa >>> 56).toInt | ((octa >>> 32).toInt & 0xFF0000))

      def write3(buf: Array[Char], index: Int, octa: Long): Unit = {
        write2(buf, index, octa)
        writeChar(buf, index + 2, ((octa >>> 40) & 0xFFL).toChar)
      }

      def write4(buf: Array[Char], index: Int, octa: Long): Unit =
        UNSAFE.putLong(
          buf,
          (index.toLong << 1) + CHAR_ARRAY_BASE_OFFSET,
          (octa >>> 56) |
            ((octa >>> 32) & 0xFF0000L) |
            ((octa >>> 8) & 0xFF00000000L) |
            ((octa << 16) & 0xFF000000000000L))

      def write5(buf: Array[Char], index: Int, octa: Long): Unit = {
        write4(buf, index, octa)
        writeChar(buf, index + 4, ((octa >>> 24) & 0xFFL).toChar)
      }

      def write6(buf: Array[Char], index: Int, octa: Long): Unit = {
        write4(buf, index, octa)
        write2(buf, index + 4, octa << 32)
      }

      def write7(buf: Array[Char], index: Int, octa: Long): Unit = {
        write4(buf, index, octa)
        write3(buf, index + 4, octa << 32)
      }

      def write8(buf: Array[Char], index: Int, octa: Long): Unit = {
        write4(buf, index, octa)
        write4(buf, index + 4, octa << 32)
      }
    }

  private def charArrayOutOnBigEndian(): CharArrayOut = null // TODO
}
