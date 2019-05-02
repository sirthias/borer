/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import java.lang.{Long ⇒ JLong, Short ⇒ JShort}
import java.nio.ByteOrder
import java.security.PrivilegedExceptionAction

import io.bullet.borer.internal.ByteArrayAccess
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

  def byteArrayInputAccess: InputAccess.ForByteArray =
    if (UNSAFE ne null) {
      ByteOrder.nativeOrder() match {
        case ByteOrder.LITTLE_ENDIAN ⇒ byteArrayInputAccessOnLittleEndian()
        case ByteOrder.BIG_ENDIAN    ⇒ byteArrayInputAccessOnBigEndian()
      }
    } else null

  private def byteArrayInputAccessOnLittleEndian(): InputAccess.ForByteArray =
    new InputAccess.ForByteArray {

      @inline override def unsafeByte(input: Array[Byte], index: Long): Byte =
        UNSAFE.getByte(input, index + BYTE_ARRAY_BASE_OFFSET)

      @inline override def doubleByteBigEndian(input: Array[Byte], index: Long): Int =
        JShort.reverseBytes(UNSAFE.getShort(input, index + BYTE_ARRAY_BASE_OFFSET)) & 0xFFFF

      @inline override def quadByteBigEndian(input: Array[Byte], index: Long): Int =
        Integer.reverseBytes(UNSAFE.getInt(input, index + BYTE_ARRAY_BASE_OFFSET))

      @inline override def octaByteBigEndian(input: Array[Byte], index: Long): Long =
        JLong.reverseBytes(UNSAFE.getLong(input, index + BYTE_ARRAY_BASE_OFFSET))
    }

  private def byteArrayInputAccessOnBigEndian(): InputAccess.ForByteArray =
    new InputAccess.ForByteArray {

      @inline override def unsafeByte(input: Array[Byte], index: Long): Byte =
        UNSAFE.getByte(input, index + BYTE_ARRAY_BASE_OFFSET)

      @inline override def doubleByteBigEndian(input: Array[Byte], index: Long): Int =
        UNSAFE.getShort(input, index + BYTE_ARRAY_BASE_OFFSET) & 0xFFFF

      @inline override def quadByteBigEndian(input: Array[Byte], index: Long): Int =
        UNSAFE.getInt(input, index + BYTE_ARRAY_BASE_OFFSET)

      @inline override def octaByteBigEndian(input: Array[Byte], index: Long): Long =
        UNSAFE.getLong(input, index + BYTE_ARRAY_BASE_OFFSET)
    }

  def byteArrayAccess: ByteArrayAccess =
    if (UNSAFE ne null) {
      ByteOrder.nativeOrder() match {
        case ByteOrder.LITTLE_ENDIAN ⇒ byteArrayAccessOnLittleEndian()
        case ByteOrder.BIG_ENDIAN    ⇒ byteArrayAccessOnBigEndian()
      }
    } else null

  private def byteArrayAccessOnLittleEndian(): ByteArrayAccess =
    new ByteArrayAccess {
      override def putLongBigEndian(buf: Array[Byte], offset: Int, long: Long): Unit =
        UNSAFE.putLong(buf, offset.toLong + BYTE_ARRAY_BASE_OFFSET, JLong.reverseBytes(long))
    }

  private def byteArrayAccessOnBigEndian(): ByteArrayAccess =
    new ByteArrayAccess {
      override def putLongBigEndian(buf: Array[Byte], offset: Int, long: Long): Unit =
        UNSAFE.putLong(buf, offset.toLong + BYTE_ARRAY_BASE_OFFSET, long)
    }
}
