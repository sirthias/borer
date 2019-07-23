/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import java.lang.{Long => JLong}
import java.nio.ByteOrder
import java.security.PrivilegedExceptionAction

import io.bullet.borer.internal.ByteArrayAccess
import sun.misc.{Unsafe => SMUnsafe}

import scala.util.control.NonFatal

object Unsafe {

  final private val UNSAFE: SMUnsafe = {
    try {
      SMUnsafe.getUnsafe
    } catch {
      case NonFatal(_) =>
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
          case NonFatal(_) =>
            try {
              val constructor = classOf[SMUnsafe].getDeclaredConstructor()
              constructor.setAccessible(true)
              constructor.newInstance()
            } catch {
              case NonFatal(_) => null
            }
        }
    }
  }

  // the offset to the first element in a byte array.
  final private val BYTE_ARRAY_BASE_OFFSET = if (UNSAFE ne null) UNSAFE.arrayBaseOffset(classOf[Array[Byte]]) else 0

  def byteArrayAccess: ByteArrayAccess =
    if (UNSAFE ne null) {
      ByteOrder.nativeOrder() match {
        case ByteOrder.LITTLE_ENDIAN => byteArrayAccessOnLittleEndian()
        case ByteOrder.BIG_ENDIAN    => byteArrayAccessOnBigEndian()
      }
    } else null

  private def byteArrayAccessOnLittleEndian(): ByteArrayAccess =
    new ByteArrayAccess {

      def doubleByteBigEndian(byteArray: Array[Byte], ix: Int): Char =
        Character.reverseBytes(UNSAFE.getChar(byteArray, (ix + BYTE_ARRAY_BASE_OFFSET).toLong))

      def quadByteBigEndian(byteArray: Array[Byte], ix: Int): Int =
        Integer.reverseBytes(UNSAFE.getInt(byteArray, (ix + BYTE_ARRAY_BASE_OFFSET).toLong))

      def octaByteBigEndian(byteArray: Array[Byte], ix: Int): Long =
        JLong.reverseBytes(UNSAFE.getLong(byteArray, (ix + BYTE_ARRAY_BASE_OFFSET).toLong))

      def setDoubleByteBigEndian(byteArray: Array[Byte], ix: Int, value: Char): Unit =
        UNSAFE.putChar(byteArray, (ix + BYTE_ARRAY_BASE_OFFSET).toLong, Character.reverseBytes(value))

      def setQuadByteBigEndian(byteArray: Array[Byte], ix: Int, value: Int): Unit =
        UNSAFE.putInt(byteArray, (ix + BYTE_ARRAY_BASE_OFFSET).toLong, Integer.reverseBytes(value))

      def setOctaByteBigEndian(byteArray: Array[Byte], ix: Int, value: Long): Unit =
        UNSAFE.putLong(byteArray, (ix + BYTE_ARRAY_BASE_OFFSET).toLong, JLong.reverseBytes(value))
    }

  private def byteArrayAccessOnBigEndian(): ByteArrayAccess =
    new ByteArrayAccess {

      def doubleByteBigEndian(byteArray: Array[Byte], ix: Int): Char =
        UNSAFE.getChar(byteArray, (ix + BYTE_ARRAY_BASE_OFFSET).toLong)

      def quadByteBigEndian(byteArray: Array[Byte], ix: Int): Int =
        UNSAFE.getInt(byteArray, (ix + BYTE_ARRAY_BASE_OFFSET).toLong)

      def octaByteBigEndian(byteArray: Array[Byte], ix: Int): Long =
        UNSAFE.getLong(byteArray, (ix + BYTE_ARRAY_BASE_OFFSET).toLong)

      def setDoubleByteBigEndian(byteArray: Array[Byte], ix: Int, value: Char): Unit =
        UNSAFE.putChar(byteArray, (ix + BYTE_ARRAY_BASE_OFFSET).toLong, value)

      def setQuadByteBigEndian(byteArray: Array[Byte], ix: Int, value: Int): Unit =
        UNSAFE.putInt(byteArray, (ix + BYTE_ARRAY_BASE_OFFSET).toLong, value)

      def setOctaByteBigEndian(byteArray: Array[Byte], ix: Int, value: Long): Unit =
        UNSAFE.putLong(byteArray, (ix + BYTE_ARRAY_BASE_OFFSET).toLong, value)
    }
}
