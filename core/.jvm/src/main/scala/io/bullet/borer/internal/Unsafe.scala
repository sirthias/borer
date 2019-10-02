/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.internal

import java.lang.{Long => JLong}
import java.nio.ByteOrder
import java.security.PrivilegedExceptionAction

import sun.misc.{Unsafe => SMUnsafe}

import scala.util.control.NonFatal

object Unsafe {

  final val UNSAFE: SMUnsafe = {
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
  final private val BYTE_ARRAY_BASE_OFFSET =
    if (UNSAFE ne null) UNSAFE.arrayBaseOffset(classOf[Array[Byte]]).toLong else 0L
  final private val SHORT_ARRAY_BASE_OFFSET =
    if (UNSAFE ne null) UNSAFE.arrayBaseOffset(classOf[Array[Short]]).toLong else 0L
  final private val INT_ARRAY_BASE_OFFSET =
    if (UNSAFE ne null) UNSAFE.arrayBaseOffset(classOf[Array[Int]]).toLong else 0L
  final private val LONG_ARRAY_BASE_OFFSET =
    if (UNSAFE ne null) UNSAFE.arrayBaseOffset(classOf[Array[Long]]).toLong else 0L

  def byteArrayAccess: ByteArrayAccess =
    if (UNSAFE ne null) {
      ByteOrder.nativeOrder() match {
        case ByteOrder.LITTLE_ENDIAN => new LittleEndianByteArrayAccess
        case ByteOrder.BIG_ENDIAN    => new BigEndianByteArrayAccess
      }
    } else null

  final class LittleEndianByteArrayAccess extends ByteArrayAccess {
    private val defaultByteAccess = new ByteArrayAccess.Default

    def doubleByteBigEndian(byteArray: Array[Byte], ix: Int): Char =
      Character.reverseBytes(UNSAFE.getChar(byteArray, ix.toLong + BYTE_ARRAY_BASE_OFFSET))

    def quadByteBigEndian(byteArray: Array[Byte], ix: Int): Int =
      Integer.reverseBytes(UNSAFE.getInt(byteArray, ix.toLong + BYTE_ARRAY_BASE_OFFSET))

    def octaByteBigEndian(byteArray: Array[Byte], ix: Int): Long =
      JLong.reverseBytes(UNSAFE.getLong(byteArray, ix.toLong + BYTE_ARRAY_BASE_OFFSET))

    def setDoubleByteBigEndian(byteArray: Array[Byte], ix: Int, value: Char): Unit =
      UNSAFE.putChar(byteArray, ix.toLong + BYTE_ARRAY_BASE_OFFSET, Character.reverseBytes(value))

    def setQuadByteBigEndian(byteArray: Array[Byte], ix: Int, value: Int): Unit =
      UNSAFE.putInt(byteArray, ix.toLong + BYTE_ARRAY_BASE_OFFSET, Integer.reverseBytes(value))

    def setOctaByteBigEndian(byteArray: Array[Byte], ix: Int, value: Long): Unit =
      UNSAFE.putLong(byteArray, ix.toLong + BYTE_ARRAY_BASE_OFFSET, JLong.reverseBytes(value))

    def shortArrayToByteArray(source: Array[Short], byteOrder: ByteOrder) =
      if (byteOrder == ByteOrder.LITTLE_ENDIAN && source.length > 0) {
        val target = new Array[Byte](source.length << 1)
        UNSAFE.copyMemory(source, SHORT_ARRAY_BASE_OFFSET, target, BYTE_ARRAY_BASE_OFFSET, target.length.toLong)
        target
      } else defaultByteAccess.shortArrayToByteArray(source, byteOrder)

    def intArrayToByteArray(source: Array[Int], byteOrder: ByteOrder) =
      if (byteOrder == ByteOrder.LITTLE_ENDIAN && source.length > 0) {
        val target = new Array[Byte](source.length << 2)
        UNSAFE.copyMemory(source, INT_ARRAY_BASE_OFFSET, target, BYTE_ARRAY_BASE_OFFSET, target.length.toLong)
        target
      } else defaultByteAccess.intArrayToByteArray(source, byteOrder)

    def longArrayToByteArray(source: Array[Long], byteOrder: ByteOrder) =
      if (byteOrder == ByteOrder.LITTLE_ENDIAN && source.length > 0) {
        val target = new Array[Byte](source.length << 3)
        UNSAFE.copyMemory(source, LONG_ARRAY_BASE_OFFSET, target, BYTE_ARRAY_BASE_OFFSET, target.length.toLong)
        target
      } else defaultByteAccess.longArrayToByteArray(source, byteOrder)

    def floatArrayToByteArray(source: Array[Float], byteOrder: ByteOrder) =
      defaultByteAccess.floatArrayToByteArray(source, byteOrder)

    def doubleArrayToByteArray(source: Array[Double], byteOrder: ByteOrder) =
      defaultByteAccess.doubleArrayToByteArray(source, byteOrder)

    def byteArrayToShortArray(source: Array[Byte], byteOrder: ByteOrder) =
      if (byteOrder == ByteOrder.LITTLE_ENDIAN && source.length > 0) {
        val target = new Array[Short](source.length >> 1)
        UNSAFE.copyMemory(source, BYTE_ARRAY_BASE_OFFSET, target, SHORT_ARRAY_BASE_OFFSET, target.length.toLong << 1)
        target
      } else defaultByteAccess.byteArrayToShortArray(source, byteOrder)

    def byteArrayToIntArray(source: Array[Byte], byteOrder: ByteOrder) =
      if (byteOrder == ByteOrder.LITTLE_ENDIAN && source.length > 0) {
        val target = new Array[Int](source.length >> 2)
        UNSAFE.copyMemory(source, BYTE_ARRAY_BASE_OFFSET, target, INT_ARRAY_BASE_OFFSET, target.length.toLong << 2)
        target
      } else defaultByteAccess.byteArrayToIntArray(source, byteOrder)

    def byteArrayToLongArray(source: Array[Byte], byteOrder: ByteOrder) =
      if (byteOrder == ByteOrder.LITTLE_ENDIAN && source.length > 0) {
        val target = new Array[Long](source.length >> 3)
        UNSAFE.copyMemory(source, BYTE_ARRAY_BASE_OFFSET, target, LONG_ARRAY_BASE_OFFSET, target.length.toLong << 3)
        target
      } else defaultByteAccess.byteArrayToLongArray(source, byteOrder)

    def byteArrayToFloatArray(source: Array[Byte], byteOrder: ByteOrder) =
      defaultByteAccess.byteArrayToFloatArray(source, byteOrder)

    def byteArrayToDoubleArray(source: Array[Byte], byteOrder: ByteOrder) =
      defaultByteAccess.byteArrayToDoubleArray(source, byteOrder)
  }

  final class BigEndianByteArrayAccess extends ByteArrayAccess {
    private val defaultByteAccess = new ByteArrayAccess.Default

    def doubleByteBigEndian(byteArray: Array[Byte], ix: Int): Char =
      UNSAFE.getChar(byteArray, ix.toLong + BYTE_ARRAY_BASE_OFFSET)

    def quadByteBigEndian(byteArray: Array[Byte], ix: Int): Int =
      UNSAFE.getInt(byteArray, ix.toLong + BYTE_ARRAY_BASE_OFFSET)

    def octaByteBigEndian(byteArray: Array[Byte], ix: Int): Long =
      UNSAFE.getLong(byteArray, ix.toLong + BYTE_ARRAY_BASE_OFFSET)

    def setDoubleByteBigEndian(byteArray: Array[Byte], ix: Int, value: Char): Unit =
      UNSAFE.putChar(byteArray, ix.toLong + BYTE_ARRAY_BASE_OFFSET, value)

    def setQuadByteBigEndian(byteArray: Array[Byte], ix: Int, value: Int): Unit =
      UNSAFE.putInt(byteArray, ix.toLong + BYTE_ARRAY_BASE_OFFSET, value)

    def setOctaByteBigEndian(byteArray: Array[Byte], ix: Int, value: Long): Unit =
      UNSAFE.putLong(byteArray, ix.toLong + BYTE_ARRAY_BASE_OFFSET, value)

    def shortArrayToByteArray(source: Array[Short], byteOrder: ByteOrder) =
      if (byteOrder == ByteOrder.BIG_ENDIAN && source.length > 0) {
        val target = new Array[Byte](source.length << 1)
        UNSAFE.copyMemory(source, SHORT_ARRAY_BASE_OFFSET, target, BYTE_ARRAY_BASE_OFFSET, target.length.toLong)
        target
      } else defaultByteAccess.shortArrayToByteArray(source, byteOrder)

    def intArrayToByteArray(source: Array[Int], byteOrder: ByteOrder) =
      if (byteOrder == ByteOrder.BIG_ENDIAN && source.length > 0) {
        val target = new Array[Byte](source.length << 2)
        UNSAFE.copyMemory(source, INT_ARRAY_BASE_OFFSET, target, BYTE_ARRAY_BASE_OFFSET, target.length.toLong)
        target
      } else defaultByteAccess.intArrayToByteArray(source, byteOrder)

    def longArrayToByteArray(source: Array[Long], byteOrder: ByteOrder) =
      if (byteOrder == ByteOrder.BIG_ENDIAN && source.length > 0) {
        val target = new Array[Byte](source.length << 3)
        UNSAFE.copyMemory(source, LONG_ARRAY_BASE_OFFSET, target, BYTE_ARRAY_BASE_OFFSET, target.length.toLong)
        target
      } else defaultByteAccess.longArrayToByteArray(source, byteOrder)

    def floatArrayToByteArray(source: Array[Float], byteOrder: ByteOrder) =
      defaultByteAccess.floatArrayToByteArray(source, byteOrder)

    def doubleArrayToByteArray(source: Array[Double], byteOrder: ByteOrder) =
      defaultByteAccess.doubleArrayToByteArray(source, byteOrder)

    def byteArrayToShortArray(source: Array[Byte], byteOrder: ByteOrder) =
      if (byteOrder == ByteOrder.BIG_ENDIAN && source.length > 0) {
        val target = new Array[Short](source.length >> 1)
        UNSAFE.copyMemory(source, BYTE_ARRAY_BASE_OFFSET, target, SHORT_ARRAY_BASE_OFFSET, target.length.toLong << 1)
        target
      } else defaultByteAccess.byteArrayToShortArray(source, byteOrder)

    def byteArrayToIntArray(source: Array[Byte], byteOrder: ByteOrder) =
      if (byteOrder == ByteOrder.BIG_ENDIAN && source.length > 0) {
        val target = new Array[Int](source.length >> 2)
        UNSAFE.copyMemory(source, BYTE_ARRAY_BASE_OFFSET, target, INT_ARRAY_BASE_OFFSET, target.length.toLong << 2)
        target
      } else defaultByteAccess.byteArrayToIntArray(source, byteOrder)

    def byteArrayToLongArray(source: Array[Byte], byteOrder: ByteOrder) =
      if (byteOrder == ByteOrder.BIG_ENDIAN && source.length > 0) {
        val target = new Array[Long](source.length >> 3)
        UNSAFE.copyMemory(source, BYTE_ARRAY_BASE_OFFSET, target, LONG_ARRAY_BASE_OFFSET, target.length.toLong << 3)
        target
      } else defaultByteAccess.byteArrayToLongArray(source, byteOrder)

    def byteArrayToFloatArray(source: Array[Byte], byteOrder: ByteOrder) =
      defaultByteAccess.byteArrayToFloatArray(source, byteOrder)

    def byteArrayToDoubleArray(source: Array[Byte], byteOrder: ByteOrder) =
      defaultByteAccess.byteArrayToDoubleArray(source, byteOrder)
  }
}
