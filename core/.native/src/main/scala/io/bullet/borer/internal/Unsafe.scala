/*
 * Copyright (c) 2019-2024 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.internal

import java.lang.{Long => JLong}
import java.nio.ByteOrder

import scala.scalanative.unsafe._

import scala.annotation.{nowarn, tailrec}
import scala.util.control.NonFatal
import scala.scalanative.unsigned.USize

object Unsafe:
  import NativeCopy.memcpy

  @extern
  private[Unsafe] object NativeCopy:
    def memcpy(dst: Ptr[Byte], src: Ptr[Byte], n: CSize): Ptr[Byte] = extern

  def byteArrayAccess: ByteArrayAccess =
    ByteOrder.nativeOrder() match
      case ByteOrder.LITTLE_ENDIAN => new LittleEndianByteArrayAccess
      case ByteOrder.BIG_ENDIAN    => new BigEndianByteArrayAccess
      case _                       => throw new IllegalStateException

  sealed abstract class UnsafeByteArrayAccess(byteOrder: ByteOrder) extends ByteArrayAccess:

    final protected def _doubleByteBigEndian(byteArray: Array[Byte], ix: Int): Char =
      // UNSAFE.getChar(byteArray, ix.toLong)
      !byteArray.at(ix).asInstanceOf[Ptr[Char]]

    final protected def _quadByteBigEndian(byteArray: Array[Byte], ix: Int): Int =
      // UNSAFE.getInt(byteArray, ix.toLong)
      !byteArray.at(ix).asInstanceOf[Ptr[Int]]

    final protected def _octaByteBigEndian(byteArray: Array[Byte], ix: Int): Long =
      // UNSAFE.getLong(byteArray, ix.toLong)
      !byteArray.at(ix).asInstanceOf[Ptr[Long]]

    final protected def _setDoubleByteBigEndian(byteArray: Array[Byte], ix: Int, value: Char): Unit =
      // UNSAFE.putChar(byteArray, ix.toLong, value)
      !byteArray.at(ix).asInstanceOf[Ptr[Char]] = value

    final protected def _setQuadByteBigEndian(byteArray: Array[Byte], ix: Int, value: Int): Unit =
      // UNSAFE.putInt(byteArray, ix.toLong, value)
      !byteArray.at(ix).asInstanceOf[Ptr[Int]] = value

    final protected def _setOctaByteBigEndian(byteArray: Array[Byte], ix: Int, value: Long): Unit =
      // UNSAFE.putLong(byteArray, ix.toLong, value)
      !byteArray.at(ix).asInstanceOf[Ptr[Long]] = value

    def shortArrayToByteArray(source: Array[Short], byteOrder: ByteOrder): Array[Byte] =
      if (source.length > 0)
        val copySource =
          if (this.byteOrder != byteOrder)
            val array = new Array[Short](source.length)
            @tailrec def rec(ix: Int): Array[Short] =
              if (ix < array.length)
                array(ix) = java.lang.Short.reverseBytes(source(ix))
                rec(ix + 1)
              else array
            rec(0)
          else source
        val target = new Array[Byte](source.length << 1)
        // UNSAFE.copyMemory(copySource, 0, target, 0, target.length.toLong)
        memcpy(target.at(0), copySource.at(0).asInstanceOf[Ptr[Byte]], target.length.toSize.toUSize)
        target
      else Array.emptyByteArray

    def intArrayToByteArray(source: Array[Int], byteOrder: ByteOrder): Array[Byte] =
      if (source.length > 0)
        val copySource =
          if (this.byteOrder != byteOrder)
            val array = new Array[Int](source.length)
            @tailrec def rec(ix: Int): Array[Int] =
              if (ix < array.length)
                array(ix) = java.lang.Integer.reverseBytes(source(ix))
                rec(ix + 1)
              else array
            rec(0)
          else source
        val target = new Array[Byte](source.length << 2)
        // UNSAFE.copyMemory(copySource, 0, target, 0, target.length.toLong)
        memcpy(target.at(0), copySource.at(0).asInstanceOf[Ptr[Byte]], target.length.toSize.toUSize)
        target
      else Array.emptyByteArray

    def longArrayToByteArray(source: Array[Long], byteOrder: ByteOrder): Array[Byte] =
      if (source.length > 0)
        val copySource =
          if (this.byteOrder != byteOrder)
            val array = new Array[Long](source.length)
            @tailrec def rec(ix: Int): Array[Long] =
              if (ix < array.length)
                array(ix) = java.lang.Long.reverseBytes(source(ix))
                rec(ix + 1)
              else array
            rec(0)
          else source
        val target = new Array[Byte](source.length << 3)
        // UNSAFE.copyMemory(copySource, 0, target, 0, target.length.toLong)
        memcpy(target.at(0), copySource.at(0).asInstanceOf[Ptr[Byte]], target.length.toSize.toUSize)
        target
      else Array.emptyByteArray

    def floatArrayToByteArray(source: Array[Float], byteOrder: ByteOrder): Array[Byte] =
      if (source.length > 0)
        val copySource = new Array[Int](source.length)
        if (this.byteOrder != byteOrder)
          @tailrec def rec(ix: Int): Array[Int] =
            if (ix < copySource.length)
              copySource(ix) = java.lang.Integer.reverseBytes(java.lang.Float.floatToIntBits(source(ix)))
              rec(ix + 1)
            else copySource
          rec(0)
        else
          @tailrec def rec(ix: Int): Array[Int] =
            if (ix < copySource.length)
              copySource(ix) = java.lang.Float.floatToIntBits(source(ix))
              rec(ix + 1)
            else copySource
          rec(0)
        val target = new Array[Byte](source.length << 2)
        // UNSAFE.copyMemory(copySource, 0, target, 0, target.length.toLong)
        memcpy(target.at(0), copySource.at(0).asInstanceOf[Ptr[Byte]], target.length.toSize.toUSize)
        target
      else Array.emptyByteArray

    def doubleArrayToByteArray(source: Array[Double], byteOrder: ByteOrder): Array[Byte] =
      if (source.length > 0)
        val copySource = new Array[Long](source.length)
        if (this.byteOrder != byteOrder)
          @tailrec def rec(ix: Int): Array[Long] =
            if (ix < copySource.length)
              copySource(ix) = java.lang.Long.reverseBytes(java.lang.Double.doubleToLongBits(source(ix)))
              rec(ix + 1)
            else copySource
          rec(0)
        else
          @tailrec def rec(ix: Int): Array[Long] =
            if (ix < copySource.length)
              copySource(ix) = java.lang.Double.doubleToLongBits(source(ix))
              rec(ix + 1)
            else copySource
          rec(0)
        val target = new Array[Byte](source.length << 3)
        // UNSAFE.copyMemory(copySource, 0, target, 0, target.length.toLong)
        memcpy(target.at(0), copySource.at(0).asInstanceOf[Ptr[Byte]], target.length.toSize.toUSize)
        target
      else Array.emptyByteArray

    def byteArrayToShortArray(source: Array[Byte], byteOrder: ByteOrder): Array[Short] =
      if (source.length > 0)
        if ((source.length & 1) != 0)
          throw new IllegalArgumentException(s"source Array[Byte] has illegal length: ${source.length}")
        val target = new Array[Short](source.length >> 1)
        // UNSAFE.copyMemory(source, 0, target, 0, source.length.toLong)
        memcpy(target.at(0).asInstanceOf[Ptr[Byte]], source.at(0), source.length.toSize.toUSize)
        if (this.byteOrder != byteOrder)
          @tailrec def rec(ix: Int): Array[Short] =
            if (ix < target.length)
              target(ix) = java.lang.Short.reverseBytes(target(ix))
              rec(ix + 1)
            else target
          rec(0)
        else target
      else Array.emptyShortArray

    def byteArrayToIntArray(source: Array[Byte], byteOrder: ByteOrder): Array[Int] =
      if (source.length > 0)
        if ((source.length & 3) != 0)
          throw new IllegalArgumentException(s"source Array[Byte] has illegal length: ${source.length}")
        val target = new Array[Int](source.length >> 2)
        // UNSAFE.copyMemory(source, 0, target, 0, source.length.toLong)
        memcpy(target.at(0).asInstanceOf[Ptr[Byte]], source.at(0), source.length.toSize.toUSize)
        if (this.byteOrder != byteOrder)
          @tailrec def rec(ix: Int): Array[Int] =
            if (ix < target.length)
              target(ix) = java.lang.Integer.reverseBytes(target(ix))
              rec(ix + 1)
            else target
          rec(0)
        else target
      else Array.emptyIntArray

    def byteArrayToLongArray(source: Array[Byte], byteOrder: ByteOrder): Array[Long] =
      if (source.length > 0)
        if ((source.length & 7) != 0)
          throw new IllegalArgumentException(s"source Array[Byte] has illegal length: ${source.length}")
        val target = new Array[Long](source.length >> 3)
        // UNSAFE.copyMemory(source, 0, target, 0, source.length.toLong)
        memcpy(target.at(0).asInstanceOf[Ptr[Byte]], source.at(0), source.length.toSize.toUSize)
        if (this.byteOrder != byteOrder)
          @tailrec def rec(ix: Int): Array[Long] =
            if (ix < target.length)
              target(ix) = java.lang.Long.reverseBytes(target(ix))
              rec(ix + 1)
            else target
          rec(0)
        else target
      else Array.emptyLongArray

    def byteArrayToFloatArray(source: Array[Byte], byteOrder: ByteOrder): Array[Float] =
      if (source.length > 0)
        if ((source.length & 3) != 0)
          throw new IllegalArgumentException(s"source Array[Byte] has illegal length: ${source.length}")
        val ints = new Array[Int](source.length >> 2)
        // UNSAFE.copyMemory(source, 0, ints, 0, source.length.toLong)
        memcpy(ints.at(0).asInstanceOf[Ptr[Byte]], source.at(0), source.length.toSize.toUSize)
        val target = new Array[Float](ints.length)
        if (this.byteOrder != byteOrder)
          @tailrec def rec(ix: Int): Array[Float] =
            if (ix < target.length)
              target(ix) = java.lang.Float.intBitsToFloat(java.lang.Integer.reverseBytes(ints(ix)))
              rec(ix + 1)
            else target
          rec(0)
        else
          @tailrec def rec(ix: Int): Array[Float] =
            if (ix < target.length)
              target(ix) = java.lang.Float.intBitsToFloat(ints(ix))
              rec(ix + 1)
            else target
          rec(0)
      else Array.emptyFloatArray

    def byteArrayToDoubleArray(source: Array[Byte], byteOrder: ByteOrder): Array[Double] =
      if (source.length > 0)
        if ((source.length & 7) != 0)
          throw new IllegalArgumentException(s"source Array[Byte] has illegal length: ${source.length}")
        val longs = new Array[Long](source.length >> 3)
        // UNSAFE.copyMemory(source, 0, longs, 0, source.length.toLong)
        memcpy(longs.at(0).asInstanceOf[Ptr[Byte]], source.at(0), source.length.toSize.toUSize)
        val target = new Array[Double](longs.length)
        if (this.byteOrder != byteOrder)
          @tailrec def rec(ix: Int): Array[Double] =
            if (ix < target.length)
              target(ix) = java.lang.Double.longBitsToDouble(java.lang.Long.reverseBytes(longs(ix)))
              rec(ix + 1)
            else target
          rec(0)
        else
          @tailrec def rec(ix: Int): Array[Double] =
            if (ix < target.length)
              target(ix) = java.lang.Double.longBitsToDouble(longs(ix))
              rec(ix + 1)
            else target
          rec(0)
      else Array.emptyDoubleArray

  final class LittleEndianByteArrayAccess extends UnsafeByteArrayAccess(ByteOrder.LITTLE_ENDIAN):

    def doubleByteBigEndian(byteArray: Array[Byte], ix: Int): Char =
      Character.reverseBytes(_doubleByteBigEndian(byteArray, ix))

    def quadByteBigEndian(byteArray: Array[Byte], ix: Int): Int =
      Integer.reverseBytes(_quadByteBigEndian(byteArray, ix))

    def octaByteBigEndian(byteArray: Array[Byte], ix: Int): Long =
      JLong.reverseBytes(_octaByteBigEndian(byteArray, ix))

    def setDoubleByteBigEndian(byteArray: Array[Byte], ix: Int, value: Char): Unit =
      _setDoubleByteBigEndian(byteArray, ix, Character.reverseBytes(value))

    def setQuadByteBigEndian(byteArray: Array[Byte], ix: Int, value: Int): Unit =
      _setQuadByteBigEndian(byteArray, ix, Integer.reverseBytes(value))

    def setOctaByteBigEndian(byteArray: Array[Byte], ix: Int, value: Long): Unit =
      _setOctaByteBigEndian(byteArray, ix, JLong.reverseBytes(value))

  final class BigEndianByteArrayAccess extends UnsafeByteArrayAccess(ByteOrder.LITTLE_ENDIAN):

    def doubleByteBigEndian(byteArray: Array[Byte], ix: Int): Char =
      _doubleByteBigEndian(byteArray, ix)

    def quadByteBigEndian(byteArray: Array[Byte], ix: Int): Int =
      _quadByteBigEndian(byteArray, ix)

    def octaByteBigEndian(byteArray: Array[Byte], ix: Int): Long =
      _octaByteBigEndian(byteArray, ix)

    def setDoubleByteBigEndian(byteArray: Array[Byte], ix: Int, value: Char): Unit =
      _setDoubleByteBigEndian(byteArray, ix, value)

    def setQuadByteBigEndian(byteArray: Array[Byte], ix: Int, value: Int): Unit =
      _setQuadByteBigEndian(byteArray, ix, value)

    def setOctaByteBigEndian(byteArray: Array[Byte], ix: Int, value: Long): Unit =
      _setOctaByteBigEndian(byteArray, ix, value)
