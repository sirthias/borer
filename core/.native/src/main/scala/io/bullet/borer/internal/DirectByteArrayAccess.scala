/*
 * Copyright (c) 2019-2024 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.internal

import java.lang.Long as JLong
import java.nio.ByteOrder
import scala.annotation.{nowarn, tailrec}
import scala.scalanative.runtime.ffi.memcpy
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.USize

object DirectByteArrayAccess extends ByteArrayAccess:

  if (ByteOrder.nativeOrder() != ByteOrder.LITTLE_ENDIAN)
    throw new UnsupportedOperationException("borer scala-native support is only available for LITTLE_ENDIAN platforms")

  def doubleByteBigEndian(byteArray: Array[Byte], ix: Int): Char =
    Character.reverseBytes(!byteArray.at(ix).asInstanceOf[Ptr[Char]])

  def quadByteBigEndian(byteArray: Array[Byte], ix: Int): Int =
    Integer.reverseBytes(!byteArray.at(ix).asInstanceOf[Ptr[Int]])

  def octaByteBigEndian(byteArray: Array[Byte], ix: Int): Long =
    JLong.reverseBytes(!byteArray.at(ix).asInstanceOf[Ptr[Long]])

  def setDoubleByteBigEndian(byteArray: Array[Byte], ix: Int, value: Char): Unit =
    !byteArray.at(ix).asInstanceOf[Ptr[Char]] = Character.reverseBytes(value)

  def setQuadByteBigEndian(byteArray: Array[Byte], ix: Int, value: Int): Unit =
    !byteArray.at(ix).asInstanceOf[Ptr[Int]] = Integer.reverseBytes(value)

  def setOctaByteBigEndian(byteArray: Array[Byte], ix: Int, value: Long): Unit =
    !byteArray.at(ix).asInstanceOf[Ptr[Long]] = JLong.reverseBytes(value)

  def shortArrayToByteArray(source: Array[Short], byteOrder: ByteOrder): Array[Byte] =
    if (source.length > 0)
      val copySource =
        if (ByteOrder.nativeOrder() != byteOrder)
          val array                               = new Array[Short](source.length)
          @tailrec def rec(ix: Int): Array[Short] =
            if (ix < array.length)
              array(ix) = java.lang.Short.reverseBytes(source(ix))
              rec(ix + 1)
            else array
          rec(0)
        else source
      val target = new Array[Byte](source.length << 1)
      memcpy(target.at(0), copySource.at(0), target.length.toSize.toUSize)
      target
    else Array.emptyByteArray

  def intArrayToByteArray(source: Array[Int], byteOrder: ByteOrder): Array[Byte] =
    if (source.length > 0)
      val copySource =
        if (ByteOrder.nativeOrder() != byteOrder)
          val array                             = new Array[Int](source.length)
          @tailrec def rec(ix: Int): Array[Int] =
            if (ix < array.length)
              array(ix) = java.lang.Integer.reverseBytes(source(ix))
              rec(ix + 1)
            else array
          rec(0)
        else source
      val target = new Array[Byte](source.length << 2)
      memcpy(target.at(0), copySource.at(0), target.length.toSize.toUSize)
      target
    else Array.emptyByteArray

  def longArrayToByteArray(source: Array[Long], byteOrder: ByteOrder): Array[Byte] =
    if (source.length > 0)
      val copySource =
        if (ByteOrder.nativeOrder() != byteOrder)
          val array                              = new Array[Long](source.length)
          @tailrec def rec(ix: Int): Array[Long] =
            if (ix < array.length)
              array(ix) = java.lang.Long.reverseBytes(source(ix))
              rec(ix + 1)
            else array
          rec(0)
        else source
      val target = new Array[Byte](source.length << 3)
      memcpy(target.at(0), copySource.at(0), target.length.toSize.toUSize)
      target
    else Array.emptyByteArray

  def floatArrayToByteArray(source: Array[Float], byteOrder: ByteOrder): Array[Byte] =
    if (source.length > 0)
      val copySource = new Array[Int](source.length)
      if (ByteOrder.nativeOrder() != byteOrder)
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
      memcpy(target.at(0), copySource.at(0), target.length.toSize.toUSize)
      target
    else Array.emptyByteArray

  def doubleArrayToByteArray(source: Array[Double], byteOrder: ByteOrder): Array[Byte] =
    if (source.length > 0)
      val copySource = new Array[Long](source.length)
      if (ByteOrder.nativeOrder() != byteOrder)
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
      memcpy(target.at(0), copySource.at(0), target.length.toSize.toUSize)
      target
    else Array.emptyByteArray

  def byteArrayToShortArray(source: Array[Byte], byteOrder: ByteOrder): Array[Short] =
    if (source.length > 0)
      if ((source.length & 1) != 0)
        throw new IllegalArgumentException(s"source Array[Byte] has illegal length: ${source.length}")
      val target = new Array[Short](source.length >> 1)
      memcpy(target.at(0), source.at(0), source.length.toSize.toUSize)
      if (ByteOrder.nativeOrder() != byteOrder)
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
      memcpy(target.at(0), source.at(0), source.length.toSize.toUSize)
      if (ByteOrder.nativeOrder() != byteOrder)
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
      memcpy(target.at(0), source.at(0), source.length.toSize.toUSize)
      if (ByteOrder.nativeOrder() != byteOrder)
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
      memcpy(ints.at(0), source.at(0), source.length.toSize.toUSize)
      val target = new Array[Float](ints.length)
      if (ByteOrder.nativeOrder() != byteOrder)
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
      memcpy(longs.at(0), source.at(0), source.length.toSize.toUSize)
      val target = new Array[Double](longs.length)
      if (ByteOrder.nativeOrder() != byteOrder)
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
