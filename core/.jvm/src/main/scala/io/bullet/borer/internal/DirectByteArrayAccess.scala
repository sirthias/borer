/*
 * Copyright (c) 2019-2026 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.internal

import java.lang.invoke.{MethodHandles, VarHandle}
import java.nio.{ByteBuffer, ByteOrder}

object DirectByteArrayAccess extends ByteArrayAccess:

  private val CharHandleBE: VarHandle = MethodHandles.byteArrayViewVarHandle(classOf[Array[Char]], ByteOrder.BIG_ENDIAN)
  private val IntHandleBE: VarHandle  = MethodHandles.byteArrayViewVarHandle(classOf[Array[Int]], ByteOrder.BIG_ENDIAN)
  private val LongHandleBE: VarHandle = MethodHandles.byteArrayViewVarHandle(classOf[Array[Long]], ByteOrder.BIG_ENDIAN)

  def doubleByteBigEndian(byteArray: Array[Byte], ix: Int): Char = CharHandleBE.get(byteArray, ix).asInstanceOf[Char]

  def quadByteBigEndian(byteArray: Array[Byte], ix: Int): Int = IntHandleBE.get(byteArray, ix).asInstanceOf[Int]

  def octaByteBigEndian(byteArray: Array[Byte], ix: Int): Long = LongHandleBE.get(byteArray, ix).asInstanceOf[Long]

  def setDoubleByteBigEndian(byteArray: Array[Byte], ix: Int, value: Char): Unit =
    CharHandleBE.set(byteArray, ix, value)

  def setQuadByteBigEndian(byteArray: Array[Byte], ix: Int, value: Int): Unit =
    IntHandleBE.set(byteArray, ix, value)

  def setOctaByteBigEndian(byteArray: Array[Byte], ix: Int, value: Long): Unit =
    LongHandleBE.set(byteArray, ix, value)

  def shortArrayToByteArray(source: Array[Short], byteOrder: ByteOrder): Array[Byte] =
    if (source.nonEmpty) {
      val target = new Array[Byte](Math.multiplyExact(source.length, java.lang.Short.BYTES))
      ByteBuffer
        .wrap(target)
        .order(byteOrder)
        .asShortBuffer()
        .put(source)
      target
    } else Array.emptyByteArray

  def intArrayToByteArray(source: Array[Int], byteOrder: ByteOrder): Array[Byte] =
    if (source.nonEmpty) {
      val target = new Array[Byte](Math.multiplyExact(source.length, java.lang.Integer.BYTES))
      ByteBuffer
        .wrap(target)
        .order(byteOrder)
        .asIntBuffer()
        .put(source)
      target
    } else Array.emptyByteArray

  def longArrayToByteArray(source: Array[Long], byteOrder: ByteOrder): Array[Byte] =
    if (source.nonEmpty) {
      val target = new Array[Byte](Math.multiplyExact(source.length, java.lang.Long.BYTES))
      ByteBuffer
        .wrap(target)
        .order(byteOrder)
        .asLongBuffer()
        .put(source)
      target
    } else Array.emptyByteArray

  def floatArrayToByteArray(source: Array[Float], byteOrder: ByteOrder): Array[Byte] =
    if (source.nonEmpty) {
      val target = new Array[Byte](Math.multiplyExact(source.length, java.lang.Float.BYTES))
      ByteBuffer
        .wrap(target)
        .order(byteOrder)
        .asFloatBuffer()
        .put(source)
      target
    } else Array.emptyByteArray

  def doubleArrayToByteArray(source: Array[Double], byteOrder: ByteOrder): Array[Byte] =
    if (source.nonEmpty) {
      val target = new Array[Byte](Math.multiplyExact(source.length, java.lang.Double.BYTES))
      ByteBuffer
        .wrap(target)
        .order(byteOrder)
        .asDoubleBuffer()
        .put(source)
      target
    } else Array.emptyByteArray

  def byteArrayToShortArray(source: Array[Byte], byteOrder: ByteOrder): Array[Short] =
    if (source.nonEmpty) {
      if ((source.length & 1) != 0)
        throw new IllegalArgumentException(s"source Array[Byte] has illegal length: ${source.length}")
      val target = new Array[Short](source.length / java.lang.Short.BYTES)
      ByteBuffer
        .wrap(source)
        .order(byteOrder)
        .asShortBuffer()
        .get(target)
      target
    } else Array.emptyShortArray

  def byteArrayToIntArray(source: Array[Byte], byteOrder: ByteOrder): Array[Int] =
    if (source.nonEmpty) {
      if ((source.length & 3) != 0)
        throw new IllegalArgumentException(s"source Array[Byte] has illegal length: ${source.length}")
      val target = new Array[Int](source.length / java.lang.Integer.BYTES)
      ByteBuffer
        .wrap(source)
        .order(byteOrder)
        .asIntBuffer()
        .get(target)
      target
    } else Array.emptyIntArray

  def byteArrayToLongArray(source: Array[Byte], byteOrder: ByteOrder): Array[Long] =
    if (source.nonEmpty) {
      if ((source.length & 7) != 0)
        throw new IllegalArgumentException(s"source Array[Byte] has illegal length: ${source.length}")
      val target = new Array[Long](source.length / java.lang.Long.BYTES)
      ByteBuffer
        .wrap(source)
        .order(byteOrder)
        .asLongBuffer()
        .get(target)
      target
    } else Array.emptyLongArray

  def byteArrayToFloatArray(source: Array[Byte], byteOrder: ByteOrder): Array[Float] =
    if (source.nonEmpty) {
      if ((source.length & 3) != 0)
        throw new IllegalArgumentException(s"source Array[Byte] has illegal length: ${source.length}")
      val target = new Array[Float](source.length / java.lang.Float.BYTES)
      ByteBuffer
        .wrap(source)
        .order(byteOrder)
        .asFloatBuffer()
        .get(target)
      target
    } else Array.emptyFloatArray

  def byteArrayToDoubleArray(source: Array[Byte], byteOrder: ByteOrder): Array[Double] =
    if (source.nonEmpty) {
      if ((source.length & 7) != 0)
        throw new IllegalArgumentException(s"source Array[Byte] has illegal length: ${source.length}")
      val target = new Array[Double](source.length / java.lang.Double.BYTES)
      ByteBuffer
        .wrap(source)
        .order(byteOrder)
        .asDoubleBuffer()
        .get(target)
      target
    } else Array.emptyDoubleArray
