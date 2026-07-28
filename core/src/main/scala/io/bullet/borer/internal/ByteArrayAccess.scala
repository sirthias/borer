/*
 * Copyright (c) 2019-2026 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.internal

import java.nio.ByteOrder

abstract class ByteArrayAccess:

  def doubleByteBigEndian(byteArray: Array[Byte], ix: Int): Char
  def quadByteBigEndian(byteArray: Array[Byte], ix: Int): Int
  def octaByteBigEndian(byteArray: Array[Byte], ix: Int): Long

  def setDoubleByteBigEndian(byteArray: Array[Byte], ix: Int, value: Char): Unit
  def setQuadByteBigEndian(byteArray: Array[Byte], ix: Int, value: Int): Unit
  def setOctaByteBigEndian(byteArray: Array[Byte], ix: Int, value: Long): Unit

  def shortArrayToByteArray(source: Array[Short], byteOrder: ByteOrder): Array[Byte]
  def intArrayToByteArray(source: Array[Int], byteOrder: ByteOrder): Array[Byte]
  def longArrayToByteArray(source: Array[Long], byteOrder: ByteOrder): Array[Byte]
  def floatArrayToByteArray(source: Array[Float], byteOrder: ByteOrder): Array[Byte]
  def doubleArrayToByteArray(source: Array[Double], byteOrder: ByteOrder): Array[Byte]

  def byteArrayToShortArray(source: Array[Byte], byteOrder: ByteOrder): Array[Short]
  def byteArrayToIntArray(source: Array[Byte], byteOrder: ByteOrder): Array[Int]
  def byteArrayToLongArray(source: Array[Byte], byteOrder: ByteOrder): Array[Long]
  def byteArrayToFloatArray(source: Array[Byte], byteOrder: ByteOrder): Array[Float]
  def byteArrayToDoubleArray(source: Array[Byte], byteOrder: ByteOrder): Array[Double]
