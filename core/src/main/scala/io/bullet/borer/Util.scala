/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

object Util {

  def requireNonNegative(value: Int, name: String): Int = {
    requireNonNegative(value.toLong, name)
    value
  }

  def requireNonNegative(value: Long, name: String): Long =
    if (value < 0) throw new IllegalArgumentException(s"$name must be >= 0, but was $value")
    else value

  def isChar(x: Int): Boolean              = (x >> 16) == 0
  def isByte(x: Int): Boolean              = (x >> 7) == (x >> 31)
  def isShort(x: Int): Boolean             = (x >> 15) == (x >> 31)
  def isUnsignedInt(uLong: Long): Boolean  = uLong >> 31 == 0
  def isUnsignedLong(uLong: Long): Boolean = uLong >= 0

  def toUnsignedInt(byte: Byte): Int   = byte.toInt & 0xFF
  def toUnsignedLong(byte: Byte): Long = byte.toLong & 0xFFL

  def toBigEndianBytes(uLong: Long): Array[Byte] = {
    val bytes = new Array[Byte](8)
    bytes(0) = (uLong >> 56).toByte
    bytes(1) = (uLong >> 48).toByte
    bytes(2) = (uLong >> 40).toByte
    bytes(3) = (uLong >> 32).toByte
    bytes(4) = (uLong >> 24).toByte
    bytes(5) = (uLong >> 16).toByte
    bytes(6) = (uLong >> 8).toByte
    bytes(7) = uLong.toByte
    bytes
  }

  def canBeRepresentedAsFloat16(value: Float): Boolean = {
    val bits = java.lang.Float.floatToIntBits(value)
    // Float has 23 mantissa bits, Float16 has only 10
    // so the 13 lower bits of the mantissa must be zero
    (bits & ((1 << 13) - 1)) == 0 && { // are the 13 lower bits of the mantissa zero?
      val exp = (bits << 1) >>> 24 // move out sign bit and get 8 bit exponent
      exp == 0 || exp == 0xFF || { // is exp a special value?
        val normalizedExp = exp - 127 // reverse exponent bias
        (normalizedExp >> 4) == (normalizedExp >> 31) // does normalizedExp fit into 5 bits?
      }
    }
  }

  def canBeRepresentedAsFloat(value: Double): Boolean = value.isNaN || value.toFloat.toDouble == value
}
