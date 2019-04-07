/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.internal

import scala.annotation.tailrec

object Util {

  val isJS  = 1.0.toString == "1"
  val isJVM = !isJS

  // "platform-independent" toString for Doubles, appends a ".0" suffix on JS, if required
  def doubleToString(value: Double): String = {
    val s = java.lang.Double.toString(value)
    // check, whether the string consists only of digits (except for the first char, which might be a minus sign)
    @tailrec def onlyDigits(ix: Int): Boolean = ix <= 0 || { val c = s(ix); '0' <= c && c <= '9' && onlyDigits(ix - 1) }
    if (isJS && onlyDigits(s.length - 1)) s + ".0" else s
  }

  @inline def requireNonNegative(value: Int, name: String): Int = {
    requireNonNegative(value.toLong, name)
    value
  }

  @inline def requireNonNegative(value: Long, name: String): Long =
    if (value < 0) throw new IllegalArgumentException(s"$name must be >= 0, but was $value")
    else value

  private[this] val _identityFunc = (x: Any) ⇒ x
  def identityFunc[T]: T ⇒ T      = _identityFunc.asInstanceOf[T ⇒ T]

  @inline def isChar(x: Int): Boolean              = (x >> 16) == 0
  @inline def isByte(x: Int): Boolean              = (x >> 7) == (x >> 31)
  @inline def isShort(x: Int): Boolean             = (x >> 15) == (x >> 31)
  @inline def isInt(x: Long): Boolean              = (x >> 31) == (x >> 63)
  @inline def isUnsignedInt(uLong: Long): Boolean  = uLong >> 31 == 0
  @inline def isUnsignedLong(uLong: Long): Boolean = uLong >= 0

  def toBigEndianBytes(uLong: Long): Array[Byte] = {
    val bytes = new Array[Byte](8)
    bytes(0) = (uLong >>> 56).toByte
    bytes(1) = (uLong >>> 48).toByte
    bytes(2) = (uLong >>> 40).toByte
    bytes(3) = (uLong >>> 32).toByte
    bytes(4) = (uLong >>> 24).toByte
    bytes(5) = (uLong >>> 16).toByte
    bytes(6) = (uLong >>> 8).toByte
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

  @inline def canBeRepresentedAsFloat(value: Double): Boolean = value.isNaN || value.toFloat.toDouble == value

  /**
    * Returns the int value of a given hex digit char.
    * Note: this implementation is very fast (since it's branchless) and therefore
    * does not perform ANY range checks!
    */
  @inline def hexValue(c: Char): Int = (c & 0x1f) + ((c >> 6) * 0x19) - 0x10

  def inPlaceNegate(bytes: Array[Byte]): Unit = {
    @tailrec def rec(ix: Int): Unit = if (ix < bytes.length) { bytes(ix) = (~bytes(ix).toInt).toByte; rec(ix + 1) }
    rec(0)
  }
}
