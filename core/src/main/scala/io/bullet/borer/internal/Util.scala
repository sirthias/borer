/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.internal

import io.bullet.borer.Input

import scala.annotation.tailrec
import scala.reflect.ClassTag

object Util {

  val isJS  = 1.0.toString == "1"
  val isJVM = !isJS

  // "platform-independent" toString for Doubles, appends a ".0" suffix on JS, if required
  def floatToString(value: Float): String = fixFloatingPointNumbersOnJS(java.lang.Float.toString(value))

  // "platform-independent" toString for Doubles, appends a ".0" suffix on JS, if required
  def doubleToString(value: Double): String = fixFloatingPointNumbersOnJS(java.lang.Double.toString(value))

  private def fixFloatingPointNumbersOnJS(s: String): String = {
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

  @inline def requirePositive(value: Int, name: String): Int = {
    requirePositive(value.toLong, name)
    value
  }

  @inline def requirePositive(value: Long, name: String): Long =
    if (value <= 0) throw new IllegalArgumentException(s"$name must be > 0, but was $value")
    else value

  private[this] val _identityFunc = (x: Any) => x
  def identityFunc[T]: T => T     = _identityFunc.asInstanceOf[T => T]

  @inline def isChar(x: Int): Boolean              = (x >> 16) == 0
  @inline def isByte(x: Int): Boolean              = (x >> 7) == (x >> 31)
  @inline def isShort(x: Int): Boolean             = (x >> 15) == (x >> 31)
  @inline def isInt(x: Long): Boolean              = (x >> 31) == (x >> 63)
  @inline def isUnsignedInt(uLong: Long): Boolean  = uLong >> 31 == 0
  @inline def isUnsignedLong(uLong: Long): Boolean = uLong >= 0

  def emptyArray[T](implicit ct: ClassTag[T]): Array[T] =
    (ct.runtimeClass match {
      case java.lang.Byte.TYPE      => Array.emptyByteArray
      case java.lang.Short.TYPE     => Array.emptyShortArray
      case java.lang.Character.TYPE => Array.emptyCharArray
      case java.lang.Integer.TYPE   => Array.emptyIntArray
      case java.lang.Long.TYPE      => Array.emptyLongArray
      case java.lang.Float.TYPE     => Array.emptyFloatArray
      case java.lang.Double.TYPE    => Array.emptyDoubleArray
      case java.lang.Boolean.TYPE   => Array.emptyBooleanArray
      case _                        => Array.emptyObjectArray
    }).asInstanceOf[Array[T]]

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

  def inPlaceNegate(bytes: Array[Byte]): Unit = {
    @tailrec def rec(ix: Int): Unit = if (ix < bytes.length) {
      bytes(ix) = (~bytes(ix).toInt).toByte; rec(ix + 1)
    }
    rec(0)
  }

  /**
    * Compares the UTF8 encoding in `input` to the given String and returns
    * a value < 0 if `input` < `string`
    * a 0 if `input` == `string`
    * a value > 0 if `input` > `string`
    */
  def stringCompare(input: Input, string: String): Int = {
    @tailrec def rec(stringIx: Int): Int =
      if (stringIx < string.length) {
        var six = stringIx
        if (input.prepareRead(1)) {
          val bytesCodepoint = {
            val b = input.readByte().toInt
            if (b < 0) input.readMultiByteUtf8Codepoint(b) else b
          }
          val stringCodepoint = {
            val c = string.charAt(six)
            if (Character.isHighSurrogate(c)) {
              def failIllegalArg(msg: String) = throw new IllegalArgumentException(msg)
              six += 1
              if (six < string.length) {
                val c2 = string.charAt(six)
                if (Character.isLowSurrogate(c2)) Character.toCodePoint(c, c2)
                else failIllegalArg(s"""Invalid UTF-16 surrogate pair at index $stringIx of string "$string"""")
              } else failIllegalArg(s"""Truncated UTF-16 surrogate pair at end of string "$string"""")
            } else c.toInt
          }
          val d = bytesCodepoint - stringCodepoint
          if (d == 0) rec(six + 1) else d
        } else -1
      } else if (input.prepareRead(1)) /* `string` is a prefix of the parsed string */ 1
      else 0

    rec(0)
  }
}
