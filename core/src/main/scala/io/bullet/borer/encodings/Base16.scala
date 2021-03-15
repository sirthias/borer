/*
 * Copyright (c) 2019-2021 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.encodings

import io.bullet.borer.internal.ByteArrayAccess

import scala.annotation.tailrec

object Base16 extends BaseEncoding("base16", 4) {

  def encode(bytes: Array[Byte]): Array[Char] = encode(bytes, upperCase = false)

  def encode(bytes: Array[Byte], upperCase: Boolean): Array[Char] = {
    val sl        = bytes.length
    val digitBase = if (upperCase) 7 else 39

    def failOverflow() =
      throw new IllegalArgumentException(
        s"Overflow: Cannot $name-encode a byte array with size > 1.073.741.824 (was: $sl")

    if (sl > Int.MaxValue / 2) failOverflow()

    val result = new Array[Char](sl << 1)
    val baa    = ByteArrayAccess.instance
    val sl4    = sl - 4

    @inline def hexDigit(i: Int) = (48 + i + (digitBase & ((9 - i) >> 31))).toChar

    @tailrec def encodeSlow(si: Int, di: Int): Array[Char] =
      if (si < sl) {
        val b = bytes(si)
        result(di + 0) = hexDigit(b << 24 >>> 28)
        result(di + 1) = hexDigit(b & 0x0F)
        encodeSlow(si + 1, di + 2)
      } else result

    @tailrec def encodeFast(si: Int, di: Int): Array[Char] =
      if (si < sl4) {
        val quad = baa.quadByteBigEndian(bytes, si)
        result(di + 0) = hexDigit(quad << 0 >>> 28)
        result(di + 1) = hexDigit(quad << 4 >>> 28)
        result(di + 2) = hexDigit(quad << 8 >>> 28)
        result(di + 3) = hexDigit(quad << 12 >>> 28)
        result(di + 4) = hexDigit(quad << 16 >>> 28)
        result(di + 5) = hexDigit(quad << 20 >>> 28)
        result(di + 6) = hexDigit(quad << 24 >>> 28)
        result(di + 7) = hexDigit(quad & 0x0F)
        encodeFast(si + 4, di + 8)
      } else encodeSlow(si, di)

    encodeFast(0, 0)
  }

  def decode(chars: Array[Char]): Array[Byte] = {
    val sl = chars.length

    def failIllegalEncoding() =
      throw new IllegalArgumentException(s"Illegal Encoding: The given char array has an odd length ($sl).")

    if ((sl & 1) != 0) failIllegalEncoding()

    val result = new Array[Byte](sl >> 1)
    val sl8    = sl - 8

    def d(ix: Int): Int = {
      val c = chars(ix)
      def fail() =
        throw new IllegalArgumentException(s""""${Util.show(chars)}" is not a valid $name encoding.
                                              | '$c' at index $ix is not part of the $name alphabet.""".stripMargin)
      val cc = c - 48
      if (c < 0 || 102 < c || (((0x7E0000007E03FFL >> cc) & 1) == 0)) fail()
      (c & 0x1F) + ((c >> 6) * 0x19) - 0x10
    }

    @tailrec def decodeSlow(si: Int, di: Int): Array[Byte] =
      if (si < sl) {
        result(di) = (d(si) << 4 | d(si + 1)).toByte
        decodeSlow(si + 2, di + 1)
      } else result

    @tailrec def decodeFast(si: Int, di: Int): Array[Byte] =
      if (si <= sl8) {
        result(di + 0) = (d(si + 0) << 4 | d(si + 1)).toByte
        result(di + 1) = (d(si + 2) << 4 | d(si + 3)).toByte
        result(di + 2) = (d(si + 4) << 4 | d(si + 5)).toByte
        result(di + 3) = (d(si + 6) << 4 | d(si + 7)).toByte
        decodeFast(si + 8, di + 4)
      } else decodeSlow(si, di)

    decodeFast(0, 0)
  }
}
