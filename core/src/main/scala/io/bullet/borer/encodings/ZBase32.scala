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

object ZBase32 extends LookupBaseEncoding("z-base32", 5, "ybndrfg8ejkmcpqxot1uwisza345h769") {

  def encode(bytes: Array[Byte]): Array[Char] = encode(bytes.length.toLong << 3, bytes)

  def encode(bitCount: Long, bytes: Array[Byte]) = {
    val sl = bytes.length

    def failOverflow() =
      throw new IllegalArgumentException(
        s"Overflow: Cannot $name-encode a byte array with size > 1.342.177.279 (was: $sl")
    def failBitCountMismatch() =
      throw new IllegalArgumentException(
        s"BitCount mismatch: The given byte array with length $sl doesn't contain $bitCount bits.")

    if (sl > 1342177279) failOverflow()
    if (sl.toLong << 3 < bitCount) failBitCountMismatch()

    val result = new Array[Char](((bitCount + 4) / 5).toInt)
    val baa    = ByteArrayAccess.instance

    def encodeRest(bitsRemaining: Long, si: Int, di: Int): Array[Char] = {
      val fullBytes =
        sl - si match {
          case 1 => bytes(si).toLong << 56
          case 2 => baa.doubleByteBigEndian(bytes, si).toLong << 48
          case 3 => baa.doubleByteBigEndian(bytes, si).toLong << 48 | (bytes(si + 2) & 0xFFL) << 40
          case 4 => baa.quadByteBigEndian(bytes, si).toLong << 32
        }
      val bits = fullBytes & ~(-1L >>> bitsRemaining)
      result(di) = alphabetChars((bits >>> 59).toInt)
      if (bitsRemaining > 5) result(di + 1) = alphabetChars((bits << 5 >>> 59).toInt)
      if (bitsRemaining > 10) result(di + 2) = alphabetChars((bits << 10 >>> 59).toInt)
      if (bitsRemaining > 15) result(di + 3) = alphabetChars((bits << 15 >>> 59).toInt)
      if (bitsRemaining > 20) result(di + 4) = alphabetChars((bits << 20 >>> 59).toInt)
      if (bitsRemaining > 25) result(di + 5) = alphabetChars((bits << 25 >>> 59).toInt)
      if (bitsRemaining > 30) result(di + 6) = alphabetChars((bits << 30 >>> 59).toInt)
      if (bitsRemaining > 35) result(di + 7) = alphabetChars((bits << 35 >>> 59).toInt)
      result
    }

    @tailrec def encode5(bitsRemaining: Long, si: Int, di: Int): Array[Char] =
      if (bitsRemaining >= 40) {
        val octa = baa.quadByteBigEndian(bytes, si).toLong << 32 | (bytes(si + 4) & 0xFFL) << 24
        result(di + 0) = alphabetChars((octa << 0 >>> 59).toInt)
        result(di + 1) = alphabetChars((octa << 5 >>> 59).toInt)
        result(di + 2) = alphabetChars((octa << 10 >>> 59).toInt)
        result(di + 3) = alphabetChars((octa << 15 >>> 59).toInt)
        result(di + 4) = alphabetChars((octa << 20 >>> 59).toInt)
        result(di + 5) = alphabetChars((octa << 25 >>> 59).toInt)
        result(di + 6) = alphabetChars((octa << 30 >>> 59).toInt)
        result(di + 7) = alphabetChars((octa << 35 >>> 59).toInt)
        encode5(bitsRemaining - 40, si + 5, di + 8)
      } else if (bitsRemaining > 0) encodeRest(bitsRemaining, si, di)
      else result

    encode5(bitCount, 0, 0)
  }

  def decode(chars: Array[Char]): Array[Byte] = {
    val sl = chars.length.toLong
    decode((sl << 2) + sl, chars)
  }

  def decode(bitCount: Long, chars: Array[Char]): Array[Byte] = {
    val sl = chars.length

    def failBitCountMismatch() =
      throw new IllegalArgumentException(
        s"BitCount mismatch: Given chat array with length $sl doesn't contain $bitCount bits.")

    if ((sl << 2) + sl < bitCount) failBitCountMismatch()

    val dlen   = ((bitCount + 7) >> 3).toInt
    val result = new Array[Byte](dlen)
    val baa    = ByteArrayAccess.instance

    def decode(ix: Int): Long = {
      val c = chars(ix)
      def fail() =
        throw new IllegalArgumentException(s""""${Util
          .show(chars)}" is not a valid $name encoding. '$c' at index $ix is not part of the $name alphabet.""")
      if (c > 127) fail()
      val b = lookup(c.toInt)
      if (b < 0) fail()
      b.toLong
    }

    def decodeRest(bitsRemaining: Long, si: Int, di: Int): Array[Byte] = {
      var fullBytes = decode(si) << 59
      if (bitsRemaining > 5) fullBytes |= decode(si + 1) << 54
      if (bitsRemaining > 10) fullBytes |= decode(si + 2) << 49
      if (bitsRemaining > 15) fullBytes |= decode(si + 3) << 44
      if (bitsRemaining > 20) fullBytes |= decode(si + 4) << 39
      if (bitsRemaining > 25) fullBytes |= decode(si + 5) << 34
      if (bitsRemaining > 30) fullBytes |= decode(si + 6) << 29
      if (bitsRemaining > 35) fullBytes |= decode(si + 7) << 24
      val bits = fullBytes & ~(-1L >>> bitsRemaining)
      dlen - di match {
        case 1 => result(di) = (bits >>> 56).toByte
        case 2 => baa.setDoubleByteBigEndian(result, di, (bits >>> 48).toChar)

        case 3 =>
          baa.setDoubleByteBigEndian(result, di, (bits >>> 48).toChar)
          result(di + 2) = (bits >>> 40).toByte

        case 4 => baa.setQuadByteBigEndian(result, di, (bits >>> 32).toInt)
      }
      result
    }

    @tailrec def decode8(bitsRemaining: Long, si: Int, di: Int): Array[Byte] =
      if (bitsRemaining >= 40) {
        @inline def d(offset: Int) = decode(si + offset)

        val long = d(0) << 35 | d(1) << 30 | d(2) << 25 | d(3) << 20 | d(4) << 15 | d(5) << 10 | d(6) << 5 | d(7)
        baa.setQuadByteBigEndian(result, di, (long >>> 8).toInt)
        result(di + 4) = long.toByte
        decode8(bitsRemaining - 40, si + 8, di + 5)
      } else if (bitsRemaining > 0) decodeRest(bitsRemaining, si, di)
      else result

    decode8(bitCount, 0, 0)
  }
}
