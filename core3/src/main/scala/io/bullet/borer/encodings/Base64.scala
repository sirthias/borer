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

final class Base64(name: String, alphabet: String) extends LookupBaseEncoding(name, 6, alphabet):
  if (alphabetChars.length != 64) throw new IllegalArgumentException

  def encode(bytes: Array[Byte]): Array[Char] =
    val sl = bytes.length

    def failOverflow() =
      throw new IllegalArgumentException(
        s"Overflow: Cannot $name-encode a byte array with size > 1.610.612.735 (was: $sl")

    if (sl > 1610612735) failOverflow()

    val result = new Array[Char](((sl + 2) / 3) << 2)
    val baa    = ByteArrayAccess.instance
    val sl3    = sl - 3

    def encodeRest(si: Int, di: Int): Array[Char] =
      sl - si match
        case 1 =>
          val int = bytes(si).toInt << 24
          result(di + 0) = alphabetChars(int << 0 >>> 26)
          result(di + 1) = alphabetChars(int << 6 >>> 26)
          result(di + 2) = 0x3D
          result(di + 3) = 0x3D

        case 2 =>
          val int = baa.doubleByteBigEndian(bytes, si).toInt << 16
          result(di + 0) = alphabetChars(int << 0 >>> 26)
          result(di + 1) = alphabetChars(int << 6 >>> 26)
          result(di + 2) = alphabetChars(int << 12 >>> 26)
          result(di + 3) = 0x3D
      result

    @tailrec def encode3(si: Int, di: Int): Array[Char] =
      if (si <= sl3)
        val quad = baa.doubleByteBigEndian(bytes, si).toInt << 16 | (bytes(si + 2) & 0xFF) << 8
        result(di + 0) = alphabetChars(quad << 0 >>> 26)
        result(di + 1) = alphabetChars(quad << 6 >>> 26)
        result(di + 2) = alphabetChars(quad << 12 >>> 26)
        result(di + 3) = alphabetChars(quad << 18 >>> 26)
        encode3(si + 3, di + 4)
      else if (si < sl) encodeRest(si, di)
      else result

    encode3(0, 0)

  def decode(chars: Array[Char]): Array[Byte] =
    val sl = chars.length
    if (sl > 0)
      def failIllegalLength() =
        throw new IllegalArgumentException(
          s"Illegal Encoding: The given char array has a length that is not evenly divisible by 4 ($sl).")

      def failIllegalPadding() =
        throw new IllegalArgumentException(
          s"Illegal Padding: The given encoding has a padding that doesn't conform to the $name spec.")

      if ((sl & 3) != 0) failIllegalLength()

      val baa = ByteArrayAccess.instance
      val sl4 = sl - 4

      @inline def c(offset: Int) = chars(sl - offset) & 0xFFL

      def decode(ix: Int): Long =
        val c = chars(ix)
        def fail() =
          throw new IllegalArgumentException(s""""${Util
              .show(chars)}" is not a valid $name encoding. '$c' at index $ix is not part of the $name alphabet.""")
        if (c > 127) fail()
        val b = lookup(c.toInt)
        if (b < 0) fail()
        b.toLong

      def decode4(result: Array[Byte], si: Int, di: Int): Unit =
        @inline def d(offset: Int) = decode(si + offset)

        val quad = d(0) << 18 | d(1) << 12 | d(2) << 6 | d(3)
        baa.setDoubleByteBigEndian(result, di, (quad >> 8).toChar)
        result(di + 2) = quad.toByte

      val baseLen =
        val x = sl >> 2
        (x << 1) + x // sl / 4 * 3

      val c1 = c(1)
      if (c1 == 0x3D) // if we have at least one padding char
        val oddBytes = if (c(2) == 0x3D) 1 else 2
        val result   = new Array[Byte](baseLen - 3 + oddBytes)

        def decodeRest(si: Int, di: Int): Array[Byte] =
          @inline def d(offset: Int) = decode(si + offset)

          oddBytes match
            case 1 => result(di) = (d(0) << 2 | d(1) >> 4).toByte
            case 2 => baa.setDoubleByteBigEndian(result, di, (d(0) << 10 | d(1) << 4 | d(2) >> 2).toChar)
            case _ => failIllegalPadding()
          result

        @tailrec def rec(si: Int, di: Int): Array[Byte] =
          if (si < sl4)
            decode4(result, si, di)
            rec(si + 4, di + 3)
          else decodeRest(si, di)

        rec(0, 0)
      else
        val result = new Array[Byte](baseLen)

        @tailrec def rec(si: Int, di: Int): Array[Byte] =
          if (si < sl)
            decode4(result, si, di)
            rec(si + 4, di + 3)
          else result

        rec(0, 0)
    else Array.emptyByteArray
