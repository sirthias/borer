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

final class Base32(name: String, alphabet: String) extends LookupBaseEncoding(name, 5, alphabet) {
  if (alphabetChars.length != 32) throw new IllegalArgumentException

  def encode(bytes: Array[Byte]): Array[Char] = {
    val sl = bytes.length

    def failOverflow() =
      throw new IllegalArgumentException(
        s"Overflow: Cannot $name-encode a byte array with size > 1.342.177.279 (was: $sl")

    if (sl > 1342177279) failOverflow()

    val result = new Array[Char](((sl + 4) / 5) << 3)
    val baa    = ByteArrayAccess.instance
    val sl5    = sl - 5

    def encodeRest(si: Int, di: Int): Array[Char] = {
      sl - si match {
        case 1 =>
          val int = bytes(si).toInt << 24
          result(di + 0) = alphabetChars(int << 0 >>> 27)
          result(di + 1) = alphabetChars(int << 5 >>> 27)
          result(di + 2) = 0x3D
          result(di + 3) = 0x3D
          result(di + 4) = 0x3D
          result(di + 5) = 0x3D
          result(di + 6) = 0x3D
          result(di + 7) = 0x3D

        case 2 =>
          val int = baa.doubleByteBigEndian(bytes, si).toInt << 16
          result(di + 0) = alphabetChars(int << 0 >>> 27)
          result(di + 1) = alphabetChars(int << 5 >>> 27)
          result(di + 2) = alphabetChars(int << 10 >>> 27)
          result(di + 3) = alphabetChars(int << 15 >>> 27)
          result(di + 4) = 0x3D
          result(di + 5) = 0x3D
          result(di + 6) = 0x3D
          result(di + 7) = 0x3D

        case 3 =>
          val int = baa.doubleByteBigEndian(bytes, si).toInt << 16 | (bytes(si + 2) & 0xFF) << 8
          result(di + 0) = alphabetChars(int << 0 >>> 27)
          result(di + 1) = alphabetChars(int << 5 >>> 27)
          result(di + 2) = alphabetChars(int << 10 >>> 27)
          result(di + 3) = alphabetChars(int << 15 >>> 27)
          result(di + 4) = alphabetChars(int << 20 >>> 27)
          result(di + 5) = 0x3D
          result(di + 6) = 0x3D
          result(di + 7) = 0x3D

        case 4 =>
          val octa = baa.quadByteBigEndian(bytes, si).toLong << 32
          result(di + 0) = alphabetChars((octa << 0 >>> 59).toInt)
          result(di + 1) = alphabetChars((octa << 5 >>> 59).toInt)
          result(di + 2) = alphabetChars((octa << 10 >>> 59).toInt)
          result(di + 3) = alphabetChars((octa << 15 >>> 59).toInt)
          result(di + 4) = alphabetChars((octa << 20 >>> 59).toInt)
          result(di + 5) = alphabetChars((octa << 25 >>> 59).toInt)
          result(di + 6) = alphabetChars((octa << 30 >>> 59).toInt)
          result(di + 7) = 0x3D
      }
      result
    }

    @tailrec def encode5(si: Int, di: Int): Array[Char] =
      if (si <= sl5) {
        val octa = baa.quadByteBigEndian(bytes, si).toLong << 32 | (bytes(si + 4) & 0xFFL) << 24
        result(di + 0) = alphabetChars((octa << 0 >>> 59).toInt)
        result(di + 1) = alphabetChars((octa << 5 >>> 59).toInt)
        result(di + 2) = alphabetChars((octa << 10 >>> 59).toInt)
        result(di + 3) = alphabetChars((octa << 15 >>> 59).toInt)
        result(di + 4) = alphabetChars((octa << 20 >>> 59).toInt)
        result(di + 5) = alphabetChars((octa << 25 >>> 59).toInt)
        result(di + 6) = alphabetChars((octa << 30 >>> 59).toInt)
        result(di + 7) = alphabetChars((octa << 35 >>> 59).toInt)
        encode5(si + 5, di + 8)
      } else if (si < sl) encodeRest(si, di)
      else result

    encode5(0, 0)
  }

  def decode(chars: Array[Char]): Array[Byte] = {
    val sl = chars.length
    if (sl > 0) {
      def failIllegalLength() =
        throw new IllegalArgumentException(
          s"Illegal Encoding: The given char array has a length that is not evenly divisible by 8 ($sl).")

      def failIllegalPadding() =
        throw new IllegalArgumentException(
          s"Illegal Padding: The given encoding has a padding that doesn't conform to the $name spec.")

      if ((sl & 7) != 0) failIllegalLength()

      val baa = ByteArrayAccess.instance
      val sl8 = sl - 8

      @inline def c(offset: Int) = chars(sl - offset) & 0xFFL

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

      def decode8(result: Array[Byte], si: Int, di: Int): Unit = {
        @inline def d(offset: Int) = decode(si + offset)

        val octa = d(0) << 35 | d(1) << 30 | d(2) << 25 | d(3) << 20 | d(4) << 15 | d(5) << 10 | d(6) << 5 | d(7)
        baa.setQuadByteBigEndian(result, di, (octa >> 8).toInt)
        result(di + 4) = octa.toByte
      }

      val baseLen = {
        val x = sl >> 3
        (x << 2) + x // sl / 8 * 5
      }

      val c1 = c(1)
      if (c1 == 0x3D) { // if we have at least one padding char
        val final6 = c(6) << 40 | c(5) << 32 | c(4) << 24 | c(3) << 16 | c(2) << 8 | c1

        // padding-length  ==> odd bytes
        // ----------------------------------------
        //       6         ==> 1
        //       5         ==> 0 (invalid per spec)
        //       4         ==> 2
        //       3         ==> 3
        //       2         ==> 0 (invalid per spec)
        //       1         ==> 4
        //       0         ==> 0
        val ntz      = java.lang.Long.numberOfTrailingZeros(final6 ^ 0x3D3D3D3D3D3D3D3DL)
        val oddBytes = (0x0001000203000400L >> (ntz & 0xF8)).toInt & 0xF
        val result   = new Array[Byte](baseLen - 5 + oddBytes)

        def decodeRest(si: Int, di: Int): Array[Byte] = {
          @inline def d(offset: Int) = decode(si + offset)

          oddBytes match {
            case 1 =>
              result(di) = (d(0) << 3 | d(1) >> 2).toByte
            case 2 =>
              val bytes = d(0) << 11 | d(1) << 6 | d(2) << 1 | d(3) >> 4
              baa.setDoubleByteBigEndian(result, di, bytes.toChar)
            case 3 =>
              val bytes = d(0) << 19 | d(1) << 14 | d(2) << 9 | d(3) << 4 | d(4) >> 1
              baa.setDoubleByteBigEndian(result, di, (bytes >> 8).toChar)
              result(di + 2) = bytes.toByte
            case 4 =>
              val bytes = d(0) << 27 | d(1) << 22 | d(2) << 17 | d(3) << 12 | d(4) << 7 | d(5) << 2 | d(6) >> 3
              baa.setQuadByteBigEndian(result, di, bytes.toInt)
            case _ =>
              failIllegalPadding()
          }
          result
        }

        @tailrec def rec(si: Int, di: Int): Array[Byte] =
          if (si < sl8) {
            decode8(result, si, di)
            rec(si + 8, di + 5)
          } else decodeRest(si, di)

        rec(0, 0)
      } else {
        val result = new Array[Byte](baseLen)

        @tailrec def rec(si: Int, di: Int): Array[Byte] =
          if (si < sl) {
            decode8(result, si, di)
            rec(si + 8, di + 5)
          } else result

        rec(0, 0)
      }
    } else Array.emptyByteArray
  }
}
