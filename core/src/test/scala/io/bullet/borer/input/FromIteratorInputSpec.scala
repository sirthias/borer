/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.input

import io.bullet.borer._
import utest._
import io.bullet.borer.internal.Util._

object FromIteratorInputSpec extends TestSuite with TestUtils {

  val inputBytes = {
    val bytes = new Array[Byte](1024)
    for (i <- 0 to 511) {
      bytes((i << 1) + 0) = (i >> 8).toByte
      bytes((i << 1) + 1) = (i >> 0).toByte
    }
    bytes
  }

  val tests = Tests {

    // verification: our "test suite" does indeed pass for a known-good Input implementation
    "FromByteArray" - testInput(Input.fromByteArray(inputBytes))

    "FromInputIterator - 100 byte chunks" - testInput(chunkIteratorInput(100))

    "FromInputIterator - 10 byte chunks" - testInput(chunkIteratorInput(10))

    "FromInputIterator - alternating 5 and 0 byte chunks" - testInput(chunkIteratorInput(5, 0))

    "FromInputIterator - mixed chunks" - testInput(chunkIteratorInput(5, 0, 11, 7, 2, 0, 0, 1, 23))
  }

  def testInput(input: Input[Array[Byte]]): Unit = {
    val pp = new FFPadder(input)

    input.readOctaByteBigEndianPadded(pp) ==> 0x0000000100020003L

    input.unread(7)
    input.readOctaByteBigEndianPadded(pp) ==> 0x0000010002000300L

    input.unread(6)
    input.readOctaByteBigEndianPadded(pp) ==> 0x0100020003000400L

    input.unread(5)
    input.readOctaByteBigEndianPadded(pp) ==> 0x0003000400050006L

    input.unread(4)
    input.readOctaByteBigEndianPadded(pp) ==> 0x0005000600070008L

    input.unread(3)
    input.readOctaByteBigEndianPadded(pp) ==> 0x0700080009000a00L

    input.unread(2)
    input.readOctaByteBigEndianPadded(pp) ==> 0x0a000b000c000d00L

    input.unread(1)
    input.readOctaByteBigEndianPadded(pp) ==> 0x000e000f00100011L

    input.readQuadByteBigEndianPadded(pp) ==> 0x00120013

    input.unread(4)
    input.readQuadByteBigEndianPadded(pp) ==> 0x00120013

    input.unread(3)
    input.readQuadByteBigEndianPadded(pp) ==> 0x12001300

    input.unread(2)
    input.readQuadByteBigEndianPadded(pp) ==> 0x13001400

    input.unread(1)
    input.readQuadByteBigEndianPadded(pp) ==> 0x00150016

    input.readDoubleByteBigEndianPadded(pp) ==> '\u0017'

    input.unread(2)
    input.readDoubleByteBigEndianPadded(pp) ==> '\u0017'

    input.unread(1)
    input.readDoubleByteBigEndianPadded(pp) ==> '\u1700'

    input.readBytePadded(pp) ==> 0x18

    input.readBytes(16, pp) ==> hex"0019001A001B001C001D001E001F0020"

    input.unread(7)
    input.readBytes(16, pp) ==> hex"1D001E001F0020002100220023002400"

    input.readBytePadded(pp) ==> 0x25

    val x = input.readBytes(178, pp)
    x ==> (38 until 127).flatMap(i => Array((i >> 8).toByte, i.toByte)).toArray

    input.unread(6)
    input.readBytes(779, pp) ==> (124 until 512).flatMap(i => Array((i >> 8).toByte, i.toByte)).toArray ++ Array[Byte](
      -1,
      -1,
      -1)

    input.unread(5)
    input.readOctaByteBigEndianPadded(pp) ==> 0xfd01fe01ffffffffL

    input.unread(251)
    input.readBytes(255, pp) ==> inputBytes.takeRight(251) ++ Array[Byte](-1, -1, -1, -1)
  }

  def chunkIteratorInput(chunkSizes: Int*): Input[Array[Byte]] = {
    val iter = Iterator.continually(0).flatMap(_ => chunkSizes)
    Input.fromIterator(chunkIterator(inputBytes, iter).map(Input.fromByteArray))
  }

  def chunkIterator(remainingBytes: Array[Byte], chunkSizes: Iterator[Int]): Iterator[Array[Byte]] = {
    val len = chunkSizes.next()
    if (remainingBytes.length <= len) Iterator.single(remainingBytes)
    else Iterator.single(remainingBytes.take(len)) ++ chunkIterator(remainingBytes.drop(len), chunkSizes)
  }

  final class FFPadder(input: Input[Array[Byte]]) extends Input.PaddingProvider[Array[Byte]] {

    def padByte(): Byte = -1

    def padDoubleByte(remaining: Int): Char =
      if (remaining < 1) '\uffff' else ((input.readByte() << 8) | 0xff).toChar

    def padQuadByte(remaining: Int): Int = {
      import input.{readByte => byte, readDoubleByteBigEndian => doub}
      // format: OFF
      remaining match {
        case 0 =>                                            0xFFFFFFFF
        case 1 =>                         (byte()   << 24) | 0xFFFFFF
        case 2 => (doub() << 16)                           | 0xFFFF
        case 3 => (doub() << 16) | ((byte() & 0xFF) <<  8) | 0xFF
        case _ => throw new IllegalStateException
      }
      // format: ON
    }

    def padOctaByte(remaining: Int): Long = {
      import input.{readByte => byte, readDoubleByteBigEndian => doub, readQuadByteBigEndian => quad}
      // format: OFF
      remaining match {
        case 0 =>                                                                                 0XFFFFFFFFFFFFFFFFL
        case 1 =>                                                      (byte().toLong    << 56) | 0XFFFFFFFFFFFFFFL
        case 2 =>                         (doub().toLong      << 48)                            | 0XFFFFFFFFFFFFL
        case 3 =>                         (doub().toLong      << 48) | ((byte() & 0XFFL) << 40) | 0XFFFFFFFFFFL
        case 4 => (quad().toLong << 32) |                                                         0XFFFFFFFFL
        case 5 => (quad().toLong << 32) |                              ((byte() & 0XFFL) << 24) | 0XFFFFFFL
        case 6 => (quad().toLong << 32) | ((doub() & 0XFFFFL) << 16) |                            0XFFFFL
        case 7 => (quad().toLong << 32) | ((doub() & 0XFFFFL) << 16) | ((byte() & 0XFFL) <<  8) | 0XFFL
        case _ => throw new IllegalStateException
      }
      // format: ON
    }

    def padBytes(rest: Array[Byte], missing: Long) =
      ByteAccess.ForByteArray.concat(rest, Array.fill[Byte](missing.toInt)(-1))
  }
}
