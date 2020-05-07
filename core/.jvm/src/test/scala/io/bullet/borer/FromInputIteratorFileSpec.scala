/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import java.nio.charset.StandardCharsets

import utest._

import scala.io.Source

object FromInputIteratorFileSpec extends TestSuite with TestUtils {

  val testFileBytes = Source.fromResource("large.json").mkString.getBytes(StandardCharsets.UTF_8)

  val config = Json.DecodingConfig.default
    .copy(maxNumberMantissaDigits = 99, maxNumberAbsExponent = 300, initialCharbufferSize = 8)
  val dom = Json.decode(testFileBytes).withConfig(config).to[Dom.Element].value

  val tests = Tests {

    "test file" - {
      Json
        .decode(chunkedInput(3, 2, 1, 0, 100, 71))
        .withConfig(config)
        .to[Dom.Element]
        .value ==> dom
    }
  }

  def chunkedInput(chunkSizes: Int*): Iterator[Array[Byte]] =
    chunkIterator(testFileBytes, Iterator.continually(0).flatMap(_ => chunkSizes))

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
