/*
 * Copyright (c) 2019-2022 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.input

import java.io.InputStream

import io.bullet.borer.BorerSuite
import io.bullet.borer._

import scala.util.Random

class FromInputStreamInputSpec extends BorerSuite:

  val random = new Random

  test("FromInputStreamInput") {

    def newBytesIterator = Iterator.from(0).take(10000).map(_.toByte)

    val bytes = newBytesIterator

    val inputStream = new InputStream {
      def read() = ???
      override def read(b: Array[Byte]) =
        if (bytes.hasNext)
          val chunk = random.nextInt(4) match
            case 0     => Array.emptyByteArray
            case 1 | 2 => bytes.take(b.length).toArray[Byte]
            case 3     => bytes.take(random.nextInt(b.length) + 1).toArray[Byte]
          System.arraycopy(chunk, 0, b, 0, chunk.length)
          chunk.length
        else -1
    }

    val input = Input.fromInputStream(inputStream, bufferSize = 300)

    val paddingProvider = new Input.PaddingProvider[Array[Byte]] {
      def padByte()                                  = 42
      def padDoubleByte(remaining: Int)              = ???
      def padQuadByte(remaining: Int)                = ???
      def padOctaByte(remaining: Int)                = ???
      def padBytes(rest: Array[Byte], missing: Long) = ???
    }

    for {
      (a, b) <- newBytesIterator.map(_ -> input.readBytePadded(paddingProvider))
    } a ==> b

    input.cursor ==> 10000

    input.readBytePadded(paddingProvider) ==> 42
  }
