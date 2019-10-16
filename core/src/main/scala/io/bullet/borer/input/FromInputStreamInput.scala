/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.input

import java.io.InputStream
import java.util

import io.bullet.borer.{ByteAccess, Input}

trait FromInputStreamInput { this: FromByteArrayInput with FromIteratorInput =>

  implicit object FromInputStreamProvider extends Input.Provider[InputStream] {
    type Bytes = Array[Byte]
    type In    = Input[Array[Byte]]
    def byteAccess                = ByteAccess.ForByteArray
    def apply(value: InputStream) = fromInputStream(value)
  }

  def fromInputStream(inputStream: InputStream, bufferSize: Int = 16384): Input[Array[Byte]] = {
    if (bufferSize < 256) throw new IllegalArgumentException(s"bufferSize must be >= 256 but was $bufferSize")
    val iterator: Iterator[Input.FromByteArray] =
      new Iterator[Input.FromByteArray] {
        private[this] val bufA                           = new Array[Byte](bufferSize)
        private[this] val bufB                           = new Array[Byte](bufferSize)
        private[this] var bufSelect: Boolean             = _
        private[this] var nextInput: Input.FromByteArray = _

        def hasNext = {
          val buf = if (bufSelect) bufA else bufB
          nextInput = inputStream.read(buf) match {
            case -1 => null
            case `bufferSize` =>
              bufSelect = !bufSelect
              new Input.FromByteArray(buf)
            case byteCount => new Input.FromByteArray(util.Arrays.copyOfRange(buf, 0, byteCount))
          }
          nextInput ne null
        }

        def next() =
          if (nextInput ne null) nextInput
          else throw new NoSuchElementException
      }
    new FromIterator(iterator, ByteAccess.ForByteArray)
  }

}
