/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.input

import java.io.{File, FileInputStream}
import java.nio.file.Files

import io.bullet.borer.{ByteAccess, Input}
import io.bullet.borer

trait FromFileInput { this: FromByteArrayInput with FromIteratorInput =>

  implicit object FromFileProvider extends Input.Provider[File] {
    type Bytes = Array[Byte]
    type In    = Input[Array[Byte]]
    def byteAccess         = ByteAccess.ForByteArray
    def apply(value: File) = fromFile(value)
  }

  def fromFile(file: File, bufferSize: Int = 16384): Input[Array[Byte]] = {
    if (bufferSize < 256) throw new IllegalArgumentException(s"bufferSize must be > 256 but was $bufferSize")
    val fileSize = file.length()
    if (fileSize > bufferSize) {
      val iterator: Iterator[Input.FromByteArray] =
        new Iterator[Input.FromByteArray] {
          private[this] val inputStream     = new FileInputStream(file)
          private[this] val bufA            = new Array[Byte](bufferSize)
          private[this] val bufB            = new Array[Byte](bufferSize)
          private[this] var chunkCount: Int = 0
          private[this] var remaining: Long = fileSize

          def hasNext = remaining > 0

          def next() = {
            val chunkNr = chunkCount
            chunkCount += 1

            val buf =
              if (remaining >= bufferSize) {
                if ((chunkNr & 1) == 0) bufA else bufB
              } else new Array[Byte](remaining.toInt)

            val byteCount = inputStream.read(buf)
            if (byteCount != buf.length)
              sys.error(
                s"Error reading chunk number $chunkNr (size: ${buf.length} bytes) " +
                  s"of input file '$file' (size: $fileSize bytes)")

            remaining -= byteCount

            new borer.Input.FromByteArray(buf)
          }
        }
      new FromIterator(iterator, ByteAccess.ForByteArray)
    } else new FromByteArray(Files.readAllBytes(file.toPath))
  }

}
