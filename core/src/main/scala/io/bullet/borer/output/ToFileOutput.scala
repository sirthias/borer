/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.output

import java.io.{BufferedOutputStream, File, FileOutputStream}

import io.bullet.borer.{ByteAccess, Output}
import io.bullet.borer.Output.ToValueProvider

trait ToFileOutput {

  implicit object ToFileProvider extends ToValueProvider[File] {
    type Out = ToFile
    def apply(file: File, bufferSize: Int) = new ToFile(file, bufferSize)
  }

  /**
    * Default, mutable implementation for serializing to files.
    */
  final class ToFile(file: File, bufferSize: Int) extends Output {
    private[this] val outputStream = new BufferedOutputStream(new FileOutputStream(file), bufferSize)

    type Self   = ToFile
    type Result = File

    def writeByte(byte: Byte): this.type = {
      outputStream.write(byte.toInt)
      this
    }

    def writeBytes(a: Byte, b: Byte): this.type                   = writeByte(a).writeByte(b)
    def writeBytes(a: Byte, b: Byte, c: Byte): this.type          = writeByte(a).writeByte(b).writeByte(c)
    def writeBytes(a: Byte, b: Byte, c: Byte, d: Byte): this.type = writeByte(a).writeByte(b).writeByte(c).writeByte(d)

    def writeBytes[Bytes](bytes: Bytes)(implicit byteAccess: ByteAccess[Bytes]): this.type = {
      outputStream.write(byteAccess.toByteArray(bytes))
      this
    }

    def result(): File = {
      outputStream.close()
      file
    }
  }
}
