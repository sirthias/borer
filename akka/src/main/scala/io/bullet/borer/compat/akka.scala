/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.compat

import io.bullet.borer.core._
import _root_.akka.util.ByteString

object akka {

  /**
    * [[ByteAccess]] for [[ByteString]].
    */
  implicit val byteStringByteAccess: ByteAccess[ByteString] =
    new ByteAccess[ByteString] {

      def sizeOf(bytes: ByteString): Long = bytes.length.toLong

      def fromByteArray(byteArray: Array[Byte]): ByteString = ByteString(byteArray)

      def toByteArray(bytes: ByteString): Array[Byte] = bytes.toArray

      def concat(a: ByteString, b: ByteString) =
        if (a.nonEmpty) {
          if (b.nonEmpty) {
            val len = a.length + b.length
            if (len >= 0) {
              a ++ b
            } else sys.error("Cannot concatenate two ByteStrings with a total size > 2^31 bytes")
          } else a
        } else b

      val empty = ByteString.empty
    }

  implicit val byteStringCodec: Codec[ByteString, ByteString, ByteString] =
    Codec(Encoder(_ writeBytes _), Decoder(_.readBytes()))

  /**
    * Mutable [[Input]] implementation for deserializing from [[ByteString]]
    */
  implicit class ByteStringInput(input: ByteString) extends Input[ByteString] with java.lang.Cloneable {
    private[this] var _cursor: Int           = _
    private[this] var _lastByte: Byte        = _
    private[this] var _lastBytes: ByteString = _

    type Self = ByteStringInput

    def cursor: Int           = _cursor
    def lastByte: Byte        = _lastByte
    def lastBytes: ByteString = _lastBytes

    def hasBytes(length: Long): Boolean = {
      val off = length + _cursor
      0 <= off && off <= input.length
    }

    def readByte(): Self = {
      val c = _cursor
      _lastByte = input(c)
      _cursor = c + 1
      this
    }

    def readBytes(length: Long): Self =
      if (length >> 31 == 0) {
        if (length > 0) {
          val c         = _cursor
          val newCursor = c + length.toInt
          _lastBytes = input.slice(c, newCursor)
          _cursor = newCursor
        } else _lastBytes = ByteString.empty
        this
      } else throw new Cbor.Error.Overflow(_cursor, "ByteString input is limited to size 2GB")

    def copy: ByteStringInput = super.clone().asInstanceOf[ByteStringInput]
  }

  implicit def newByteStringOutput: ByteStringOutput = new ByteStringOutput

  /**
    * Mutable [[Output]] implementation for serializing to [[ByteString]].
    */
  final class ByteStringOutput extends Output[ByteString] {
    private[this] var builder = ByteString.newBuilder

    type Self = ByteStringOutput

    def cursor: Int = builder.length

    def writeByte(byte: Byte): this.type = {
      builder += byte
      this
    }

    def writeBytes(bytes: Array[Byte]): this.type = writeBytes(ByteString(bytes))

    def writeBytes(bytes: ByteString): this.type = {
      builder ++= bytes
      this
    }

    def result(): ByteString = builder.result()
  }
}
