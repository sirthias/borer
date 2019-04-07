/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.compat

import io.bullet.borer.{ByteAccess, _}
import _root_.akka.util.ByteString

object akka {

  /**
    * [[ByteAccess]] for [[ByteString]].
    */
  implicit object ByteStringByteAccess extends ByteAccess[ByteString] {

    type Out = ByteStringOutput

    def newOutput = new ByteStringOutput

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

    def convert[B](value: B)(implicit byteAccess: ByteAccess[B]) =
      value match {
        case x: ByteString ⇒ x
        case x             ⇒ ByteString(byteAccess.toByteArray(x))
      }

    def empty = ByteString.empty
  }

  /**
    * Encoding and Decoding for [[ByteString]].
    */
  implicit val ByteStringCodec = Codec.of[ByteString](_ writeBytes _, _.readBytes())

  /**
    * [[InputAccess]] for [[ByteString]].
    */
  implicit object ByteStringInputAccess extends InputAccess[ByteString] {
    type Bytes = ByteString

    def byteAccess = ByteStringByteAccess

    @inline def length(input: ByteString): Long = input.length.toLong

    def safeByte(input: ByteString, index: Long): Byte = input(index.toInt)

    def unsafeByte(input: ByteString, index: Long): Byte = input(index.toInt)

    def doubleByteBigEndian(input: ByteString, index: Long): Int = {
      val i = index.toInt
      (input(i) & 0xFF) << 8 |
      (input(i + 1) & 0xFF)
    }

    def quadByteBigEndian(input: ByteString, index: Long): Int = {
      val i = index.toInt
      (input(i) & 0xFF) << 24 |
      (input(i + 1) & 0xFF) << 16 |
      (input(i + 2) & 0xFF) << 8 |
      (input(i + 3) & 0xFF)
    }

    def octaByteBigEndian(input: ByteString, index: Long): Long = {
      val i = index.toInt
      (input(i) & 0xFFL) << 56 |
      (input(i + 1) & 0xFFL) << 48 |
      (input(i + 2) & 0xFFL) << 40 |
      (input(i + 3) & 0xFFL) << 32 |
      (input(i + 4) & 0xFFL) << 24 |
      (input(i + 5) & 0xFFL) << 16 |
      (input(i + 6) & 0xFFL) << 8 |
      (input(i + 7) & 0xFFL)
    }

    def bytes(input: ByteString, index: Long, length: Long): ByteString =
      if ((index | length) >> 31 == 0) {
        if (length != 0) {
          val end = index + length
          if ((end >> 31) == 0) input.slice(index.toInt, end.toInt)
          else throw new Borer.Error.Overflow(Position(input, index), "ByteString input is limited to size 2GB")
        } else ByteString.empty
      } else throw new Borer.Error.Overflow(Position(input, index), "ByteString input is limited to size 2GB")
  }

  /**
    * Mutable [[Output]] implementation for serializing to [[ByteString]].
    */
  final class ByteStringOutput extends Output {
    private[this] var builder = ByteString.newBuilder

    type Self   = ByteStringOutput
    type Result = ByteString

    def cursor: Int = builder.length

    def writeByte(byte: Byte): this.type = {
      builder += byte
      this
    }

    def writeBytes(a: Byte, b: Byte): this.type = {
      builder += a
      builder += b
      this
    }

    def writeBytes(a: Byte, b: Byte, c: Byte): this.type = {
      builder += a
      builder += b
      builder += c
      this
    }

    def writeBytes(a: Byte, b: Byte, c: Byte, d: Byte): this.type = {
      builder += a
      builder += b
      builder += c
      builder += d
      this
    }

    def writeBytes[Bytes: ByteAccess](bytes: Bytes): this.type = {
      builder ++= ByteStringByteAccess.convert(bytes)
      this
    }

    def result(): ByteString = builder.result()
  }
}
