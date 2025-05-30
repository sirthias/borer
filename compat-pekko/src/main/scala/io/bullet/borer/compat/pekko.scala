/*
 * Copyright (c) 2019-2024 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.compat

import _root_.org.apache.pekko.actor
import _root_.org.apache.pekko.actor.typed.{ActorRef, ActorRefResolver, ActorSystem}
import _root_.org.apache.pekko.serialization.Serialization
import _root_.org.apache.pekko.util.ByteString
import io.bullet.borer.*

import java.nio.ByteBuffer

object pekko {

  implicit def actorRefCodec(implicit system: actor.ActorSystem = serializationSystem): Codec[actor.ActorRef] = {
    val actorRefProvider = system.asInstanceOf[actor.ExtendedActorSystem].provider

    Codec.bimap[String, actor.ActorRef](
      Serialization.serializedActorPath,
      actorRefProvider.resolveActorRef
    )
  }

  implicit def typedActorRefCodec[T](implicit system: actor.ActorSystem = serializationSystem): Codec[ActorRef[T]] = {
    val resolver = ActorRefResolver(ActorSystem.wrap(system))

    Codec.bimap[String, ActorRef[T]](
      resolver.toSerializationFormat,
      resolver.resolveActorRef
    )
  }

  private def serializationSystem: actor.ActorSystem = Serialization.getCurrentTransportInformation().system

  /**
   * [[ByteAccess]] for [[ByteString]].
   */
  implicit object ByteStringByteAccess extends ByteAccess[ByteString] {

    type Out = ByteStringOutput

    def isEmpty(bytes: ByteString): Boolean = bytes.isEmpty

    def sizeOf(bytes: ByteString): Long = bytes.length.toLong

    def fromByteArray(byteArray: Array[Byte]): ByteString = ByteString(byteArray)

    def toByteArray(bytes: ByteString): Array[Byte] = bytes.toArray

    def concat(a: ByteString, b: ByteString): ByteString =
      if (a.nonEmpty) {
        if (b.nonEmpty) {
          val len = a.length + b.length
          if (len >= 0) {
            a ++ b
          } else sys.error("Cannot concatenate two ByteStrings with a total size > 2^31 bytes")
        } else a
      } else b

    def copyToByteArray(bytes: ByteString, byteArray: Array[Byte], startIndex: Int): ByteString = {
      val len = byteArray.length - startIndex
      bytes.copyToArray(byteArray, startIndex)
      if (len < bytes.size) bytes.drop(len) else empty
    }

    def copyToByteBuffer(bytes: ByteString, byteBuffer: ByteBuffer): ByteString = {
      val copied = bytes.copyToBuffer(byteBuffer)
      if (copied < bytes.size) bytes.drop(copied) else empty
    }

    def convert[B](value: B)(implicit byteAccess: ByteAccess[B]): ByteString =
      value match {
        case x: ByteString => x
        case x             => fromByteArray(byteAccess.toByteArray(x))
      }

    def empty = ByteString.empty
  }

  /**
   * Encoding and Decoding for [[ByteString]].
   */
  implicit val ByteStringCodec: Codec[ByteString] = Codec[ByteString](_ writeBytes _, _.readBytes())

  /**
   * [[Input]] around [[ByteString]].
   */
  implicit object ByteStringProvider extends Input.Provider[ByteString] {
    type Bytes = ByteString
    type In    = FromByteString
    def byteAccess: ByteAccess[ByteString] = ByteStringByteAccess
    def apply(value: ByteString)           = new FromByteString(value)
  }

  final class FromByteString(byteString: ByteString) extends Input[ByteString] {
    private[this] var _cursor: Int = _

    def cursor: Long = _cursor.toLong

    def unread(numberOfBytes: Int): this.type = {
      _cursor -= numberOfBytes
      this
    }

    def readByte(): Byte = {
      val c = _cursor
      _cursor = c + 1
      byteString(c)
    }

    def readBytePadded(pp: Input.PaddingProvider[ByteString]): Byte =
      if (_cursor < byteString.length) readByte()
      else pp.padByte()

    def readDoubleByteBigEndian(): Char = {
      val c = _cursor
      _cursor = c + 2
      ((byteString(c) << 8) | byteString(c + 1) & 0xFF).toChar
    }

    def readDoubleByteBigEndianPadded(pp: Input.PaddingProvider[ByteString]): Char = {
      val remaining = byteString.length - _cursor
      if (remaining >= 2) readDoubleByteBigEndian()
      else pp.padDoubleByte(remaining)
    }

    def readQuadByteBigEndian(): Int = {
      val c = _cursor
      _cursor = c + 4
      byteString(c) << 24 |
      (byteString(c + 1) & 0xFF) << 16 |
      (byteString(c + 2) & 0xFF) << 8 |
      byteString(c + 3) & 0xFF
    }

    def readQuadByteBigEndianPadded(pp: Input.PaddingProvider[ByteString]): Int = {
      val remaining = byteString.length - _cursor
      if (remaining >= 4) readQuadByteBigEndian()
      else pp.padQuadByte(remaining)
    }

    def readOctaByteBigEndian(): Long = {
      val c = _cursor
      _cursor = c + 8
      byteString(c).toLong << 56 |
      (byteString(c + 1) & 0xFFL) << 48 |
      (byteString(c + 2) & 0xFFL) << 40 |
      (byteString(c + 3) & 0xFFL) << 32 |
      (byteString(c + 4) & 0xFFL) << 24 |
      (byteString(c + 5) & 0xFFL) << 16 |
      (byteString(c + 6) & 0xFFL) << 8 |
      byteString(c + 7) & 0xFFL
    }

    def readOctaByteBigEndianPadded(pp: Input.PaddingProvider[ByteString]): Long = {
      val remaining = byteString.length - _cursor
      if (remaining >= 8) readOctaByteBigEndian()
      else pp.padOctaByte(remaining)
    }

    def readBytes(length: Long, pp: Input.PaddingProvider[ByteString]): ByteString = {
      val remaining = (byteString.length - _cursor).toLong
      val len       = math.min(remaining, length).toInt
      val bytes     =
        if (len > 0) {
          val c = _cursor
          _cursor = c + len
          byteString.slice(c, _cursor)
        } else ByteString.empty
      if (length <= remaining) bytes
      else pp.padBytes(bytes, length - remaining)
    }
  }

  implicit object ByteStringOutputProvider extends Output.ToTypeProvider[ByteString] {
    type Out = ByteStringOutput
    def apply(bufferSize: Int, allowBufferCaching: Boolean) = new ByteStringOutput
  }

  /**
   * Mutable [[Output]] implementation for serializing to [[ByteString]].
   */
  final class ByteStringOutput extends Output {
    private[this] val builder = ByteString.newBuilder

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
