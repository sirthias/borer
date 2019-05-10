/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import java.nio.ByteBuffer

import scala.annotation.tailrec

/**
  * Abstraction over serialization output.
  *
  * The implementation be either mutable or immutable.
  */
trait Output { outer =>
  type Self <: Output { type Self <: outer.Self }
  type Result <: AnyRef

  def writeByte(byte: Byte): Self
  def writeBytes(a: Byte, b: Byte): Self
  def writeBytes(a: Byte, b: Byte, c: Byte): Self
  def writeBytes(a: Byte, b: Byte, c: Byte, d: Byte): Self

  def writeShort(value: Short): Self =
    writeBytes((value >> 8).toByte, value.toByte)

  def writeInt(value: Int): Self =
    writeBytes((value >> 24).toByte, (value >> 16).toByte, (value >> 8).toByte, value.toByte)

  def writeLong(value: Long): Self =
    writeInt((value >> 32).toInt).writeInt(value.toInt)

  def writeBytes[Bytes: ByteAccess](bytes: Bytes): Self

  def result(): Result
}

object Output {

  /**
    * Responsible for providing an Output that produces instances of [[T]].
    */
  trait Provider[T] {
    type Out <: Output { type Result = T }
    def apply(bufferSize: Int): Out
  }

  implicit final class OutputOps(val underlying: Output) extends AnyVal {
    @inline def writeAsByte(i: Int): underlying.Self = underlying.writeByte(i.toByte)

    @inline def writeAsByte(c: Char): underlying.Self           = underlying.writeByte(c.toByte)
    @inline def writeAsBytes(a: Char, b: Char): underlying.Self = underlying.writeBytes(a.toByte, b.toByte)

    @inline def writeAsBytes(a: Char, b: Char, c: Char): underlying.Self =
      underlying.writeBytes(a.toByte, b.toByte, c.toByte)

    @inline def writeAsBytes(a: Char, b: Char, c: Char, d: Char): underlying.Self =
      underlying.writeBytes(a.toByte, b.toByte, c.toByte, d.toByte)

    def writeStringAsAsciiBytes(s: String): underlying.Self = {
      @tailrec def rec(out: underlying.Self, ix: Int): underlying.Self =
        s.length - ix match {
          case 0 => out
          case 1 => writeAsByte(s.charAt(ix))
          case 2 => writeAsBytes(s.charAt(ix), s.charAt(ix + 1))
          case 3 => writeAsBytes(s.charAt(ix), s.charAt(ix + 1), s.charAt(ix + 2))
          case _ => rec(writeAsBytes(s.charAt(ix), s.charAt(ix + 1), s.charAt(ix + 2), s.charAt(ix + 3)), ix + 4)
        }
      rec(underlying.asInstanceOf[underlying.Self], 0)
    }
  }

  final private class Chunk[T <: AnyRef](val buffer: T, var next: Chunk[T])

  implicit object ToByteArrayProvider extends Provider[Array[Byte]] {
    type Out = ToByteArray
    def apply(bufferSize: Int) = new ToByteArray(bufferSize)
  }

  /**
    * Default, mutable implementation for serializing to plain byte arrays.
    */
  final class ToByteArray(bufferSize: Int) extends Output {
    private[this] var currentChunkBuffer            = new Array[Byte](bufferSize)
    private[this] var currentChunkBufferCursor: Int = _
    private[this] val rootChunk                     = new Chunk(currentChunkBuffer, next = null)
    private[this] var currentChunk                  = rootChunk
    private[this] var filledChunksSize              = 0L

    type Self   = ToByteArray
    type Result = Array[Byte]

    @inline def size: Long = filledChunksSize + currentChunkBufferCursor.toLong

    def writeByte(byte: Byte): this.type = {
      if (currentChunkBufferCursor == bufferSize) appendChunk()
      val cursor = currentChunkBufferCursor
      currentChunkBuffer(cursor) = byte
      currentChunkBufferCursor = cursor + 1
      this
    }

    def writeBytes(a: Byte, b: Byte): this.type =
      if (currentChunkBufferCursor < bufferSize - 1) {
        val cursor = currentChunkBufferCursor
        currentChunkBuffer(cursor) = a
        currentChunkBuffer(cursor + 1) = b
        currentChunkBufferCursor = cursor + 2
        this
      } else writeByte(a).writeByte(b)

    def writeBytes(a: Byte, b: Byte, c: Byte): this.type =
      if (currentChunkBufferCursor < bufferSize - 2) {
        val cursor = currentChunkBufferCursor
        currentChunkBuffer(cursor) = a
        currentChunkBuffer(cursor + 1) = b
        currentChunkBuffer(cursor + 2) = c
        currentChunkBufferCursor = cursor + 3
        this
      } else writeByte(a).writeByte(b).writeByte(c)

    def writeBytes(a: Byte, b: Byte, c: Byte, d: Byte): this.type =
      if (currentChunkBufferCursor < bufferSize - 3) {
        val cursor = currentChunkBufferCursor
        currentChunkBuffer(cursor) = a
        currentChunkBuffer(cursor + 1) = b
        currentChunkBuffer(cursor + 2) = c
        currentChunkBuffer(cursor + 3) = d
        currentChunkBufferCursor = cursor + 4
        this
      } else writeByte(a).writeByte(b).writeByte(c).writeByte(d)

    def writeBytes[Bytes](bytes: Bytes)(implicit byteAccess: ByteAccess[Bytes]): this.type = {
      @tailrec def rec(rest: Bytes): this.type = {
        val remaining = bufferSize - currentChunkBufferCursor
        val len       = byteAccess.sizeOf(rest)
        val newRest   = byteAccess.copyToByteArray(rest, currentChunkBuffer, currentChunkBufferCursor)
        if (len > remaining) {
          appendChunk()
          rec(newRest)
        } else {
          currentChunkBufferCursor += len.toInt
          if (currentChunkBufferCursor < 0) throw new Borer.Error.Overflow(this, f"Output size exceed 2^31 bytes")
          this
        }
      }
      rec(bytes)
    }

    def result(): Array[Byte] = {
      val longSize = size
      val intSize  = longSize.toInt
      if (intSize != longSize) {
        throw new Borer.Error.Overflow(this, f"Output size of $longSize%,d bytes too large for byte array")
      }
      val array = new Array[Byte](intSize)

      @tailrec def rec(chunk: Chunk[Array[Byte]], cursor: Int): Array[Byte] =
        if (chunk ne null) {
          val len = if (chunk.next eq null) currentChunkBufferCursor else bufferSize
          System.arraycopy(chunk.buffer, 0, array, cursor, len)
          rec(chunk.next, cursor + bufferSize)
        } else array

      rec(rootChunk, 0)
    }

    private def appendChunk(): Unit = {
      currentChunkBuffer = new Array[Byte](bufferSize)
      val newChunk = new Chunk(currentChunkBuffer, null)
      currentChunkBufferCursor = 0
      currentChunk.next = newChunk
      currentChunk = newChunk
      filledChunksSize += bufferSize.toLong
    }

    override def toString = s"Output.ToByteArray index $size"
  }

  implicit object ToByteBufferProvider extends Provider[ByteBuffer] {
    type Out = ToByteBuffer
    def apply(bufferSize: Int) = new ToByteBuffer(bufferSize)
  }

  /**
    * Default, mutable implementation for serializing to [[java.nio.ByteBuffer]] instances.
    */
  final class ToByteBuffer(bufferSize: Int) extends Output {
    private[this] var currentChunkBuffer = ByteBuffer.allocate(bufferSize)
    private[this] val rootChunk          = new Chunk(currentChunkBuffer, next = null)
    private[this] var currentChunk       = rootChunk
    private[this] var fullChunksSize     = 0L

    type Self   = ToByteBuffer
    type Result = ByteBuffer

    @inline def size: Long = fullChunksSize + currentChunkBuffer.position().toLong

    def writeByte(byte: Byte): this.type = {
      if (!currentChunkBuffer.hasRemaining) appendChunk()
      currentChunkBuffer.put(byte)
      this
    }

    def writeBytes(a: Byte, b: Byte): this.type =
      if (currentChunkBuffer.remaining >= 2) {
        currentChunkBuffer.put(a)
        currentChunkBuffer.put(b)
        this
      } else writeByte(a).writeByte(b)

    override def writeShort(value: Short): this.type =
      if (currentChunkBuffer.remaining >= 2) {
        currentChunkBuffer.putShort(value)
        this
      } else writeByte((value >> 8).toByte).writeByte(value.toByte)

    def writeBytes(a: Byte, b: Byte, c: Byte): this.type =
      if (currentChunkBuffer.remaining >= 3) {
        currentChunkBuffer.put(a)
        currentChunkBuffer.put(b)
        currentChunkBuffer.put(c)
        this
      } else writeByte(a).writeByte(b).writeByte(c)

    def writeBytes(a: Byte, b: Byte, c: Byte, d: Byte): this.type =
      if (currentChunkBuffer.remaining >= 4) {
        currentChunkBuffer.put(a)
        currentChunkBuffer.put(b)
        currentChunkBuffer.put(c)
        currentChunkBuffer.put(d)
        this
      } else writeByte(a).writeByte(b).writeByte(c).writeByte(d)

    override def writeInt(value: Int) =
      if (currentChunkBuffer.remaining >= 4) {
        currentChunkBuffer.putInt(value)
        this
      } else writeShort((value >> 16).toShort).writeShort(value.toShort)

    override def writeLong(value: Long) =
      if (currentChunkBuffer.remaining >= 8) {
        currentChunkBuffer.putLong(value)
        this
      } else writeInt((value >> 32).toInt).writeInt(value.toInt)

    def writeBytes[Bytes](bytes: Bytes)(implicit byteAccess: ByteAccess[Bytes]): this.type = {
      @tailrec def rec(rest: Bytes): this.type = {
        val newRest = byteAccess.copyToByteBuffer(bytes, currentChunkBuffer)
        if (!byteAccess.isEmpty(newRest)) {
          appendChunk()
          rec(newRest)
        } else this
      }
      rec(bytes)
    }

    def result(): ByteBuffer = {
      val longSize = size
      val intSize  = longSize.toInt
      if (intSize != longSize) {
        throw new Borer.Error.Overflow(this, f"Output size of $longSize%,d bytes too large for ByteBuffer")
      }
      val buf = ByteBuffer.allocate(intSize)

      @tailrec def rec(chunk: Chunk[ByteBuffer]): ByteBuffer =
        if (chunk ne null) {
          buf.put(chunk.buffer.flip().asInstanceOf[ByteBuffer])
          rec(chunk.next)
        } else buf.flip().asInstanceOf[ByteBuffer]

      rec(rootChunk)
    }

    private def appendChunk(): Unit = {
      currentChunkBuffer = ByteBuffer.allocate(bufferSize)
      val newChunk = new Chunk(currentChunkBuffer, null)
      currentChunk.next = newChunk
      currentChunk = newChunk
      fullChunksSize += bufferSize.toLong
    }

    override def toString = s"Output.ToByteBuffer index $size"
  }
}
