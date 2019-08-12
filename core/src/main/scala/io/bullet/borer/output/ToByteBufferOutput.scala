/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.output

import java.nio.ByteBuffer

import io.bullet.borer.{Borer, ByteAccess, Output}
import io.bullet.borer.Output.ToTypeProvider

import scala.annotation.tailrec

trait ToByteBufferOutput {

  implicit object ToByteBufferProvider extends ToTypeProvider[ByteBuffer] {
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
        val newRest = byteAccess.copyToByteBuffer(rest, currentChunkBuffer)
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

      @tailrec def rec(chunk: Chunk): ByteBuffer =
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

  final private class Chunk(val buffer: ByteBuffer, var next: Chunk)
}
