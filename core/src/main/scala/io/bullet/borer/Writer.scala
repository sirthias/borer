/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import io.bullet.borer.internal.Util

import scala.annotation.tailrec
import scala.collection.LinearSeq

/**
  * Stateful, mutable abstraction for writing a stream of CBOR or JSON data to the given [[Output]].
  */
final class Writer(receiver: Receiver, val target: Target, config: Writer.Config) {

  @inline def writingJson: Boolean = target eq null
  @inline def writingCbor: Boolean = target eq Cbor

  @inline def ~(value: Boolean): this.type = writeBool(value)
  @inline def ~(value: Char): this.type    = writeChar(value)
  @inline def ~(value: Byte): this.type    = writeByte(value)
  @inline def ~(value: Short): this.type   = writeShort(value)
  @inline def ~(value: Int): this.type     = writeInt(value)
  @inline def ~(value: Long): this.type    = writeLong(value)
  @inline def ~(value: Float): this.type   = writeFloat(value)
  @inline def ~(value: Double): this.type  = writeDouble(value)
  @inline def ~(value: String): this.type  = writeString(value)

  @inline def ~[T: Encoder](value: T): this.type = write(value)

  def writeNull(): this.type      = { receiver.onNull(); this }
  def writeUndefined(): this.type = { receiver.onUndefined(); this }

  @inline def writeBool(value: Boolean): this.type             = { receiver.onBool(value); this }
  @inline def writeChar(value: Char): this.type                = writeInt(value.toInt)
  @inline def writeByte(value: Byte): this.type                = writeInt(value.toInt)
  @inline def writeShort(value: Short): this.type              = writeInt(value.toInt)
  @inline def writeInt(value: Int): this.type                  = { receiver.onInt(value.toInt); this }
  @inline def writeLong(value: Long): this.type                = { receiver.onLong(value); this }
  def writeOverLong(negative: Boolean, value: Long): this.type = { receiver.onOverLong(negative, value); this }
  def writeFloat16(value: Float): this.type                    = { receiver.onFloat16(value); this }

  def writeFloat(value: Float): this.type = {
    if (config.compressFloatingPointValues && Util.canBeRepresentedAsFloat16(value)) {
      receiver.onFloat16(value)
    } else receiver.onFloat(value)
    this
  }

  def writeDouble(value: Double): this.type = {
    if (config.compressFloatingPointValues && Util.canBeRepresentedAsFloat(value)) {
      writeFloat(value.toFloat)
    } else receiver.onDouble(value)
    this
  }

  def writeNumberString(value: String): this.type = { receiver.onNumberString(value); this }

  @inline def writeString(value: String): this.type          = { receiver.onString(value); this }
  def writeBytes[Bytes: ByteAccess](value: Bytes): this.type = { receiver.onBytes(value); this }
  def writeText[Bytes: ByteAccess](value: Bytes): this.type  = { receiver.onText(value); this }
  def writeTag(value: Tag): this.type                        = { receiver.onTag(value); this }
  def writeSimpleValue(value: Int): this.type                = { receiver.onSimpleValue(value); this }

  def writeBytesStart(): this.type = { receiver.onBytesStart(); this }
  def writeTextStart(): this.type  = { receiver.onTextStart(); this }

  @inline def writeArrayHeader(length: Int): this.type  = writeArrayHeader(length.toLong)
  @inline def writeArrayHeader(length: Long): this.type = { receiver.onArrayHeader(length); this }
  @inline def writeArrayStart(): this.type              = { receiver.onArrayStart(); this }

  @inline def writeMapHeader(length: Int): this.type  = writeMapHeader(length.toLong)
  @inline def writeMapHeader(length: Long): this.type = { receiver.onMapHeader(length); this }
  @inline def writeMapStart(): this.type              = { receiver.onMapStart(); this }

  @inline def writeBreak(): this.type = { receiver.onBreak(); this }

  @inline def writeEndOfInput(): this.type = { receiver.onEndOfInput(); this }

  @inline def write[T](value: T)(implicit encoder: Encoder[T]): this.type =
    encoder.write(this, value).asInstanceOf[this.type]

  def writeEmptyArray(): this.type = if (writingJson) writeArrayStart().writeBreak() else writeArrayHeader(0)

  def writeToArray[T: Encoder](x: T): this.type =
    if (writingJson) writeArrayStart().write(x).writeBreak()
    else writeArrayHeader(1).write(x)

  def writeToArray[A: Encoder, B: Encoder](a: A, b: B): this.type =
    if (writingJson) writeArrayStart().write(a).write(b).writeBreak()
    else writeArrayHeader(2).write(a).write(b)

  def writeToArray[A: Encoder, B: Encoder, C: Encoder](a: A, b: B, c: C): this.type =
    if (writingJson) writeArrayStart().write(a).write(b).write(c).writeBreak()
    else writeArrayHeader(3).write(a).write(b).write(c)

  def writeEmptyMap(): this.type = if (writingJson) writeMapStart().writeBreak() else writeMapHeader(0)

  def writeIndexedSeq[T: Encoder](x: IndexedSeq[T]): this.type = {
    @tailrec def rec(ix: Int): Unit =
      if (ix < x.size) {
        write(x(ix))
        rec(ix + 1)
      }
    if (writingJson) {
      writeArrayStart()
      rec(0)
      writeBreak()
    } else {
      writeArrayHeader(x.size)
      rec(0)
      this
    }
  }

  def writeLinearSeq[T: Encoder](x: LinearSeq[T]): this.type = {
    @tailrec def rec(x: LinearSeq[T]): Unit =
      if (x.nonEmpty) {
        write(x.head)
        rec(x.tail)
      }
    if (writingJson || x.nonEmpty) {
      writeArrayStart()
      rec(x)
      writeBreak()
    } else writeArrayHeader(0)
  }

  def writeIterator[T: Encoder](iterator: Iterator[T]): this.type =
    if (iterator.hasNext) {
      writeArrayStart()
      while (iterator.hasNext) write(iterator.next())
      writeBreak()
    } else writeArrayHeader(0)

  def writeMap[A: Encoder, B: Encoder](x: Map[A, B]): this.type = {
    val iterator = x.iterator
    def writeEntries(): Unit =
      while (iterator.hasNext) {
        val (k, v) = iterator.next()
        write(k).write(v)
      }
    if (writingJson) {
      writeMapStart()
      writeEntries()
      writeBreak()
    } else {
      writeMapHeader(x.size)
      writeEntries()
      this
    }
  }
}

object Writer {

  trait Config {
    def compressFloatingPointValues: Boolean
  }

  /**
    * Simple encapsulation of encoding logic in a stand-alone object.
    */
  final case class Script(encode: Writer ⇒ Writer)

  object Script {
    val Undefined  = Script(_.writeUndefined())
    val BytesStart = Script(_.writeBytesStart())
    val TextStart  = Script(_.writeTextStart())
    val ArrayStart = Script(_.writeArrayStart())
    val MapStart   = Script(_.writeMapStart())
    val Break      = Script(_.writeBreak())

    implicit val encoder: Encoder[Script] = Encoder((w, x) ⇒ x.encode(w))
  }
}
