/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import java.math.{BigDecimal ⇒ JBigDecimal, BigInteger ⇒ JBigInteger}

import scala.annotation.tailrec
import scala.collection.LinearSeq

/**
  * Stateful, mutable abstraction for writing a stream of CBOR or JSON data to the given [[Output]].
  */
final class Writer(startOutput: Output,
                   receiver: Receiver[Output],
                   val config: Writer.Config,
                   val target: Borer.Target) {

  private[this] var _output: Output = startOutput

  def output: Output = _output

  def writingJson: Boolean = target eq Json
  def writingCbor: Boolean = target eq Cbor

  def ~(value: Boolean): this.type = writeBool(value)
  def ~(value: Char): this.type    = writeChar(value)
  def ~(value: Byte): this.type    = writeByte(value)
  def ~(value: Short): this.type   = writeShort(value)
  def ~(value: Int): this.type     = writeInt(value)
  def ~(value: Long): this.type    = writeLong(value)
  def ~(value: Float): this.type   = writeFloat(value)
  def ~(value: Double): this.type  = writeDouble(value)
  def ~(value: String): this.type  = writeString(value)

  def ~[T: Encoder](value: T): this.type = write(value)

  def writeNull(): this.type      = ret(receiver.onNull(_output))
  def writeUndefined(): this.type = ret(receiver.onUndefined(_output))

  def writeBool(value: Boolean): this.type                     = ret(receiver.onBool(_output, value))
  def writeChar(value: Char): this.type                        = writeInt(value.toInt)
  def writeByte(value: Byte): this.type                        = writeInt(value.toInt)
  def writeShort(value: Short): this.type                      = writeInt(value.toInt)
  def writeInt(value: Int): this.type                          = ret(receiver.onInt(_output, value.toInt))
  def writeLong(value: Long): this.type                        = ret(receiver.onLong(_output, value))
  def writeOverLong(negative: Boolean, value: Long): this.type = ret(receiver.onOverLong(_output, negative, value))
  def writeFloat16(value: Float): this.type                    = ret(receiver.onFloat16(_output, value))

  def writeFloat(value: Float): this.type = ret {
    if (writingJson || config.cborDontCompressFloatingPointValues || !Util.canBeRepresentedAsFloat16(value)) {
      receiver.onFloat(_output, value)
    } else receiver.onFloat16(_output, value)
  }

  def writeDouble(value: Double): this.type =
    if (writingJson || config.cborDontCompressFloatingPointValues || !Util.canBeRepresentedAsFloat(value)) {
      ret(receiver.onDouble(_output, value))
    } else writeFloat(value.toFloat)

  def writeBigInteger(value: JBigInteger): this.type = ret(receiver.onBigInteger(_output, value))
  def writeBigDecimal(value: JBigDecimal): this.type = ret(receiver.onBigDecimal(_output, value))

  def writeString(value: String): this.type                  = ret(receiver.onString(_output, value))
  def writeBytes[Bytes: ByteAccess](value: Bytes): this.type = ret(receiver.onBytes(_output, value))
  def writeText[Bytes: ByteAccess](value: Bytes): this.type  = ret(receiver.onText(_output, value))
  def writeTag(value: Tag): this.type                        = ret(receiver.onTag(_output, value))
  def writeSimpleValue(value: Int): this.type                = ret(receiver.onSimpleValue(_output, value))

  def writeBytesStart(): this.type = ret(receiver.onBytesStart(_output))
  def writeTextStart(): this.type  = ret(receiver.onTextStart(_output))

  def writeArrayHeader(length: Int): this.type  = writeArrayHeader(length.toLong)
  def writeArrayHeader(length: Long): this.type = ret(receiver.onArrayHeader(_output, length))
  def writeArrayStart(): this.type              = ret(receiver.onArrayStart(_output))

  def writeMapHeader(length: Int): this.type  = writeMapHeader(length.toLong)
  def writeMapHeader(length: Long): this.type = ret(receiver.onMapHeader(_output, length))
  def writeMapStart(): this.type              = ret(receiver.onMapStart(_output))

  def writeBreak(): this.type = ret(receiver.onBreak(_output))

  def writeEndOfInput(): this.type = ret(receiver.onEndOfInput(_output))

  def write[T](value: T)(implicit encoder: Encoder[T]): this.type = encoder.write(this, value)

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

  private def ret(out: Output): this.type = {
    _output = out
    this
  }
}

object Writer {

  /**
    * Serialization config settings
    *
    * @param validation the validation settings to use or `None` if no validation should be performed
    * @param cborDontCompressFloatingPointValues set to true in order to always write floats as 32-bit values and doubles
    *                                        as 64-bit values, even if they could safely be represented with fewer bits
    */
  final case class Config(
      validation: Option[Validation.Config] = Some(Validation.Config()),
      cborDontCompressFloatingPointValues: Boolean = false
  )

  object Config {
    val default = Config()
  }

  /**
    * Simple encapsulation of encoding logic in a stand-alone object.
    */
  final case class Script(encode: Writer ⇒ Unit)

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
