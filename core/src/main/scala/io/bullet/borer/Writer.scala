/*
 * Copyright (c) 2019-2026 Mathias Doenitz
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
final class Writer(
    val output: Output, // CAUTION: `null` in case of transcoding!
    private[borer] var receiver: Receiver,
    val target: Target,
    val config: Writer.Config):

  inline def writingJson: Boolean = target eq Json
  inline def writingCbor: Boolean = target eq Cbor

  inline def ~(value: Boolean): this.type = writeBoolean(value)
  inline def ~(value: Char): this.type    = writeChar(value)
  inline def ~(value: Byte): this.type    = writeByte(value)
  inline def ~(value: Short): this.type   = writeShort(value)
  inline def ~(value: Int): this.type     = writeInt(value)
  inline def ~(value: Long): this.type    = writeLong(value)
  inline def ~(value: Float): this.type   = writeFloat(value)
  inline def ~(value: Double): this.type  = writeDouble(value)
  inline def ~(value: String): this.type  = writeString(value)

  inline def ~[T: Encoder](value: T): this.type = write(value)

  def writeNull(): this.type      = { receiver.onNull(); this }
  def writeUndefined(): this.type = { receiver.onUndefined(); this }

  def writeBoolean(value: Boolean): this.type                  = { receiver.onBoolean(value); this }
  def writeChar(value: Char): this.type                        = writeInt(value.toInt)
  def writeByte(value: Byte): this.type                        = writeInt(value.toInt)
  def writeShort(value: Short): this.type                      = writeInt(value.toInt)
  def writeInt(value: Int): this.type                          = { receiver.onInt(value); this }
  def writeLong(value: Long): this.type                        = { receiver.onLong(value); this }
  def writeOverLong(negative: Boolean, value: Long): this.type = { receiver.onOverLong(negative, value); this }
  def writeFloat16(value: Float): this.type                    = { receiver.onFloat16(value); this }

  def writeFloat(value: Float): this.type =
    if (config.compressFloatingPointValues && Util.canBeRepresentedAsFloat16(value))
      receiver.onFloat16(value)
    else receiver.onFloat(value)
    this

  def writeDouble(value: Double): this.type =
    if (config.compressFloatingPointValues && Util.canBeRepresentedAsFloat(value))
      writeFloat(value.toFloat)
    else receiver.onDouble(value)
    this

  def writeNumberString(value: String): this.type = { receiver.onNumberString(value); this }

  def writeString(value: String): this.type     = { receiver.onString(value); this }
  def writeChars(value: Array[Char]): this.type = { receiver.onChars(value, value.length); this }

  def writeBytes[Bytes: ByteAccess](value: Bytes): this.type = { receiver.onBytes(value); this }
  def writeText[Bytes: ByteAccess](value: Bytes): this.type  = { receiver.onText(value); this }
  def writeTag(value: Tag): this.type                        = { receiver.onTag(value); this }
  def writeSimpleValue(value: Int): this.type                = { receiver.onSimpleValue(value); this }

  def writeBytesStart(): this.type = { receiver.onBytesStart(); this }
  def writeTextStart(): this.type  = { receiver.onTextStart(); this }

  def writeArrayHeader(length: Int): this.type  = writeArrayHeader(length.toLong)
  def writeArrayHeader(length: Long): this.type = { receiver.onArrayHeader(length); this }
  def writeArrayStart(): this.type              = { receiver.onArrayStart(); this }

  def writeMapHeader(length: Int): this.type  = writeMapHeader(length.toLong)
  def writeMapHeader(length: Long): this.type = { receiver.onMapHeader(length); this }
  def writeMapStart(): this.type              = { receiver.onMapStart(); this }

  def writeBreak(): this.type = { receiver.onBreak(); this }

  def writeEndOfInput(): this.type = { receiver.onEndOfInput(); this }

  /**
   * Writes the given bytes, which must be the complete, already encoded representation of exactly one element
   * in the format of this [[Writer]] (i.e. CBOR or JSON), as is to the output.
   * Structurally the given bytes count as a single element, e.g. as one array element or one map key or value,
   * but their contents are NOT validated, so passing anything else than exactly one properly encoded element
   * results in broken output!
   *
   * The main use case for this is the re-encoding of values with exactly the same bytes as they were originally
   * decoded from (see `Reader.readWithRawBytes`), e.g. because the bytes are hashed or signed.
   *
   * NOTE: When transcoding the given bytes are decoded and forwarded as individual data items.
   */
  def writeValueAsRawBytes[Bytes](value: Bytes)(using ba: ByteAccess[Bytes]): this.type =
    if (ba.isEmpty(value))
      throw new Borer.Error.InvalidInputData(output, "Raw value bytes must not be empty")
    if (output ne null) receiver.onRawValue(value)
    else transcodeRawValue(ba.toByteArray(value))
    this

  def write[T](value: T)(using encoder: Encoder[T]): this.type =
    encoder.write(this, value).asInstanceOf[this.type]

  def writeEmptyArray(): this.type = writeArrayOpen(0).writeArrayClose()

  def writeToArray[T: Encoder](x: T): this.type =
    writeArrayOpen(1).write(x).writeArrayClose()

  def writeToArray[A: Encoder, B: Encoder](a: A, b: B): this.type =
    writeArrayOpen(2).write(a).write(b).writeArrayClose()

  def writeToArray[A: Encoder, B: Encoder, C: Encoder](a: A, b: B, c: C): this.type =
    writeArrayOpen(3).write(a).write(b).write(c).writeArrayClose()

  def writeEmptyMap(): this.type = if (writingJson) writeMapStart().writeBreak() else writeMapHeader(0)

  def writeIndexedSeq[T: Encoder](x: IndexedSeq[T]): this.type =
    @tailrec def rec(ix: Int): Unit =
      if (ix < x.size)
        write(x(ix))
        rec(ix + 1)
    writeArrayOpen(x.size)
    rec(0)
    writeArrayClose()

  def writeLinearSeq[T: Encoder](x: LinearSeq[T]): this.type =
    @tailrec def rec(x: LinearSeq[T]): Unit =
      if (x.nonEmpty)
        write(x.head)
        rec(x.tail)
    if (writingJson || x.nonEmpty)
      writeArrayStart()
      rec(x)
      writeBreak()
    else writeEmptyArray()

  def writeIterableOnce[T: Encoder](iterableOnce: IterableOnce[T]): this.type =
    val size = iterableOnce.knownSize
    if (size > 0)
      writeArrayOpen(size)
      val iterator = iterableOnce.iterator
      while (iterator.hasNext) write(iterator.next())
      writeArrayClose()
    else if (size < 0) writeIterator(iterableOnce.iterator)
    else writeEmptyArray()

  def writeIterator[T: Encoder](iterator: Iterator[T]): this.type =
    if (iterator.hasNext)
      writeArrayStart()
      while (iterator.hasNext) write(iterator.next())
      writeBreak()
    else writeEmptyArray()

  def writeBytesIterator[Bytes: ByteAccess](iterator: Iterator[Bytes]): this.type =
    writeBytesStart()
    while (iterator.hasNext) writeBytes(iterator.next())
    writeBreak()

  def writeStringIterator(iterator: Iterator[String]): this.type =
    writeTextStart()
    while (iterator.hasNext) writeString(iterator.next())
    writeBreak()

  def writeMap[A: Encoder, B: Encoder](x: Map[A, B]): this.type =
    if (x.nonEmpty)
      val iterator             = x.iterator
      def writeEntries(): Unit =
        while (iterator.hasNext)
          val (k, v) = iterator.next()
          write(k).write(v)
      if (writingJson)
        writeMapStart()
        writeEntries()
        writeBreak()
      else
        writeMapHeader(x.size)
        writeEntries()
        this
    else writeEmptyMap()

  def writeMapMember[A: Encoder, B: Encoder](key: A, value: B): this.type =
    write(key).write(value)

  def writeArrayOpen(size: Int): this.type =
    if (target eq Json) writeArrayStart() else writeArrayHeader(size)

  def writeArrayClose(): this.type =
    if (target eq Json) writeBreak() else this

  def writeMapOpen(size: Int): this.type =
    if (target eq Json) writeMapStart() else writeMapHeader(size)

  def writeMapClose(): this.type =
    if (target eq Json) writeBreak() else this

  // when transcoding we have no actual output to write the raw bytes to,
  // so we decode them and forward the resulting data items instead
  private def transcodeRawValue(bytes: Array[Byte]): Unit =
    import io.bullet.borer.DataItem as DI
    val r: Reader = if (writingJson) Json.reader(bytes) else Cbor.reader(bytes)

    def copyElement(level: Int): Unit =
      @tailrec def copyN(remaining: Long): Unit =
        if (remaining > 0)
          copyElement(level + 1)
          copyN(remaining - 1)

      @tailrec def copyUntilBreak(): Unit =
        if (r.hasBreak) r.receiveInto(receiver)
        else
          copyElement(level + 1)
          copyUntilBreak()

      if (level < 100)
        r.dataItem() match
          case DI.ArrayHeader =>
            val len = r.readArrayHeader()
            receiver.onArrayHeader(len)
            copyN(len)
          case DI.MapHeader =>
            val len = r.readMapHeader()
            receiver.onMapHeader(len)
            copyN(len << 1)
          case DI.ArrayStart | DI.MapStart | DI.BytesStart | DI.TextStart =>
            r.receiveInto(receiver)
            copyUntilBreak()
          case DI.Tag =>
            r.receiveInto(receiver)
            copyElement(level) // a tag is only a prefix to the actual element
          case DI.EndOfInput => r.unexpectedDataItem(expected = "raw value element")
          case _             => r.receiveInto(receiver)
      else r.overflow("Raw values more than 100 levels deep cannot be transcoded")

    copyElement(0)
    r.readEndOfInput()

object Writer:

  trait Config extends CommonConfig:
    def compressFloatingPointValues: Boolean

  /**
   * Simple encapsulation of encoding logic in a stand-alone object.
   */
  final case class Script(encode: Writer => Writer)

  object Script:
    val Undefined  = Script(_.writeUndefined())
    val BytesStart = Script(_.writeBytesStart())
    val TextStart  = Script(_.writeTextStart())
    val ArrayStart = Script(_.writeArrayStart())
    val MapStart   = Script(_.writeMapStart())
    val Break      = Script(_.writeBreak())

    given Encoder[Script] = Encoder((w, x) => x.encode(w))
