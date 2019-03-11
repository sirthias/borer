/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.core

import java.nio.charset.StandardCharsets
import scala.util.control.NonFatal

/**
  * Stateful, mutable abstraction for reading a stream of CBOR data from the given [[Input]].
  */
final class Reader(startInput: Input,
                   config: Reader.Config = Reader.Config(),
                   validationApplier: Receiver.Applier[Input] = Receiver.defaultApplier) {

  private[this] var _input: Input = startInput
  private[this] var receptacle    = new BufferingReceiver[Input]
  private[this] var receiver: Receiver[Input] =
    validationApplier(Validation.creator(config.validation), receptacle)

  def input: Input  = _input
  def dataItem: Int = receptacle.dataItem

  /**
    * Checks whether this [[Reader]] currently has any of the data items masked in the given bit mask.
    *
    * Example: reader.has(DataItem.Int | DataItem.Float)
    */
  def has(mask: Int): Boolean = (dataItem & mask) != 0

  def apply[T: Decoder]: T = read[T]()

  def hasNull: Boolean       = has(DataItem.Null)
  def readNull(): Null       = if (hasNull) { pull(); null } else unexpectedDataItem(expected = "null")
  def tryReadNull(): Boolean = hasNull && { pull(); true }

  def hasUndefined: Boolean       = has(DataItem.Undefined)
  def readUndefined(): this.type  = if (tryReadUndefined()) this else unexpectedDataItem(expected = "undefined")
  def tryReadUndefined(): Boolean = hasUndefined && { pull(); true }

  def hasBoolean: Boolean = has(DataItem.Bool)
  def readBoolean(): Boolean =
    if (hasBoolean) {
      val result = receptacle.boolValue
      pull()
      result
    } else unexpectedDataItem(expected = "Bool")

  def hasChar: Boolean = hasInt && Util.isChar(receptacle.intValue)
  def readChar(): Char =
    if (hasChar) {
      val result = receptacle.intValue.toChar
      pull()
      result
    } else unexpectedDataItem(expected = "Char")

  def hasByte: Boolean = hasInt && Util.isByte(receptacle.intValue)
  def readByte(): Byte =
    if (hasByte) {
      val result = receptacle.intValue.toByte
      pull()
      result
    } else unexpectedDataItem(expected = "Byte")

  def hasShort: Boolean = hasInt && Util.isShort(receptacle.intValue)
  def readShort(): Short =
    if (hasShort) {
      val result = receptacle.intValue.toShort
      pull()
      result
    } else unexpectedDataItem(expected = "Short")

  def hasInt: Boolean = has(DataItem.Int)
  def readInt(): Int =
    if (hasInt) {
      val result = receptacle.intValue
      pull()
      result
    } else unexpectedDataItem(expected = "Int")

  def hasLong: Boolean = has(DataItem.Int | DataItem.Long)
  def readLong(): Long =
    if (hasLong) {
      val result = if (dataItem == DataItem.Int) receptacle.intValue.toLong else receptacle.longValue
      pull()
      result
    } else unexpectedDataItem(expected = "Long")

  def hasPosOverLong: Boolean = has(DataItem.PosOverLong)
  def readPosOverLong(): Long =
    if (hasPosOverLong) {
      val result = receptacle.longValue
      pull()
      result
    } else unexpectedDataItem(expected = "PosOverLong")

  def hasNegOverLong: Boolean = has(DataItem.NegOverLong)
  def readNegOverLong(): Long =
    if (hasNegOverLong) {
      val result = receptacle.longValue
      pull()
      result
    } else unexpectedDataItem(expected = "NegOverLong")

  def hasFloat16: Boolean = has(DataItem.Float16)
  def readFloat16(): Float =
    if (hasFloat16) {
      val result = receptacle.floatValue
      pull()
      result
    } else unexpectedDataItem(expected = "Float16")

  def hasFloat: Boolean =
    has(DataItem.Float) || !config.readFloat16OnlyAsFloat16 && has(DataItem.Float16)
  def readFloat(): Float =
    if (hasFloat) {
      val result = receptacle.floatValue
      pull()
      result
    } else unexpectedDataItem(expected = "Float")

  def hasDouble: Boolean =
    has(DataItem.Double) ||
      !config.readFloat16OnlyAsFloat16 && has(DataItem.Float16) ||
      !config.readFloatOnlyAsFloat && has(DataItem.Float)
  def readDouble(): Double = {
    val result = dataItem match {
      case DataItem.Double                                      ⇒ receptacle.doubleValue
      case DataItem.Float if !config.readFloatOnlyAsFloat       ⇒ receptacle.floatValue.toDouble
      case DataItem.Float16 if !config.readFloat16OnlyAsFloat16 ⇒ receptacle.floatValue.toDouble
      case _                                                    ⇒ unexpectedDataItem(expected = "Double")
    }
    pull()
    result
  }

  def hasByteArray: Boolean        = hasBytes
  def readByteArray(): Array[Byte] = readBytes[Array[Byte]]()

  def hasBytes: Boolean = has(DataItem.Bytes | DataItem.BytesStart)
  def readBytes[Bytes: ByteAccess](): Bytes =
    dataItem match {
      case DataItem.Bytes      ⇒ readSizedBytes()
      case DataItem.BytesStart ⇒ readUnsizedBytes()
      case _                   ⇒ unexpectedDataItem(expected = "Bytes")
    }

  def hasSizedBytes: Boolean = has(DataItem.Bytes)
  def readSizedBytes[Bytes]()(implicit byteAccess: ByteAccess[Bytes]): Bytes =
    if (hasSizedBytes) {
      val result = receptacle.getBytes
      pull()
      result
    } else unexpectedDataItem(expected = "Bounded Bytes")

  def hasBytesStart: Boolean = has(DataItem.BytesStart)
  def readBytesStart(): this.type =
    if (tryReadBytesStart()) this else unexpectedDataItem(expected = "Unbounded Bytes Start")
  def tryReadBytesStart(): Boolean = hasBytesStart && { pull(); true }

  def hasUnsizedBytes: Boolean = hasBytesStart
  def readUnsizedBytes[Bytes]()(implicit byteAccess: ByteAccess[Bytes]): Bytes =
    if (tryReadBytesStart()) {
      var result = byteAccess.empty
      while (!tryReadBreak()) result = byteAccess.concat(result, readBytes())
      result
    } else unexpectedDataItem(expected = "Unbounded Bytes")

  def hasString: Boolean = hasTextBytes
  def readString(): String = {
    val byteArray = readTextBytes[Array[Byte]]()
    if (byteArray.length > 0) new String(byteArray, StandardCharsets.UTF_8) else ""
  }

  def hasTextBytes: Boolean = has(DataItem.Text | DataItem.TextStart)
  def readTextBytes[Bytes: ByteAccess](): Bytes =
    dataItem match {
      case DataItem.Text      ⇒ readSizedTextBytes()
      case DataItem.TextStart ⇒ readUnsizedTextBytes()
      case _                  ⇒ unexpectedDataItem(expected = "Text Bytes")
    }

  def hasSizedTextBytes: Boolean = has(DataItem.Text)
  def readSizedTextBytes[Bytes]()(implicit byteAccess: ByteAccess[Bytes]): Bytes =
    if (hasSizedTextBytes) {
      val result = receptacle.getBytes
      pull()
      result
    } else unexpectedDataItem(expected = "Bounded Text Bytes")

  def hasTextStart: Boolean = has(DataItem.TextStart)
  def readTextStart(): this.type =
    if (tryReadTextStart()) this else unexpectedDataItem(expected = "Unbounded Text Start")
  def tryReadTextStart(): Boolean = hasTextStart && { pull(); true }

  def hasUnsizedTextBytes: Boolean = hasTextStart
  def readUnsizedTextBytes[Bytes]()(implicit byteAccess: ByteAccess[Bytes]): Bytes =
    if (tryReadTextStart()) {
      var result = byteAccess.empty
      while (!tryReadBreak()) result = byteAccess.concat(result, readTextBytes())
      result
    } else unexpectedDataItem(expected = "Unbounded Text Bytes")

  def hasArrayHeader: Boolean = has(DataItem.ArrayHeader)
  def readArrayHeader(): Long =
    if (hasArrayHeader) {
      val result = receptacle.longValue
      pull()
      result
    } else unexpectedDataItem(expected = "Array Header")

  def hasArrayHeader(length: Int): Boolean    = hasArrayHeader(length.toLong)
  def hasArrayHeader(length: Long): Boolean   = hasArrayHeader && receptacle.longValue == length
  def readArrayHeader(length: Int): this.type = readArrayHeader(length.toLong)
  def readArrayHeader(length: Long): this.type =
    if (tryReadArrayHeader(length)) this else unexpectedDataItem(expected = s"Array Header ($length)")
  def tryReadArrayHeader(length: Int): Boolean  = tryReadArrayHeader(length.toLong)
  def tryReadArrayHeader(length: Long): Boolean = hasArrayHeader(length) && { pull(); true }

  def hasArrayStart: Boolean       = has(DataItem.ArrayStart)
  def readArrayStart(): Unit       = if (hasArrayStart) pull() else unexpectedDataItem(expected = "Array Start")
  def tryReadArrayStart(): Boolean = hasArrayStart && { pull(); true }

  def hasMapHeader: Boolean = has(DataItem.MapHeader)
  def readMapHeader(): Long =
    if (hasMapHeader) {
      val result = receptacle.longValue
      pull()
      result
    } else unexpectedDataItem(expected = "Map Header")

  def hasMapHeader(length: Int): Boolean    = hasMapHeader(length.toLong)
  def hasMapHeader(length: Long): Boolean   = hasMapHeader && receptacle.longValue == length
  def readMapHeader(length: Int): this.type = readMapHeader(length.toLong)
  def readMapHeader(length: Long): this.type =
    if (tryReadMapHeader(length)) this else unexpectedDataItem(expected = s"Map Header ($length)")
  def tryReadMapHeader(length: Int): Boolean  = tryReadMapHeader(length.toLong)
  def tryReadMapHeader(length: Long): Boolean = hasMapHeader(length) && { pull(); true }

  def hasMapStart: Boolean       = has(DataItem.MapStart)
  def readMapStart(): this.type  = if (tryReadMapStart()) this else unexpectedDataItem(expected = "Map Start")
  def tryReadMapStart(): Boolean = hasMapStart && { pull(); true }

  def hasBreak: Boolean       = has(DataItem.Break)
  def readBreak(): this.type  = if (tryReadBreak()) this else unexpectedDataItem(expected = "BREAK")
  def tryReadBreak(): Boolean = hasBreak && { pull(); true }

  def hasTag: Boolean = has(DataItem.Tag)
  def readTag(): Tag =
    if (hasTag) {
      val result = receptacle.tagValue
      pull()
      result
    } else unexpectedDataItem(expected = "Tag")

  def hasTag(tag: Tag): Boolean     = hasTag && receptacle.tagValue == tag
  def readTag(tag: Tag): this.type  = if (tryReadTag(tag)) this else unexpectedDataItem(expected = tag.toString)
  def tryReadTag(tag: Tag): Boolean = hasTag(tag) && { pull(); true }

  def hasSimpleValue: Boolean = has(DataItem.SimpleValue)
  def readSimpleValue(): Int =
    if (hasSimpleValue) {
      val result = receptacle.intValue
      pull()
      result
    } else unexpectedDataItem(expected = "Simple Value")

  def hasSimpleValue(value: Int): Boolean = hasSimpleValue && receptacle.intValue == value
  def readSimpleValue(value: Int): this.type =
    if (tryReadSimpleValue(value)) this else unexpectedDataItem(expected = s"Simple Value $value")
  def tryReadSimpleValue(value: Int): Boolean = hasSimpleValue(value) && { pull(); true }

  def hasEndOfInput: Boolean       = has(DataItem.EndOfInput)
  def readEndOfInput(): Unit       = if (!hasEndOfInput) unexpectedDataItem(expected = "End of Input")
  def tryReadEndOfInput(): Boolean = hasEndOfInput

  def read[T]()(implicit decoder: Decoder[T]): T = decoder.read(this)

  /**
    * Attempts to read an instance of [[T]].
    * If this fails due to any kind of error this [[Reader]] is "reset" to the state it was before this attempted
    * read, which enables discrimination between several possible input consumption alternatives, when simple
    * one-element look-ahead doesn't suffice.
    *
    * NOTE: Saving and restoring the [[Reader]] state (as well as throwing an catching exceptions) does come with
    * some additional object allocation cost. So, if the one-element look-ahead provided by the [[Reader]] API is
    * sufficient for discriminating between cases then thas should be preferred over the use of `tryRead`.
    */
  def tryRead[T]()(implicit decoder: Decoder[T]): Option[T] = {
    val saved = saveState
    try Some(decoder.read(this))
    catch {
      case NonFatal(_) ⇒
        restoreState(saved)
        None
    }
  }

  def pull(): Unit = {
    receptacle.clear()
    _input = ByteReader.pull(_input, receiver)
  }

  def validationFailure(msg: String): Nothing =
    throw new Cbor.Error.ValidationFailure(input, msg)

  def overflow(msg: String): Nothing =
    throw new Cbor.Error.Overflow(input, msg)

  def unexpectedDataItem(expected: String): Nothing = {
    val actual = dataItem match {
      case DataItem.ArrayHeader ⇒ s"Array Header (${receptacle.longValue})"
      case DataItem.MapHeader   ⇒ s"Map Header (${receptacle.longValue})"
      case DataItem.Tag         ⇒ "Tag: " + receptacle.tagValue
      case _                    ⇒ DataItem.stringify(dataItem)
    }
    unexpectedDataItem(expected, actual)
  }

  def unexpectedDataItem(expected: String, actual: String): Nothing =
    throw new Cbor.Error.UnexpectedDataItem(input, expected, actual)

  def saveState: Reader.SavedState = {
    val clonedReceiver   = receiver.copy
    var clonedReceptacle = clonedReceiver.target
    while (clonedReceptacle.target ne clonedReceptacle) clonedReceptacle = clonedReceptacle.target
    val newBufferingReceiver = clonedReceptacle.asInstanceOf[BufferingReceiver[Input]]
    new Reader.SavedStateImpl(_input.copy, newBufferingReceiver, clonedReceiver)
  }

  def restoreState(mark: Reader.SavedState): Unit = {
    val savedState = mark.asInstanceOf[Reader.SavedStateImpl]
    _input = savedState.input
    receptacle = savedState.receptacle
    receiver = savedState.receiver
  }
}

object Reader {

  /**
    * Deserialization config settings
    *
    * @param validation               the validation settings to use or `None` if no validation should be performed
    * @param readFloat16OnlyAsFloat16 set to true in order to never allow for reading a decoded half-precision (16-bit)
    *                                 floating point value as a float or a double
    * @param readFloatOnlyAsFloat     set to true in order to never allow for reading a decoded single-precision (32-bit)
    *                                 floating point value as a double
    */
  final case class Config(
      validation: Option[Validation.Config] = Some(Validation.Config()),
      readFloat16OnlyAsFloat16: Boolean = false,
      readFloatOnlyAsFloat: Boolean = false
  )

  object Config {
    val default = Config()
  }

  sealed trait SavedState {
    def input: Input
  }

  private final class SavedStateImpl(val input: Input,
                                     val receptacle: BufferingReceiver[Input],
                                     val receiver: Receiver[Input])
      extends SavedState
}
