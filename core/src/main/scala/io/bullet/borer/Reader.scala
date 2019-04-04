/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import java.nio.charset.StandardCharsets
import java.math.{BigDecimal ⇒ JBigDecimal, BigInteger ⇒ JBigInteger}

import scala.annotation.tailrec
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import scala.util.control.NonFatal

/**
  * Stateful, mutable abstraction for reading a stream of CBOR or JSON data from the given `input`.
  */
final class Reader(val input: Any,
                   startCursor: Long,
                   parser: Receiver.Parser,
                   validationApplier: Receiver.Applier,
                   val config: Reader.Config,
                   val target: Borer.Target)(implicit inputAccess: InputAccess[Any]) {

  import io.bullet.borer.{DataItem ⇒ DI}

  private[this] var _cursor: Long = startCursor
  private[this] var receiver: Receiver =
    validationApplier(Validation.creator(target, config.validation), new Receptacle)
  private[this] var receptacle: Receptacle = receiver.finalTarget.asInstanceOf[Receptacle]

  @inline def cursor: Long  = _cursor
  @inline def dataItem: Int = receptacle.dataItem

  @inline def readingJson: Boolean = target eq Json
  @inline def readingCbor: Boolean = target eq Cbor

  /**
    * Checks whether this [[Reader]] currently has a data item of the given type.
    *
    * Example: reader.has(DataItem.Int)
    */
  @inline def has(item: Int): Boolean = dataItem == item

  /**
    * Checks whether this [[Reader]] currently has any of the data items masked in the given bit mask.
    *
    * Example: reader.hasAnyOf(DataItem.Int | DataItem.Float)
    */
  @inline def hasAnyOf(mask: Int): Boolean = (dataItem & mask) != 0

  @inline def apply[T: Decoder]: T = read[T]()

  @inline def hasNull: Boolean = has(DI.Null)
  def readNull(): Null         = if (hasNull) pullReturn(null) else unexpectedDataItem(expected = "null")
  def tryReadNull(): Boolean   = pullIfTrue(hasNull)

  @inline def hasUndefined: Boolean = has(DI.Undefined)
  def readUndefined(): this.type    = if (hasUndefined) pullReturn(this) else unexpectedDataItem(expected = "undefined")
  def tryReadUndefined(): Boolean   = pullIfTrue(hasUndefined)

  @inline def hasBoolean: Boolean = has(DI.Bool)
  def readBoolean(): Boolean =
    if (hasBoolean) {
      val result = receptacle.boolValue
      pull()
      result
    } else unexpectedDataItem(expected = "Bool")

  @inline def hasChar: Boolean = hasInt && Util.isChar(receptacle.intValue)
  def readChar(): Char =
    if (hasChar) {
      val result = receptacle.intValue.toChar
      pull()
      result
    } else unexpectedDataItem(expected = "Char")

  @inline def hasByte: Boolean = hasInt && Util.isByte(receptacle.intValue)
  def readByte(): Byte =
    if (hasByte) {
      val result = receptacle.intValue.toByte
      pull()
      result
    } else unexpectedDataItem(expected = "Byte")

  @inline def hasShort: Boolean = hasInt && Util.isShort(receptacle.intValue)
  def readShort(): Short =
    if (hasShort) {
      val result = receptacle.intValue.toShort
      pull()
      result
    } else unexpectedDataItem(expected = "Short")

  @inline def hasInt: Boolean = has(DI.Int)
  def readInt(): Int =
    if (hasInt) {
      val result = receptacle.intValue
      pull()
      result
    } else unexpectedDataItem(expected = "Int")

  @inline def hasLong: Boolean = hasAnyOf(DI.Int | DI.Long)
  def readLong(): Long =
    if (hasLong) {
      val result = if (dataItem == DI.Int) receptacle.intValue.toLong else receptacle.longValue
      pull()
      result
    } else unexpectedDataItem(expected = "Long")

  @inline def hasOverLong: Boolean = has(DI.OverLong)
  def overLongNegative: Boolean    = if (hasOverLong) receptacle.boolValue else unexpectedDataItem(expected = "OverLong")
  def readOverLong(): Long =
    if (hasOverLong) {
      val result = receptacle.longValue
      pull()
      result
    } else unexpectedDataItem(expected = "OverLong")

  @inline def hasFloat16: Boolean = has(DI.Float16)
  def readFloat16(): Float =
    if (hasFloat16) {
      val result = receptacle.floatValue
      pull()
      result
    } else unexpectedDataItem(expected = "Float16")

  @inline def hasFloat: Boolean = hasAnyOf(DI.Float16 | DI.Float)
  def readFloat(): Float =
    if (hasFloat) {
      val result = receptacle.floatValue
      pull()
      result
    } else unexpectedDataItem(expected = "Float")

  @inline def hasDouble: Boolean = hasAnyOf(DI.Float16 | DI.Float | DI.Double)
  def readDouble(): Double = {
    val result = dataItem match {
      case DI.Float16 | DI.Float ⇒ receptacle.floatValue.toDouble
      case DI.Double             ⇒ receptacle.doubleValue
      case _                     ⇒ unexpectedDataItem(expected = "Double")
    }
    pull()
    result
  }

  @inline def hasBigInteger: Boolean = hasAnyOf(DI.Int | DI.Long | DI.OverLong | DI.BigInteger)
  def readBigInteger(): JBigInteger =
    dataItem match {
      case DI.Int | DI.Long ⇒ JBigInteger.valueOf(readLong())
      case DI.OverLong ⇒
        def value = new JBigInteger(1, Util.toBigEndianBytes(readOverLong()))
        if (overLongNegative) value.not else value
      case DI.BigInteger ⇒ pullReturn(receptacle.bigIntegerValue)
      case _             ⇒ unexpectedDataItem(expected = "BigInteger")
    }

  @inline def hasBigDecimal: Boolean =
    hasAnyOf(DI.Int | DI.Long | DI.OverLong | DI.Float16 | DI.Float | DI.Double | DI.BigDecimal)
  def readBigDecimal(): JBigDecimal =
    if (hasLong) JBigDecimal.valueOf(readLong())
    else if (hasDouble) JBigDecimal.valueOf(readDouble())
    else if (hasBigInteger) new JBigDecimal(readBigInteger(), 0)
    else if (hasBigDecimal) pullReturn(receptacle.bigDecimalValue)
    else unexpectedDataItem(expected = "BigDecimal")

  @inline def hasByteArray: Boolean = hasBytes
  def readByteArray(): Array[Byte]  = readBytes[Array[Byte]]()

  @inline def hasBytes: Boolean = hasAnyOf(DI.Bytes | DI.BytesStart)
  def readBytes[Bytes: ByteAccess](): Bytes =
    dataItem match {
      case DI.Bytes      ⇒ readSizedBytes()
      case DI.BytesStart ⇒ readUnsizedBytes()
      case _             ⇒ unexpectedDataItem(expected = "Bytes")
    }

  @inline def hasSizedBytes: Boolean = has(DI.Bytes)
  def readSizedBytes[Bytes]()(implicit byteAccess: ByteAccess[Bytes]): Bytes =
    if (hasSizedBytes) pullReturn(receptacle.getBytes)
    else unexpectedDataItem(expected = "Bounded Bytes")

  @inline def hasBytesStart: Boolean = has(DI.BytesStart)
  def readBytesStart(): this.type =
    if (tryReadBytesStart()) this else unexpectedDataItem(expected = "Unbounded Bytes Start")
  def tryReadBytesStart(): Boolean = pullIfTrue(hasBytesStart)

  @inline def hasUnsizedBytes: Boolean = hasBytesStart
  def readUnsizedBytes[Bytes]()(implicit byteAccess: ByteAccess[Bytes]): Bytes =
    if (tryReadBytesStart()) {
      var result = byteAccess.empty
      while (!tryReadBreak()) result = byteAccess.concat(result, readBytes())
      result
    } else unexpectedDataItem(expected = "Unbounded Bytes")

  @inline def hasString: Boolean = hasAnyOf(DI.StringLike | DI.Text | DI.TextStart)
  def readString(): String =
    dataItem match {
      case DI.Chars     ⇒ pullReturn(new String(receptacle.charBufValue, receptacle.intValue, receptacle.longValue.toInt))
      case DI.String    ⇒ pullReturn(receptacle.stringValue)
      case DI.Text      ⇒ stringOf(readSizedTextBytes[Array[Byte]]())
      case DI.TextStart ⇒ stringOf(readUnsizedTextBytes[Array[Byte]]())
      case _            ⇒ unexpectedDataItem(expected = "String or Text Bytes")
    }

  def tryReadString(s: String): Boolean =
    dataItem match {
      case DI.Chars ⇒
        @tailrec def rec(buf: Array[Char], ix: Int, end: Int, six: Int): Boolean =
          ix == end || buf(ix) == s.charAt(six) && rec(buf, ix + 1, end, six + 1)
        val from  = receptacle.intValue
        val until = receptacle.longValue.toInt
        pullIfTrue((until - from) == s.length && rec(receptacle.charBufValue, from, until, 0))
      case DI.String ⇒ pullIfTrue(receptacle.stringValue == s)
      case DI.Text   ⇒ pullIfTrue(stringOf(receptacle.getBytes[Array[Byte]]) == s)
      case DI.TextStart ⇒
        val saved = saveState
        stringOf(readUnsizedTextBytes[Array[Byte]]()) == s || { restoreState(saved); false }
      case _ ⇒ false
    }

  @inline def hasTextBytes: Boolean = hasAnyOf(DI.Text | DI.TextStart)
  def readTextBytes[Bytes: ByteAccess](): Bytes =
    dataItem match {
      case DI.Text      ⇒ readSizedTextBytes()
      case DI.TextStart ⇒ readUnsizedTextBytes()
      case _            ⇒ unexpectedDataItem(expected = "Text Bytes")
    }

  @inline def hasSizedTextBytes: Boolean = has(DI.Text)
  def readSizedTextBytes[Bytes]()(implicit byteAccess: ByteAccess[Bytes]): Bytes =
    if (hasSizedTextBytes) pullReturn(receptacle.getBytes)
    else unexpectedDataItem(expected = "Bounded Text Bytes")

  @inline def hasTextStart: Boolean = has(DI.TextStart)
  def readTextStart(): this.type =
    if (tryReadTextStart()) this else unexpectedDataItem(expected = "Unbounded Text Start")
  def tryReadTextStart(): Boolean = pullIfTrue(hasTextStart)

  @inline def hasUnsizedTextBytes: Boolean = hasTextStart
  def readUnsizedTextBytes[Bytes]()(implicit byteAccess: ByteAccess[Bytes]): Bytes =
    if (tryReadTextStart()) {
      var result = byteAccess.empty
      while (!tryReadBreak()) result = byteAccess.concat(result, readTextBytes())
      result
    } else unexpectedDataItem(expected = "Unbounded Text Bytes")

  @inline def hasArrayHeader: Boolean = has(DI.ArrayHeader)
  def readArrayHeader(): Long =
    if (hasArrayHeader) {
      val result = receptacle.longValue
      pull()
      result
    } else unexpectedDataItem(expected = "Array Header")

  @inline def hasArrayHeader(length: Int): Boolean  = hasArrayHeader(length.toLong)
  @inline def hasArrayHeader(length: Long): Boolean = hasArrayHeader && receptacle.longValue == length
  def readArrayHeader(length: Int): this.type       = readArrayHeader(length.toLong)
  def readArrayHeader(length: Long): this.type =
    if (tryReadArrayHeader(length)) this else unexpectedDataItem(expected = s"Array Header ($length)")
  def tryReadArrayHeader(length: Int): Boolean  = tryReadArrayHeader(length.toLong)
  def tryReadArrayHeader(length: Long): Boolean = pullIfTrue(hasArrayHeader(length))

  @inline def hasArrayStart: Boolean = has(DI.ArrayStart)
  def readArrayStart(): this.type    = if (tryReadArrayStart()) this else unexpectedDataItem(expected = "Array Start")
  def tryReadArrayStart(): Boolean   = pullIfTrue(hasArrayStart)

  @inline def hasMapHeader: Boolean = has(DI.MapHeader)
  def readMapHeader(): Long =
    if (hasMapHeader) {
      val result = receptacle.longValue
      pull()
      result
    } else unexpectedDataItem(expected = "Map Header")

  @inline def hasMapHeader(length: Int): Boolean  = hasMapHeader(length.toLong)
  @inline def hasMapHeader(length: Long): Boolean = hasMapHeader && receptacle.longValue == length
  def readMapHeader(length: Int): this.type       = readMapHeader(length.toLong)
  def readMapHeader(length: Long): this.type =
    if (tryReadMapHeader(length)) this else unexpectedDataItem(expected = s"Map Header ($length)")
  def tryReadMapHeader(length: Int): Boolean  = tryReadMapHeader(length.toLong)
  def tryReadMapHeader(length: Long): Boolean = pullIfTrue(hasMapHeader(length))

  @inline def hasMapStart: Boolean = has(DI.MapStart)
  def readMapStart(): this.type    = if (tryReadMapStart()) this else unexpectedDataItem(expected = "Map Start")
  def tryReadMapStart(): Boolean   = pullIfTrue(hasMapStart)

  @inline def hasBreak: Boolean = has(DI.Break)
  def readBreak(): this.type    = if (tryReadBreak()) this else unexpectedDataItem(expected = "BREAK")
  def tryReadBreak(): Boolean   = pullIfTrue(hasBreak)

  @inline def hasTag: Boolean = has(DI.Tag)
  def readTag(): Tag =
    if (hasTag) pullReturn(receptacle.tagValue)
    else unexpectedDataItem(expected = "Tag")

  @inline def hasTag(tag: Tag): Boolean = hasTag && receptacle.tagValue == tag
  def readTag(tag: Tag): this.type      = if (tryReadTag(tag)) this else unexpectedDataItem(expected = tag.toString)
  def tryReadTag(tag: Tag): Boolean     = pullIfTrue(hasTag(tag))

  @inline def hasSimpleValue: Boolean = has(DI.SimpleValue)
  def readSimpleValue(): Int =
    if (hasSimpleValue) {
      val result = receptacle.intValue
      pull()
      result
    } else unexpectedDataItem(expected = "Simple Value")

  @inline def hasSimpleValue(value: Int): Boolean = hasSimpleValue && receptacle.intValue == value
  def readSimpleValue(value: Int): this.type =
    if (tryReadSimpleValue(value)) this else unexpectedDataItem(expected = s"Simple Value $value")
  def tryReadSimpleValue(value: Int): Boolean = pullIfTrue(hasSimpleValue(value))

  @inline def hasEndOfInput: Boolean       = has(DI.EndOfInput)
  @inline def readEndOfInput(): Unit       = if (!hasEndOfInput) unexpectedDataItem(expected = "End of Input")
  @inline def tryReadEndOfInput(): Boolean = hasEndOfInput

  @inline def read[T]()(implicit decoder: Decoder[T]): T = decoder.read(this)

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

  def readUntilBreak[M[_], T: Decoder]()(implicit cbf: CanBuildFrom[M[T], T, M[T]]): M[T] = {
    @tailrec def rec(b: mutable.Builder[T, M[T]]): M[T] =
      if (tryReadBreak()) b.result() else rec(b += read[T]())
    rec(cbf())
  }

  @inline def pull(): Unit = {
    receptacle.clear()
    _cursor = parser.pull(input, _cursor, receiver)
  }

  @inline private def pullReturn[T](value: T): T = {
    pull()
    value
  }

  @inline private def pullIfTrue(value: Boolean): Boolean = value && { pull(); true }

  @inline def validationFailure(msg: String): Nothing =
    throw Borer.Error.ValidationFailure(input, msg)

  @inline def overflow(msg: String): Nothing =
    throw Borer.Error.Overflow(input, msg)

  def unexpectedDataItem(expected: String): Nothing = {
    val actual = dataItem match {
      case DI.ArrayHeader ⇒ s"Array Header (${receptacle.longValue})"
      case DI.MapHeader   ⇒ s"Map Header (${receptacle.longValue})"
      case DI.Tag         ⇒ "Tag: " + receptacle.tagValue
      case _              ⇒ DataItem.stringify(dataItem)
    }
    unexpectedDataItem(expected, actual)
  }

  @inline def unexpectedDataItem(expected: String, actual: String): Nothing =
    throw Borer.Error.UnexpectedDataItem(input, expected, actual)

  @inline def saveState: Reader.SavedState = new Reader.SavedStateImpl(_cursor, receiver.copy)

  def restoreState(mark: Reader.SavedState): Unit = {
    val savedState = mark.asInstanceOf[Reader.SavedStateImpl]
    _cursor = savedState.cursor
    receiver = savedState.receiver
    receptacle = receiver.finalTarget.asInstanceOf[Receptacle]
  }

  @inline private def stringOf(bytes: Array[Byte]): String =
    if (bytes.length > 0) new String(bytes, StandardCharsets.UTF_8) else ""
}

object Reader {

  /**
    * Deserialization config settings
    *
    * @param validation               the validation settings to use or `None` if no validation should be performed
    */
  final case class Config(
      validation: Option[Validation.Config] = Some(Validation.Config())
  )

  object Config {
    val default                  = Config()
    val defaultWithoutValidation = Config(validation = None)
  }

  sealed trait SavedState {
    def cursor: Long
  }

  private final class SavedStateImpl(val cursor: Long, val receiver: Receiver) extends SavedState
}
