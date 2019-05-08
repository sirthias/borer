/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import java.nio.charset.StandardCharsets

import io.bullet.borer.internal.{Receptacle, Util}

import scala.annotation.tailrec
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable

/**
  * Stateful, mutable abstraction for reading a stream of CBOR or JSON data from the given `input`.
  */
final class InputReader[+In <: Input, +Config <: Reader.Config](
    parser: Receiver.Parser[In],
    receiverWrapper: Receiver.Wrapper[Config],
    config: Config,
    val target: Target) {

  import io.bullet.borer.{DataItem => DI}

  private[this] val configReadIntegersAlsoAsFloatingPoint = config.readIntegersAlsoAsFloatingPoint
  private[this] val configReadDoubleAlsoAsFloat           = config.readDoubleAlsoAsFloat
  private[this] val receiver: Receiver                    = receiverWrapper(new Receptacle, config)
  private[this] val receptacle: Receptacle                = receiver.finalTarget.asInstanceOf[Receptacle]
  private[this] var _lastCursor: Long                     = _
  private[this] var _cursor: Long                         = _
  private[this] var _dataItem: Int                        = _

  @inline def dataItem: Int = _dataItem

  @inline def readingJson: Boolean = target eq Json
  @inline def readingCbor: Boolean = target eq Cbor

  @inline def input: In                 = parser.input
  @inline def lastCursor: Long          = _lastCursor
  @inline def cursor: Long              = _cursor
  @inline def lastPosition: In#Position = input.position(_lastCursor)
  @inline def position: In#Position     = input.position(_cursor)

  /**
    * Checks whether this [[Reader]] currently has a data item of the given type.
    *
    * Example: reader.has(DataItem.Int)
    */
  @inline def has(item: Int): Boolean = _dataItem == item

  /**
    * Checks whether this [[Reader]] currently has any of the data items masked in the given bit mask.
    *
    * Example: reader.hasAnyOf(DataItem.Int | DataItem.Float)
    */
  @inline def hasAnyOf(mask: Int): Boolean = (_dataItem & mask) != 0

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
      val result = if (_dataItem == DI.Int) receptacle.intValue.toLong else receptacle.longValue
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

  @inline def hasFloat: Boolean =
    hasAnyOf(DI.Float16 | DI.Float | DI.NumberString) ||
      configReadIntegersAlsoAsFloatingPoint && hasLong ||
      configReadDoubleAlsoAsFloat && hasDouble

  def readFloat(): Float = {
    val result =
      _dataItem match {
        case DI.Float16 | DI.Float                            => receptacle.floatValue
        case DI.Double if configReadDoubleAlsoAsFloat         => receptacle.doubleValue.toFloat
        case DI.Int if configReadIntegersAlsoAsFloatingPoint  => receptacle.intValue.toFloat
        case DI.Long if configReadIntegersAlsoAsFloatingPoint => receptacle.longValue.toFloat
        case DI.NumberString                                  => java.lang.Float.parseFloat(receptacle.stringValue)
        case _                                                => unexpectedDataItem(expected = "Float")
      }
    pull()
    result
  }

  @inline def hasDouble: Boolean =
    hasAnyOf(DI.Float16 | DI.Float | DI.Double | DI.NumberString) || config.readIntegersAlsoAsFloatingPoint && hasLong

  def readDouble(): Double = {
    val result = _dataItem match {
      case DI.Double                                        => receptacle.doubleValue
      case DI.Float16 | DI.Float                            => receptacle.floatValue.toDouble
      case DI.Int if configReadIntegersAlsoAsFloatingPoint  => receptacle.intValue.toDouble
      case DI.Long if configReadIntegersAlsoAsFloatingPoint => receptacle.longValue.toDouble
      case DI.NumberString                                  => java.lang.Double.parseDouble(receptacle.stringValue)
      case _                                                => unexpectedDataItem(expected = "Double")
    }
    pull()
    result
  }

  @inline def hasNumberString: Boolean = has(DI.NumberString)

  def readNumberString(): String =
    if (hasNumberString) pullReturn(receptacle.stringValue)
    else unexpectedDataItem(expected = "NumberString")

  @inline def hasByteArray: Boolean = hasBytes
  def readByteArray(): Array[Byte]  = readBytes[Array[Byte]]()

  @inline def hasBytes: Boolean = hasAnyOf(DI.Bytes | DI.BytesStart)

  def readBytes[Bytes: ByteAccess](): Bytes =
    _dataItem match {
      case DI.Bytes      => readSizedBytes()
      case DI.BytesStart => readUnsizedBytes()
      case _             => unexpectedDataItem(expected = "Bytes")
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

  @inline def hasString: Boolean = hasAnyOf(DI.String | DI.Chars | DI.Text | DI.TextStart)

  def readString(): String =
    _dataItem match {
      case DI.Chars     => pullReturn(new String(receptacle.charBufValue, 0, receptacle.intValue))
      case DI.String    => pullReturn(receptacle.stringValue)
      case DI.Text      => stringOf(readSizedTextBytes[Array[Byte]]())
      case DI.TextStart => stringOf(readUnsizedTextBytes[Array[Byte]]())
      case _            => unexpectedDataItem(expected = "String or Text Bytes")
    }

  def readString(s: String): this.type = if (tryReadString(s)) this else unexpectedDataItem(expected = '"' + s + '"')

  def tryReadString(s: String): Boolean =
    _dataItem match {
      case DI.Chars =>
        val len                                              = receptacle.intValue
        @tailrec def rec(buf: Array[Char], ix: Int): Boolean = ix == len || buf(ix) == s.charAt(ix) && rec(buf, ix + 1)
        pullIfTrue(len == s.length && rec(receptacle.charBufValue, 0))
      case DI.String => pullIfTrue(receptacle.stringValue == s)
      case DI.Text   => pullIfTrue(stringOf(receptacle.getBytes[Array[Byte]]) == s)
      case _         => false
    }

  @inline def hasTextBytes: Boolean = hasAnyOf(DI.Text | DI.TextStart)

  def readTextBytes[Bytes: ByteAccess](): Bytes =
    _dataItem match {
      case DI.Text      => readSizedTextBytes()
      case DI.TextStart => readUnsizedTextBytes()
      case _            => unexpectedDataItem(expected = "Text Bytes")
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

  def readUntilBreak[M[_], T: Decoder]()(implicit cbf: CanBuildFrom[M[T], T, M[T]]): M[T] = {
    @tailrec def rec(b: mutable.Builder[T, M[T]]): M[T] =
      if (tryReadBreak()) b.result() else rec(b += read[T]())
    rec(cbf())
  }

  def readUntilBreak[T](zero: T)(f: T => T): T = {
    @tailrec def rec(acc: T): T = if (tryReadBreak()) acc else rec(f(acc))
    rec(zero)
  }

  /**
    * Skips the current (atomic) data item.
    *
    * CAUTION: If the data item is an Array/Map - Start/Header then this call will NOT skip the whole array or map,
    * but only the starting data item! Use `skipElement` instead if you also want to skip complex elements!
    */
  def skipDataItem(): this.type = {
    pull()
    this
  }

  /**
    * Moves the cursor beyond the current data element,
    * thereby also skipping complex, potentially nested array or map structures.
    */
  def skipElement(): this.type =
    if (hasAnyOf(DI.Complex)) {
      // for simplicity we go for stack-based recursion here
      // if this ever becomes a problem we can upgrade to more costly heap-based recursion instead
      def skipComplex(level: Int): this.type = {
        @tailrec def skipN(remaining: Long): this.type =
          if (remaining > 0) {
            if (hasAnyOf(DI.Complex)) skipComplex(level + 1) else pull()
            skipN(remaining - 1)
          } else this

        @tailrec def skipUntilBreak(): this.type =
          if (!tryReadBreak()) {
            if (hasAnyOf(DI.Complex)) skipComplex(level + 1) else pull()
            skipUntilBreak()
          } else this

        if (level == 100) overflow("Structures more than 100 levels deep cannot be skipped") // TODO: make configurable
        _dataItem match {
          case DI.ArrayHeader => skipN(readArrayHeader())
          case DI.MapHeader =>
            val elemsToSkip = readMapHeader() << 1
            if (elemsToSkip >= 0) skipN(elemsToSkip)
            else overflow("Maps with more than 2^62 elements cannot be skipped")
          case DI.ArrayStart | DI.MapStart =>
            pull()
            skipUntilBreak()
        }
      }
      skipComplex(0)
    } else skipDataItem()

  @inline private def pull(): Unit = {
    _lastCursor = _cursor
    _dataItem = parser.pull(receiver)
    _cursor = parser.lastCursor
  }

  @inline private def pullReturn[T](value: T): T = {
    pull()
    value
  }

  @inline private def pullIfTrue(value: Boolean): Boolean = value && { pull(); true }

  @inline def validationFailure(msg: String): Nothing =
    throw new Borer.Error.ValidationFailure(lastPosition, msg)

  @inline def overflow(msg: String): Nothing =
    throw new Borer.Error.Overflow(lastPosition, msg)

  def unexpectedDataItem(expected: String): Nothing = {
    val actual = _dataItem match {
      case DI.ArrayHeader => s"Array Header (${receptacle.longValue})"
      case DI.MapHeader   => s"Map Header (${receptacle.longValue})"
      case DI.Tag         => "Tag: " + receptacle.tagValue
      case _              => DataItem.stringify(_dataItem)
    }
    unexpectedDataItem(expected, actual)
  }

  @inline def unexpectedDataItem(expected: String, actual: String): Nothing =
    throw new Borer.Error.InvalidInputData(lastPosition, expected, actual)

  @inline private def stringOf(bytes: Array[Byte]): String =
    if (bytes.length > 0) new String(bytes, StandardCharsets.UTF_8) else ""
}

object Reader {

  trait Config {
    def readIntegersAlsoAsFloatingPoint: Boolean
    def readDoubleAlsoAsFloat: Boolean
  }
}
