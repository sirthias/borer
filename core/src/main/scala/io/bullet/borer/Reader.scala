/*
 * Copyright (c) 2019-2023 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import java.nio.charset.StandardCharsets
import io.bullet.borer.internal.{ElementDeque, Parser, Receptacle, Util}
import io.bullet.borer.json.JsonParser

import scala.annotation.tailrec
import scala.collection.{mutable, Factory}

/**
 * Stateful, mutable abstraction for reading a stream of CBOR or JSON data from the given `input`.
 */
final class InputReader[Config <: Reader.Config](
    parser: Parser[_],
    directParser: io.bullet.borer.json.DirectJsonParser,
    receiverWrapper: Receiver.Transformer[Config],
    config: Config,
    val target: Target):

  import io.bullet.borer.{DataItem => DI}

  private[this] val configReadIntegersAlsoAsFloatingPoint = config.readIntegersAlsoAsFloatingPoint
  private[this] val configReadDoubleAlsoAsFloat           = config.readDoubleAlsoAsFloat
  private[this] val receptacle: Receptacle                = new Receptacle
  private[this] val receiver: Receiver                    = receiverWrapper(receptacle, config)
  private[this] var _dataItem: Int                        = _

  // a stash of elements that are injected _before_ the next element from the parser,
  // if null or empty the next element comes from the parser
  private[borer] var stash: ElementDeque = _

  private[borer] def release(): Unit =
    if (directParser eq null)
      parser match
        case x: JsonParser[_] => x.release()
        case _                => //
    else directParser.release()

  def dataItem(): Int =
    def pullFromStash() =
      if (stash.isEmpty)
        stash = stash.next
        dataItem()
      else stash.pull(receptacle)

    if (_dataItem == DI.None)
      _dataItem =
        if (stash ne null) pullFromStash()
        else if (directParser ne null) directParser.pull(receiver)
        else parser.pull(receiver)
    _dataItem

  private[borer] def receiveInto(rcv: Receiver): Int =
    val result = dataItem()
    receptacle.pushInto(rcv, result)
    clearDataItem()
    result

  inline def readingJson: Boolean = target eq Json
  inline def readingCbor: Boolean = target eq Cbor

  def input: Input[_]                 = if (directParser ne null) directParser.input else parser.input
  def cursor: Long                    = if (directParser ne null) directParser.valueIndex else parser.valueIndex
  inline def position: Input.Position = input.position(cursor)

  /**
   * Checks whether the next data item is of the given type.
   *
   * Example: reader.has(DataItem.Int)
   */
  inline def has(item: Int): Boolean = dataItem() == item

  /**
   * Checks whether the next data item type is masked in the given bit mask.
   *
   * Example: reader.hasAnyOf(DataItem.Int | DataItem.Float)
   */
  inline def hasAnyOf(mask: Int): Boolean = (dataItem() & mask) != 0

  inline def apply[T: Decoder]: T = read[T]()

  def readNull(): Null       = if (hasNull) ret(null) else unexpectedDataItem(expected = "null")
  def hasNull: Boolean       = has(DI.Null)
  def tryReadNull(): Boolean = clearIfTrue(hasNull)

  def readUndefined(): this.type  = if (hasUndefined) ret(this) else unexpectedDataItem(expected = "undefined")
  def hasUndefined: Boolean       = has(DI.Undefined)
  def tryReadUndefined(): Boolean = clearIfTrue(hasUndefined)

  def readBoolean(): Boolean =
    if (hasBoolean)
      clearDataItem()
      receptacle.boolValue
    else unexpectedDataItem(expected = "Bool")
  def hasBoolean: Boolean                     = has(DI.Boolean)
  def hasBoolean(value: Boolean): Boolean     = hasBoolean && receptacle.boolValue == value
  def tryReadBoolean(value: Boolean): Boolean = clearIfTrue(hasBoolean(value))

  def readChar(): Char =
    if (hasChar)
      clearDataItem()
      receptacle.intValue.toChar
    else unexpectedDataItem(expected = "Char")
  def hasChar: Boolean                  = hasInt && Util.isChar(receptacle.intValue)
  def hasChar(value: Char): Boolean     = hasChar && receptacle.intValue == value.toInt
  def tryReadChar(value: Char): Boolean = clearIfTrue(hasChar(value))

  def readByte(): Byte =
    if (hasByte)
      clearDataItem()
      receptacle.intValue.toByte
    else unexpectedDataItem(expected = "Byte")
  def hasByte: Boolean                  = hasInt && Util.isByte(receptacle.intValue)
  def hasByte(value: Byte): Boolean     = hasByte && receptacle.intValue == value.toInt
  def tryReadByte(value: Byte): Boolean = clearIfTrue(hasByte(value))

  def readShort(): Short =
    if (hasShort)
      clearDataItem()
      receptacle.intValue.toShort
    else unexpectedDataItem(expected = "Short")
  def hasShort: Boolean                   = hasInt && Util.isShort(receptacle.intValue)
  def hasShort(value: Short): Boolean     = hasShort && receptacle.intValue == value.toInt
  def tryReadShort(value: Short): Boolean = clearIfTrue(hasShort(value))

  def readInt(): Int =
    if (hasInt)
      clearDataItem()
      receptacle.intValue
    else unexpectedDataItem(expected = "Int")
  def hasInt: Boolean                 = has(DI.Int)
  def hasInt(value: Int): Boolean     = hasInt && receptacle.intValue == value
  def tryReadInt(value: Int): Boolean = clearIfTrue(hasInt(value))

  def readLong(): Long =
    if (hasLong)
      val result = if (hasInt) receptacle.intValue.toLong else receptacle.longValue
      clearDataItem()
      result
    else unexpectedDataItem(expected = "Long")
  def hasLong: Boolean = hasAnyOf(DI.Int | DI.Long)
  def hasLong(value: Long): Boolean =
    hasInt && (receptacle.intValue.toLong == value) || hasLong && (receptacle.longValue == value)
  def tryReadLong(value: Long): Boolean = clearIfTrue(hasLong(value))

  /**
   * Returns one of the following 4 values:
   * - Int.MaxValue if the next data item is not a Long
   * - minus one a if the next data item is a Long < `value`
   * - zero if the next data item is a Long == `value`
   * - one if the next data item is a Long > `value`
   */
  def longCompare(value: Long): Int =
    if (hasLong)
      val long = if (hasInt) receptacle.intValue.toLong else receptacle.longValue
      math.signum(long - value).toInt
    else Int.MaxValue

  def tryReadLongCompare(value: Long): Int =
    val result = longCompare(value)
    if (result == 0) clearDataItem()
    result

  def readOverLong(): Long =
    if (hasOverLong)
      clearDataItem()
      receptacle.longValue
    else unexpectedDataItem(expected = "OverLong")
  def hasOverLong: Boolean                  = has(DI.OverLong)
  def hasOverLong(value: Long): Boolean     = hasOverLong && receptacle.longValue == value
  def tryReadOverLong(value: Long): Boolean = clearIfTrue(hasOverLong(value))

  def overLongNegative: Boolean =
    if (hasOverLong) receptacle.boolValue else unexpectedDataItem(expected = "OverLong")

  def readFloat16(): Float =
    if (hasFloat16)
      clearDataItem()
      receptacle.floatValue
    else unexpectedDataItem(expected = "Float16")
  def hasFloat16: Boolean                   = has(DI.Float16)
  def hasFloat16(value: Float): Boolean     = hasFloat16 && receptacle.floatValue == value
  def tryReadFloat16(value: Float): Boolean = clearIfTrue(hasFloat16(value))

  def readFloat(): Float =
    val result =
      dataItem() match
        case DI.Float16 | DI.Float                            => receptacle.floatValue
        case DI.Double if configReadDoubleAlsoAsFloat         => receptacle.doubleValue.toFloat
        case DI.Int if configReadIntegersAlsoAsFloatingPoint  => receptacle.intValue.toFloat
        case DI.Long if configReadIntegersAlsoAsFloatingPoint => receptacle.longValue.toFloat
        case DI.NumberString                                  => java.lang.Float.parseFloat(receptacle.stringValue)
        case _                                                => unexpectedDataItem(expected = "Float")
    clearDataItem()
    result

  def hasFloat: Boolean =
    hasAnyOf(DI.Float16 | DI.Float | DI.NumberString) ||
      configReadIntegersAlsoAsFloatingPoint && hasLong ||
      configReadDoubleAlsoAsFloat && has(DI.Double)
  def hasFloat(value: Float): Boolean     = hasFloat && receptacle.floatValue == value
  def tryReadFloat(value: Float): Boolean = clearIfTrue(hasFloat(value))

  def readDouble(): Double =
    val result = dataItem() match
      case DI.Double                                        => receptacle.doubleValue
      case DI.Float16 | DI.Float                            => receptacle.floatValue.toDouble
      case DI.Int if configReadIntegersAlsoAsFloatingPoint  => receptacle.intValue.toDouble
      case DI.Long if configReadIntegersAlsoAsFloatingPoint => receptacle.longValue.toDouble
      case DI.NumberString                                  => java.lang.Double.parseDouble(receptacle.stringValue)
      case _                                                => unexpectedDataItem(expected = "Double")
    clearDataItem()
    result

  def hasDouble: Boolean =
    hasAnyOf(DI.Float16 | DI.Float | DI.Double | DI.NumberString) || configReadIntegersAlsoAsFloatingPoint && hasLong
  def hasDouble(value: Double): Boolean     = hasDouble && receptacle.doubleValue == value
  def tryReadDouble(value: Double): Boolean = clearIfTrue(hasDouble(value))

  def readNumberString(): String =
    if (hasNumberString) ret(receptacle.stringValue)
    else unexpectedDataItem(expected = "NumberString")
  def hasNumberString: Boolean                    = has(DI.NumberString)
  def hasNumberString(value: String): Boolean     = hasNumberString && stringCompare(value) == 0
  def tryReadNumberString(value: String): Boolean = clearIfTrue(hasNumberString(value))

  def readByteArray(): Array[Byte] = readBytes[Array[Byte]]()
  inline def hasByteArray: Boolean = hasBytes

  def readBytes[Bytes: ByteAccess](): Bytes =
    dataItem() match
      case DI.Bytes      => readSizedBytes()
      case DI.BytesStart => readUnsizedBytes()
      case _             => unexpectedDataItem(expected = "Bytes")
  def hasBytes: Boolean = hasAnyOf(DI.Bytes | DI.BytesStart)

  def readSizedBytes[Bytes]()(using byteAccess: ByteAccess[Bytes]): Bytes =
    if (hasSizedBytes) ret(receptacle.getBytes)
    else unexpectedDataItem(expected = "Bounded Bytes")
  def hasSizedBytes: Boolean = has(DI.Bytes)

  def readBytesStart(): this.type =
    if (tryReadBytesStart()) this else unexpectedDataItem(expected = "Unbounded Bytes-Start")
  def hasBytesStart: Boolean       = has(DI.BytesStart)
  def tryReadBytesStart(): Boolean = clearIfTrue(hasBytesStart)

  def readUnsizedBytes[Bytes]()(using byteAccess: ByteAccess[Bytes]): Bytes =
    if (hasUnsizedBytes) bufferUnsizedBytes().readSizedBytes()
    else unexpectedDataItem(expected = "Unbounded Bytes")
  def hasUnsizedBytes: Boolean = hasBytesStart

  def bufferUnsizedBytes[Bytes]()(using byteAccess: ByteAccess[Bytes]): this.type =
    if (tryReadBytesStart())
      var result = byteAccess.empty
      while (!tryReadBreak()) result = byteAccess.concat(result, readBytes())
      receptacle.onBytes(result)
      _dataItem = DI.Bytes
    this

  def readString(): String =
    dataItem() match
      case DI.Chars     => ret(new String(receptacle.charBufValue, 0, receptacle.intValue))
      case DI.String    => ret(receptacle.stringValue)
      case DI.Text      => stringOf(readSizedTextBytes[Array[Byte]]())
      case DI.TextStart => stringOf(readUnsizedTextBytes[Array[Byte]]())
      case _            => unexpectedDataItem(expected = "String or Text-Bytes")
  def readString(s: String): this.type = if (tryReadString(s)) this else unexpectedDataItem(expected = s""""$s"""")
  def hasString: Boolean               = hasAnyOf(DI.String | DI.Chars | DI.Text | DI.TextStart)

  /**
   * Tests the next data item for equality with the given [[String]].
   * NOTE: This method causes text bytes (sized or unsized) to be buffered and converted to Chars data items!
   */
  def hasString(value: String): Boolean =
    dataItem() match
      case DI.Chars               => Util.charsStringCompare(receptacle.charBufValue, receptacle.intValue, value) == 0
      case DI.String              => receptacle.stringValue == value
      case DI.Text | DI.TextStart => decodeTextBytes().hasString(value)
      case _                      => false

  /**
   * Tests the next data item for equality with the given [[String]] and advances the cursor if so.
   * NOTE: This method causes text bytes (sized or unsized) to be buffered and converted to Chars data items!
   */
  def tryReadString(value: String): Boolean = clearIfTrue(hasString(value))

  /**
   * Returns one of the following 4 values:
   * - Int.MinValue if the next data item is not a string
   * - a negative value (!= Int.MinValue) a if the next data item is a string that compares as '<' to `value`
   * - zero if the next data item is a string that compares as '==' to `value`
   * - a positive value if the next data item is a string that compares as '>' to `value`
   *
   * NOTE: This method causes text bytes (sized or unsized) to be buffered and converted to Chars data items!
   */
  def stringCompare(value: String): Int =
    dataItem() match
      case DI.Chars               => Util.charsStringCompare(receptacle.charBufValue, receptacle.intValue, value)
      case DI.String              => receptacle.stringValue.compareTo(value)
      case DI.Text | DI.TextStart => decodeTextBytes().stringCompare(value)
      case _                      => Int.MinValue

  /**
   * Returns one of the following 4 values:
   * - Int.MinValue if the next data item is not a string
   * - a negative value (!= Int.MinValue) a if the next data item is a string that compares as '<' to `value`
   * - zero if the next data item is a string that compares as '==' to `value`
   * - a positive value if the next data item is a string that compares as '>' to `value`
   *
   * Advanced the cursor if the return value is zero.
   *
   * NOTE: This method causes text bytes (sized or unsized) to be buffered and converted to Chars data items!
   */
  def tryReadStringCompare(value: String): Int =
    val result = stringCompare(value)
    if (result == 0) clearDataItem()
    result

  def readChars(): Array[Char] =
    dataItem() match
      case DI.Chars     => ret(java.util.Arrays.copyOf(receptacle.charBufValue, receptacle.intValue))
      case DI.String    => ret(receptacle.stringValue.toCharArray)
      case DI.Text      => Utf8.decode(readSizedTextBytes[Array[Byte]]())
      case DI.TextStart => Utf8.decode(readUnsizedTextBytes[Array[Byte]]())
      case _            => unexpectedDataItem(expected = "String or Text-Bytes")

  def readChars(chars: Array[Char]): this.type =
    if (tryReadChars(chars)) this
    else unexpectedDataItem(expected = s""""${new String(chars)}"""")
  inline def hasChars: Boolean = hasString

  /**
   * Tests the next data item for equality with the given `Array[Char]`.
   * NOTE: This method causes text bytes (sized or unsized) to be buffered and converted to Chars data items!
   */
  def hasChars(value: Array[Char]): Boolean =
    dataItem() match
      case DI.Chars               => Util.charsCharsCompare(receptacle.charBufValue, receptacle.intValue, value) == 0
      case DI.String              => Util.charsStringCompare(value, value.length, receptacle.stringValue) == 0
      case DI.Text | DI.TextStart => decodeTextBytes().hasChars(value)
      case _                      => false

  /**
   * Tests the next data item for equality with the given `Array[Char]` and advances the cursor if so.
   * NOTE: This method causes text bytes (sized or unsized) to be buffered and converted to Chars data items!
   */
  def tryReadChars(value: Array[Char]): Boolean = clearIfTrue(hasChars(value))

  /**
   * Returns one of the following 4 values:
   * - Int.MinValue if the next data item is not a string
   * - a negative value (!= Int.MinValue) a if the next data item is a string that compares as '<' to `value`
   * - zero if the next data item is a string that compares as '==' to `value`
   * - a positive value if the next data item is a string that compares as '>' to `value`
   *
   * NOTE: This method causes text bytes (sized or unsized) to be buffered and converted to Chars data items!
   */
  def charsCompare(value: Array[Char]): Int =
    dataItem() match
      case DI.Chars               => Util.charsCharsCompare(receptacle.charBufValue, receptacle.intValue, value)
      case DI.String              => -Util.charsStringCompare(value, value.length, receptacle.stringValue)
      case DI.Text | DI.TextStart => decodeTextBytes().charsCompare(value)
      case _                      => Int.MinValue

  /**
   * Returns one of the following 4 values:
   * - Int.MinValue if the next data item is not a string
   * - a negative value (!= Int.MinValue) a if the next data item is a string that compares as '<' to `value`
   * - zero if the next data item is a string that compares as '==' to `value`
   * - a positive value if the next data item is a string that compares as '>' to `value`
   *
   * Advanced the cursor if the return value is zero.
   *
   * NOTE: This method causes text bytes (sized or unsized) to be buffered and converted to Chars data items!
   */
  def tryReadCharsCompare(value: Array[Char]): Int =
    val result = charsCompare(value)
    if (result == 0) clearDataItem()
    result

  def readTextBytes[Bytes: ByteAccess](): Bytes =
    dataItem() match
      case DI.Text      => readSizedTextBytes()
      case DI.TextStart => readUnsizedTextBytes()
      case _            => unexpectedDataItem(expected = "Text-Bytes")
  def hasTextBytes: Boolean = hasAnyOf(DI.Text | DI.TextStart)

  def readSizedTextBytes[Bytes]()(using byteAccess: ByteAccess[Bytes]): Bytes =
    if (hasSizedTextBytes) ret(receptacle.getBytes)
    else unexpectedDataItem(expected = "Bounded Text-Bytes")
  def hasSizedTextBytes: Boolean = has(DI.Text)

  def readTextStart(): this.type =
    if (tryReadTextStart()) this else unexpectedDataItem(expected = "Unbounded Text-Start")
  def hasTextStart: Boolean       = has(DI.TextStart)
  def tryReadTextStart(): Boolean = clearIfTrue(hasTextStart)

  def readUnsizedTextBytes[Bytes]()(using byteAccess: ByteAccess[Bytes]): Bytes =
    if (hasUnsizedTextBytes) bufferUnsizedTextBytes().readSizedTextBytes()
    else unexpectedDataItem(expected = "Unbounded Text Bytes")
  def hasUnsizedTextBytes: Boolean = hasTextStart

  /**
   * If the current data item is an unsized Text item it'll be buffered and converted into a sized text item.
   */
  def bufferUnsizedTextBytes[Bytes]()(using byteAccess: ByteAccess[Bytes]): this.type =
    if (tryReadTextStart())
      var result = byteAccess.empty
      while (!tryReadBreak()) result = byteAccess.concat(result, readTextBytes())
      receptacle.onText(result)
      _dataItem = DI.Text
    this

  /**
   * If the current data item is a sized or unsized Text item it'll be buffered and decoded into a Chars data item.
   */
  @tailrec def decodeTextBytes(): this.type =
    dataItem() match
      case DI.Text =>
        receptacle.onChars(Utf8.decode(receptacle.getBytes[Array[Byte]]))
        _dataItem = DI.Chars
        this
      case DI.TextStart => bufferUnsizedTextBytes[Array[Byte]]().decodeTextBytes()
      case _            => this

  def readArrayHeader(): Long =
    if (hasArrayHeader)
      val result = receptacle.longValue
      clearDataItem()
      result
    else unexpectedDataItem(expected = "Array-Header")
  def hasArrayHeader: Boolean                 = has(DI.ArrayHeader)
  def hasArrayHeader(length: Int): Boolean    = hasArrayHeader(length.toLong)
  def hasArrayHeader(length: Long): Boolean   = hasArrayHeader && receptacle.longValue == length
  def readArrayHeader(length: Int): this.type = readArrayHeader(length.toLong)

  def readArrayHeader(length: Long): this.type =
    if (tryReadArrayHeader(length)) this else unexpectedDataItem(expected = s"Array-Header($length)")
  def tryReadArrayHeader(length: Int): Boolean  = tryReadArrayHeader(length.toLong)
  def tryReadArrayHeader(length: Long): Boolean = clearIfTrue(hasArrayHeader(length))

  def readArrayStart(): this.type  = if (tryReadArrayStart()) this else unexpectedDataItem(expected = "Array-Start")
  def hasArrayStart: Boolean       = has(DI.ArrayStart)
  def tryReadArrayStart(): Boolean = clearIfTrue(hasArrayStart)

  def readMapHeader(): Long =
    if (hasMapHeader)
      val result = receptacle.longValue
      clearDataItem()
      result
    else unexpectedDataItem(expected = "Map-Header")
  def hasMapHeader: Boolean                 = has(DI.MapHeader)
  def hasMapHeader(length: Int): Boolean    = hasMapHeader(length.toLong)
  def hasMapHeader(length: Long): Boolean   = hasMapHeader && receptacle.longValue == length
  def readMapHeader(length: Int): this.type = readMapHeader(length.toLong)

  def readMapHeader(length: Long): this.type =
    if (tryReadMapHeader(length)) this else unexpectedDataItem(expected = s"Map-Header($length)")
  def tryReadMapHeader(length: Int): Boolean  = tryReadMapHeader(length.toLong)
  def tryReadMapHeader(length: Long): Boolean = clearIfTrue(hasMapHeader(length))

  def readMapStart(): this.type  = if (tryReadMapStart()) this else unexpectedDataItem(expected = "Map-Start")
  def hasMapStart: Boolean       = has(DI.MapStart)
  def tryReadMapStart(): Boolean = clearIfTrue(hasMapStart)

  def readBreak(): this.type  = if (tryReadBreak()) this else unexpectedDataItem(expected = "BREAK")
  def hasBreak: Boolean       = has(DI.Break)
  def tryReadBreak(): Boolean = clearIfTrue(hasBreak)

  def readTag(): Tag =
    if (hasTag) ret(receptacle.tagValue)
    else unexpectedDataItem(expected = "Tag")
  def hasTag: Boolean               = has(DI.Tag)
  def hasTag(tag: Tag): Boolean     = hasTag && receptacle.tagValue == tag
  def readTag(tag: Tag): this.type  = if (tryReadTag(tag)) this else unexpectedDataItem(expected = tag.toString)
  def tryReadTag(tag: Tag): Boolean = clearIfTrue(hasTag(tag))

  def readSimpleValue(): Int =
    if (hasSimpleValue)
      val result = receptacle.intValue
      clearDataItem()
      result
    else unexpectedDataItem(expected = "Simple Value")
  def hasSimpleValue: Boolean             = has(DI.SimpleValue)
  def hasSimpleValue(value: Int): Boolean = hasSimpleValue && receptacle.intValue == value

  def readSimpleValue(value: Int): this.type =
    if (tryReadSimpleValue(value)) this else unexpectedDataItem(expected = s"Simple Value $value")
  def tryReadSimpleValue(value: Int): Boolean = clearIfTrue(hasSimpleValue(value))

  def hasEndOfInput: Boolean       = has(DI.EndOfInput)
  def readEndOfInput(): Unit       = if (!hasEndOfInput) unexpectedDataItem(expected = "End-of-Input")
  def tryReadEndOfInput(): Boolean = hasEndOfInput

  inline def read[T]()(using decoder: Decoder[T]): T = decoder.read(this)

  def readUntilBreak[M[_], T: Decoder]()(using factory: Factory[T, M[T]]): M[T] =
    @tailrec def rec(b: mutable.Builder[T, M[T]]): M[T] =
      if (tryReadBreak()) b.result() else rec(b += read[T]())
    rec(factory.newBuilder)

  def readUntilBreak[T](zero: T)(f: T => T): T =
    @tailrec def rec(acc: T): T = if (tryReadBreak()) acc else rec(f(acc))
    rec(zero)

  /**
   * Skips the current (atomic) data item.
   *
   * CAUTION: If the data item is an Array/Map - Start/Header then this call will NOT skip the whole array or map,
   * but only the starting data item! Use `skipElement` instead if you also want to skip complex elements!
   */
  def skipDataItem(): this.type =
    dataItem()
    clearDataItem()
    this

  /**
   * Moves the cursor beyond the current data element,
   * thereby also skipping complex, potentially nested array or map structures.
   */
  def skipElement(): this.type =

    // for simplicity we go for stack-based recursion here
    // if this ever becomes a problem we can upgrade to more costly heap-based recursion instead
    def skipComplex(level: Int): this.type =
      @tailrec def skipN(remaining: Long): this.type =
        if (remaining > 0)
          if (hasAnyOf(DI.Complex)) skipComplex(level + 1) else clearDataItem()
          skipN(remaining - 1)
        else this

      @tailrec def skipUntilBreak(): this.type =
        if (!tryReadBreak())
          if (hasAnyOf(DI.Complex)) skipComplex(level + 1) else clearDataItem()
          skipUntilBreak()
        else this

      if (level < 100)
        dataItem() match
          case DI.ArrayHeader => skipN(readArrayHeader())
          case DI.MapHeader =>
            val elemsToSkip = readMapHeader() << 1
            if (elemsToSkip >= 0) skipN(elemsToSkip)
            else overflow("Maps with more than 2^62 elements cannot be skipped")
          case _ =>
            clearDataItem()
            skipUntilBreak()
      else overflow("Structures more than 100 levels deep cannot be skipped") // TODO: make configurable

    if (hasAnyOf(DI.Complex)) skipComplex(0)
    else
      clearDataItem()
      this

  inline def skipTwoElements(): this.type = skipElement().skipElement()

  def readArrayOpen(arity: Long): Boolean =
    tryReadArrayStart() || { readArrayHeader(arity); false }

  def readArrayClose[T](unbounded: Boolean, value: T): T =
    if (unbounded) readBreak()
    value

  def readMapOpen(arity: Long): Boolean =
    tryReadMapStart() || { readMapHeader(arity); false }

  def readMapClose[T](unbounded: Boolean, value: T): T =
    if (unbounded) readBreak()
    value

  def validationFailure(msg: String): Nothing = throw new Borer.Error.ValidationFailure(position, msg)

  def overflow(msg: String): Nothing = throw new Borer.Error.Overflow(position, msg)

  def unexpectedDataItem(expected: String): Nothing =
    val actual = _dataItem match
      case DI.ArrayHeader => s"Array-Header(${receptacle.longValue})"
      case DI.MapHeader   => s"Map-Header(${receptacle.longValue})"
      case DI.Tag         => "Tag: " + receptacle.tagValue
      case _              => DI.stringify(_dataItem)
    unexpectedDataItem(expected, actual)

  def unexpectedDataItem(expected: String, actual: String): Nothing =
    throw new Borer.Error.InvalidInputData(position, expected, actual)

  private inline def stringOf(bytes: Array[Byte]): String =
    if (bytes.length > 0) new String(bytes, StandardCharsets.UTF_8) else ""

  private inline def clearDataItem(): Unit = _dataItem = DI.None

  private inline def ret[T](value: T): T =
    clearDataItem()
    value

  private inline def clearIfTrue(value: Boolean): Boolean = value && { clearDataItem(); true }

object Reader:

  trait Config:
    def readIntegersAlsoAsFloatingPoint: Boolean
    def readDoubleAlsoAsFloat: Boolean
