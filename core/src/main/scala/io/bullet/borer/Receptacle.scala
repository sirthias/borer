/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import java.math.{BigDecimal ⇒ JBigDecimal, BigInteger ⇒ JBigInteger}

/**
  * A [[Receiver]] which simply buffers all incoming data in fields of the appropriate type,
  * for easy querying from the outside.
  */
private[borer] final class Receptacle extends Receiver[Input] with java.lang.Cloneable {

  private[this] var _dataItem: Int = _

  private[this] var _bool: Boolean           = _
  private[this] var _int: Int                = _
  private[this] var _long: Long              = _
  private[this] var _float: Float            = _
  private[this] var _double: Double          = _
  private[this] var _bigInteger: JBigInteger = _
  private[this] var _bigDecimal: JBigDecimal = _
  private[this] var _string: String          = _
  private[this] var _tag: Tag                = _

  private[this] var _bytes: Any                   = _
  private[this] var _bytesAccess: ByteAccess[Any] = _

  def dataItem: Int = _dataItem

  def boolValue: Boolean           = _bool
  def intValue: Int                = _int
  def longValue: Long              = _long
  def floatValue: Float            = _float
  def doubleValue: Double          = _double
  def bigIntegerValue: JBigInteger = _bigInteger
  def bigDecimalValue: JBigDecimal = _bigDecimal
  def stringValue: String          = _string
  def tagValue: Tag                = _tag

  def getBytes[Bytes](implicit byteAccess: ByteAccess[Bytes]): Bytes =
    byteAccess.convert(_bytes)(_bytesAccess)

  def clear(): Unit = _dataItem = DataItem.None

  def onNull(in: Input): Input = ret(in, DataItem.Null)

  def onUndefined(in: Input): Input = ret(in, DataItem.Undefined)

  def onBool(in: Input, value: Boolean): Input = {
    _bool = value
    ret(in, DataItem.Bool)
  }

  def onInt(in: Input, value: Int): Input = {
    _int = value
    ret(in, DataItem.Int)
  }

  def onLong(in: Input, value: Long): Input = {
    _long = value
    ret(in, DataItem.Long)
  }

  def onOverLong(in: Input, negative: Boolean, value: Long): Input = {
    _bool = negative
    _long = value
    ret(in, DataItem.OverLong)
  }

  def onFloat16(in: Input, value: Float): Input = {
    _float = value
    ret(in, DataItem.Float16)
  }

  def onFloat(in: Input, value: Float): Input = {
    _float = value
    ret(in, DataItem.Float)
  }

  def onDouble(in: Input, value: Double): Input = {
    _double = value
    ret(in, DataItem.Double)
  }

  def onBigInteger(in: Input, value: JBigInteger): Input = {
    _bigInteger = value
    ret(in, DataItem.BigInteger)
  }

  def onBigDecimal(in: Input, value: JBigDecimal): Input = {
    _bigDecimal = value
    ret(in, DataItem.BigDecimal)
  }

  def onBytes[Bytes](in: Input, value: Bytes)(implicit byteAccess: ByteAccess[Bytes]): Input = {
    _bytes = value
    _bytesAccess = byteAccess.asInstanceOf[ByteAccess[Any]]
    ret(in, DataItem.Bytes)
  }

  def onBytesStart(in: Input): Input = ret(in, DataItem.BytesStart)

  def onString(in: Input, value: String): Input = {
    _string = value
    ret(in, DataItem.String)
  }

  def onText[Bytes](in: Input, value: Bytes)(implicit byteAccess: ByteAccess[Bytes]): Input = {
    _bytes = value
    _bytesAccess = byteAccess.asInstanceOf[ByteAccess[Any]]
    ret(in, DataItem.Text)
  }

  def onTextStart(in: Input): Input = ret(in, DataItem.TextStart)

  def onArrayHeader(in: Input, length: Long): Input = {
    _long = length
    ret(in, DataItem.ArrayHeader)
  }

  def onArrayStart(in: Input): Input = ret(in, DataItem.ArrayStart)

  def onMapHeader(in: Input, length: Long): Input = {
    _long = length
    ret(in, DataItem.MapHeader)
  }

  def onMapStart(in: Input): Input = ret(in, DataItem.MapStart)

  def onBreak(in: Input): Input = ret(in, DataItem.Break)

  def onTag(in: Input, value: Tag): Input = {
    _tag = value
    ret(in, DataItem.Tag)
  }

  def onSimpleValue(in: Input, value: Int): Input = {
    _int = value
    ret(in, DataItem.SimpleValue)
  }

  def onEndOfInput(in: Input): Input = ret(in, DataItem.EndOfInput)

  def target = this

  def copy = super.clone().asInstanceOf[Receptacle]

  private def ret(in: Input, dataItem: Int): Input = {
    _dataItem = dataItem
    in
  }
}
