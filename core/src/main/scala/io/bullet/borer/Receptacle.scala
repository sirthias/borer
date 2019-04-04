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
private[borer] final class Receptacle extends Receiver with java.lang.Cloneable {

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

  @inline def dataItem: Int = _dataItem

  @inline def boolValue: Boolean           = _bool
  @inline def intValue: Int                = _int
  @inline def longValue: Long              = _long
  @inline def floatValue: Float            = _float
  @inline def doubleValue: Double          = _double
  @inline def bigIntegerValue: JBigInteger = _bigInteger
  @inline def bigDecimalValue: JBigDecimal = _bigDecimal
  @inline def stringValue: String          = _string
  @inline def tagValue: Tag                = _tag

  @inline def getBytes[Bytes](implicit byteAccess: ByteAccess[Bytes]): Bytes =
    byteAccess.convert(_bytes)(_bytesAccess)

  @inline def clear(): Unit = _dataItem = DataItem.None

  def onNull(): Unit = _dataItem = DataItem.Null

  def onUndefined(): Unit = _dataItem = DataItem.Undefined

  def onBool(value: Boolean): Unit = {
    _bool = value
    _dataItem = DataItem.Bool
  }

  def onInt(value: Int): Unit = {
    _int = value
    _dataItem = DataItem.Int
  }

  def onLong(value: Long): Unit = {
    _long = value
    _dataItem = DataItem.Long
  }

  def onOverLong(negative: Boolean, value: Long): Unit = {
    _bool = negative
    _long = value
    _dataItem = DataItem.OverLong
  }

  def onFloat16(value: Float): Unit = {
    _float = value
    _dataItem = DataItem.Float16
  }

  def onFloat(value: Float): Unit = {
    _float = value
    _dataItem = DataItem.Float
  }

  def onDouble(value: Double): Unit = {
    _double = value
    _dataItem = DataItem.Double
  }

  def onBigInteger(value: JBigInteger): Unit = {
    _bigInteger = value
    _dataItem = DataItem.BigInteger
  }

  def onBigDecimal(value: JBigDecimal): Unit = {
    _bigDecimal = value
    _dataItem = DataItem.BigDecimal
  }

  def onBytes[Bytes](value: Bytes)(implicit byteAccess: ByteAccess[Bytes]): Unit = {
    _bytes = value
    _bytesAccess = byteAccess.asInstanceOf[ByteAccess[Any]]
    _dataItem = DataItem.Bytes
  }

  def onBytesStart(): Unit = _dataItem = DataItem.BytesStart

  def onString(value: String): Unit = {
    _string = value
    _dataItem = DataItem.String
  }

  def onText[Bytes](value: Bytes)(implicit byteAccess: ByteAccess[Bytes]): Unit = {
    _bytes = value
    _bytesAccess = byteAccess.asInstanceOf[ByteAccess[Any]]
    _dataItem = DataItem.Text
  }

  def onTextStart(): Unit = _dataItem = DataItem.TextStart

  def onArrayHeader(length: Long): Unit = {
    _long = length
    _dataItem = DataItem.ArrayHeader
  }

  def onArrayStart(): Unit = _dataItem = DataItem.ArrayStart

  def onMapHeader(length: Long): Unit = {
    _long = length
    _dataItem = DataItem.MapHeader
  }

  def onMapStart(): Unit = _dataItem = DataItem.MapStart

  def onBreak(): Unit = _dataItem = DataItem.Break

  def onTag(value: Tag): Unit = {
    _tag = value
    _dataItem = DataItem.Tag
  }

  def onSimpleValue(value: Int): Unit = {
    _int = value
    _dataItem = DataItem.SimpleValue
  }

  def onEndOfInput(): Unit = _dataItem = DataItem.EndOfInput

  def target = this

  def copy = super.clone().asInstanceOf[Receptacle]
}
