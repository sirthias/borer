/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.core

/**
  * A [[Receiver]] which simply buffers all incoming data in fields of the appropriate
  * type, for easy querying from the outside.
  */
final class BufferingReceiver[In, Bytes] extends Receiver[In, Bytes] with java.lang.Cloneable {

  private[this] var _dataItem: Int = _

  private[this] var _bool: Boolean  = _
  private[this] var _int: Int       = _
  private[this] var _long: Long     = _
  private[this] var _float: Float   = _
  private[this] var _double: Double = _
  private[this] var _bytes: Bytes   = _
  private[this] var _tag: Tag       = _

  def dataItem: Int = _dataItem

  def boolValue: Boolean  = _bool
  def intValue: Int       = _int
  def longValue: Long     = _long
  def floatValue: Float   = _float
  def doubleValue: Double = _double
  def bytesValue: Bytes   = _bytes
  def tagValue: Tag       = _tag

  def clear(): Unit = _dataItem = 0

  def onNull(in: In): In = ret(in, DataItem.Null)

  def onUndefined(in: In): In = ret(in, DataItem.Undefined)

  def onBool(in: In, value: Boolean): In = {
    _bool = value
    ret(in, DataItem.Bool)
  }

  def onInt(in: In, value: Int): In = {
    _int = value
    ret(in, DataItem.Int)
  }

  def onLong(in: In, value: Long): In = {
    _long = value
    ret(in, DataItem.Long)
  }

  def onPosOverLong(in: In, value: Long): In = {
    _long = value
    ret(in, DataItem.PosOverLong)
  }

  def onNegOverLong(in: In, value: Long): In = {
    _long = value
    ret(in, DataItem.NegOverLong)
  }

  def onFloat16(in: In, value: Float): In = {
    _float = value
    ret(in, DataItem.Float16)
  }

  def onFloat(in: In, value: Float): In = {
    _float = value
    ret(in, DataItem.Float)
  }

  def onDouble(in: In, value: Double): In = {
    _double = value
    ret(in, DataItem.Double)
  }

  def onBytes(in: In, value: Bytes): In = {
    _bytes = value
    ret(in, DataItem.Bytes)
  }

  def onByteArray(io: In, value: Array[Byte]) = throw new UnsupportedOperationException

  def onBytesStart(in: In): In = ret(in, DataItem.BytesStart)

  def onText(in: In, value: Bytes): In = {
    _bytes = value
    ret(in, DataItem.Text)
  }

  def onTextByteArray(io: In, value: Array[Byte]) = throw new UnsupportedOperationException

  def onTextStart(in: In): In = ret(in, DataItem.TextStart)

  def onArrayHeader(in: In, length: Long): In = {
    _long = length
    ret(in, DataItem.ArrayHeader)
  }

  def onArrayStart(in: In): In = ret(in, DataItem.ArrayStart)

  def onMapHeader(in: In, length: Long): In = {
    _long = length
    ret(in, DataItem.MapHeader)
  }

  def onMapStart(in: In): In = ret(in, DataItem.MapStart)

  def onBreak(in: In): In = ret(in, DataItem.Break)

  def onTag(in: In, value: Tag): In = {
    _tag = value
    ret(in, DataItem.Tag)
  }

  def onSimpleValue(in: In, value: Int): In = {
    _int = value
    ret(in, DataItem.SimpleValue)
  }

  def onEndOfInput(in: In): In = ret(in, DataItem.EndOfInput)

  def target = this

  def copy = super.clone().asInstanceOf[BufferingReceiver[In, Bytes]]

  private def ret(in: In, dataItem: Int): In = {
    _dataItem = dataItem
    in
  }
}
