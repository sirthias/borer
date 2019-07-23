/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.internal

import io.bullet.borer._

/**
  * A [[Receiver]] which simply buffers all incoming data in fields of the appropriate type,
  * for easy querying from the outside.
  */
final private[borer] class Receptacle extends Receiver with java.lang.Cloneable {

  private[this] var _bool: Boolean  = _
  private[this] var _int: Int       = _
  private[this] var _long: Long     = _
  private[this] var _float: Float   = _
  private[this] var _double: Double = _
  private[this] var _obj: Any       = _

  private[this] var _byteAccess: ByteAccess[Any] = _

  @inline def boolValue: Boolean        = _bool
  @inline def intValue: Int             = _int
  @inline def longValue: Long           = _long
  @inline def floatValue: Float         = _float
  @inline def doubleValue: Double       = _double
  @inline def stringValue: String       = _obj.asInstanceOf[String]
  @inline def charBufValue: Array[Char] = _obj.asInstanceOf[Array[Char]]
  @inline def tagValue: Tag             = _obj.asInstanceOf[Tag]

  @inline def getBytes[Bytes](implicit byteAccess: ByteAccess[Bytes]): Bytes =
    byteAccess.convert(_obj)(_byteAccess)

  def onNull(): Unit = ()

  def onUndefined(): Unit = ()

  def onBoolean(value: Boolean): Unit = _bool = value

  def onInt(value: Int): Unit = _int = value

  def onLong(value: Long): Unit = _long = value

  def onOverLong(negative: Boolean, value: Long): Unit = {
    _bool = negative
    _long = value
  }

  def onFloat16(value: Float): Unit = _float = value

  def onFloat(value: Float): Unit = _float = value

  def onDouble(value: Double): Unit = _double = value

  def onNumberString(value: String): Unit = _obj = value

  def onBytes[Bytes](value: Bytes)(implicit byteAccess: ByteAccess[Bytes]): Unit = {
    _obj = value
    _byteAccess = byteAccess.asInstanceOf[ByteAccess[Any]]
  }

  def onBytesStart(): Unit = ()

  def onString(value: String): Unit = _obj = value

  def onChars(buffer: Array[Char]): Unit = onChars(buffer, buffer.length)

  def onChars(buffer: Array[Char], length: Int): Unit = {
    _obj = buffer
    _int = length
  }

  def onText[Bytes](value: Bytes)(implicit byteAccess: ByteAccess[Bytes]): Unit = {
    _obj = value
    _byteAccess = byteAccess.asInstanceOf[ByteAccess[Any]]
  }

  def onTextStart(): Unit = ()

  def onArrayHeader(length: Long): Unit = _long = length

  def onArrayStart(): Unit = ()

  def onMapHeader(length: Long): Unit = _long = length

  def onMapStart(): Unit = ()

  def onBreak(): Unit = ()

  def onTag(value: Tag): Unit = _obj = value

  def onSimpleValue(value: Int): Unit = _int = value

  def onEndOfInput(): Unit = ()
}
