/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.internal

import io.bullet.borer._

import scala.annotation.tailrec

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

  private[this] var _bytesAccess: ByteAccess[Any] = _

  @inline def boolValue: Boolean        = _bool
  @inline def intValue: Int             = _int
  @inline def longValue: Long           = _long
  @inline def floatValue: Float         = _float
  @inline def doubleValue: Double       = _double
  @inline def stringValue: String       = _obj.asInstanceOf[String]
  @inline def charBufValue: Array[Char] = _obj.asInstanceOf[Array[Char]]
  @inline def tagValue: Tag             = _obj.asInstanceOf[Tag]

  @inline def getBytes[Bytes](implicit byteAccess: ByteAccess[Bytes]): Bytes =
    byteAccess.convert(_obj)(_bytesAccess)

  @inline def stringCompareBytes(string: String): Int = {
    def failIllegalArg(msg: String) = throw new IllegalArgumentException(msg)
    val input                       = _bytesAccess.inputFrom(_obj)

    @tailrec def rec(stringIx: Int): Int =
      if (stringIx < string.length) {
        var six = stringIx
        if (input.prepareRead(1)) {
          val bytesCodepoint = {
            val b = input.readByte().toInt
            if (b < 0) input.readMultiByteUtf8Codepoint(b) else b
          }
          val stringCodepoint = {
            val c = string.charAt(six)
            if (Character.isHighSurrogate(c)) {
              six += 1
              if (six < string.length) {
                val c2 = string.charAt(six)
                if (Character.isLowSurrogate(c2)) Character.toCodePoint(c, c2)
                else failIllegalArg(s"""Invalid UTF-16 surrogate pair at index $stringIx of string "$string"""")
              } else failIllegalArg(s"""Truncated UTF-16 surrogate pair at end of string "$string"""")
            } else c.toInt
          }
          val d = bytesCodepoint - stringCodepoint
          if (d == 0) rec(six + 1) else d
        } else -1
      } else if (input.prepareRead(1)) /* `string` is a prefix of the parsed string */ 1
      else 0

    rec(0)
  }

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
    _bytesAccess = byteAccess.asInstanceOf[ByteAccess[Any]]
  }

  def onBytesStart(): Unit = ()

  def onString(value: String): Unit = _obj = value

  def onChars(length: Int, buffer: Array[Char]): Unit = {
    _obj = buffer
    _int = length
  }

  def onText[Bytes](value: Bytes)(implicit byteAccess: ByteAccess[Bytes]): Unit = {
    _obj = value
    _bytesAccess = byteAccess.asInstanceOf[ByteAccess[Any]]
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
