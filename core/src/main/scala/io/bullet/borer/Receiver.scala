/*
 * Copyright (c) 2019-2021 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

/**
  * The common interface of all types that consume CBOR data.
  * (On the reading as well as the writing side)
  */
abstract class Receiver {

  def onNull(): Unit
  def onUndefined(): Unit
  def onBoolean(value: Boolean): Unit

  def onInt(value: Int): Unit
  def onLong(value: Long): Unit
  def onOverLong(negative: Boolean, value: Long): Unit

  def onFloat16(value: Float): Unit
  def onFloat(value: Float): Unit
  def onDouble(value: Double): Unit
  def onNumberString(value: String): Unit

  def onBytes[Bytes: ByteAccess](value: Bytes): Unit
  def onBytesStart(): Unit

  def onString(value: String): Unit
  def onChars(buffer: Array[Char], length: Int): Unit
  def onText[Bytes: ByteAccess](value: Bytes): Unit
  def onTextStart(): Unit

  def onArrayHeader(length: Long): Unit
  def onArrayStart(): Unit

  def onMapHeader(length: Long): Unit
  def onMapStart(): Unit

  def onBreak(): Unit

  def onTag(value: Tag): Unit

  def onSimpleValue(value: Int): Unit

  def onEndOfInput(): Unit
}

object Receiver {

  type Transformer[Config] = (Receiver, Config) => Receiver
  private[this] val _nopTransformer: Transformer[Any] = (receiver, _) => receiver

  def nopTransformer[Config]: Transformer[Config] = _nopTransformer.asInstanceOf[Transformer[Config]]

  abstract class WithDefault extends Receiver {
    def onNull(): Unit                                   = default("`null`")
    def onUndefined(): Unit                              = default("`undefined`")
    def onBoolean(value: Boolean): Unit                  = default(s"the Boolean `$value`")
    def onInt(value: Int): Unit                          = default(s"the Int `$value`")
    def onLong(value: Long): Unit                        = default(s"the Long `$value`")
    def onOverLong(negative: Boolean, value: Long): Unit = default(s"the OverLong `$value`")
    def onFloat16(value: Float): Unit                    = default(s"the Float16 `$value`")
    def onFloat(value: Float): Unit                      = default(s"the Float `$value`")
    def onDouble(value: Double): Unit                    = default(s"the Double `$value`")
    def onNumberString(value: String): Unit              = default(s"the NumberString `$value`")

    def onBytes[Bytes: ByteAccess](value: Bytes): Unit =
      default(s"a `Bytes` value of length ${implicitly[ByteAccess[Bytes]].sizeOf(value)}")
    def onBytesStart(): Unit = default("`BytesStart`")

    def onString(value: String): Unit =
      default(s"the String `${if (value.length > 20) value.take(20) + "..." else value}`")
    def onChars(buffer: Array[Char], length: Int): Unit = default(s"Chars with length $length")

    def onText[Bytes: ByteAccess](value: Bytes): Unit =
      default(s"a `Text` value of length ${implicitly[ByteAccess[Bytes]].sizeOf(value)}")
    def onTextStart(): Unit               = default("`TextStart`")
    def onArrayHeader(length: Long): Unit = default(s"`ArrayHeader($length)")
    def onArrayStart(): Unit              = default("`ArrayStart`")
    def onMapHeader(length: Long): Unit   = default(s"`MapHeader($length)")
    def onMapStart(): Unit                = default("`MapStart`")
    def onBreak(): Unit                   = default("`Break`")
    def onTag(value: Tag): Unit           = default(s"`$value`")
    def onSimpleValue(value: Int): Unit   = default(s"`SimpleValue($value)")
    def onEndOfInput(): Unit              = default("`End Of Input`")

    protected def default(tpe: String): Unit
  }
}
