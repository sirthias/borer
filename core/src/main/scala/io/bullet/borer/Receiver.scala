/*
 * Copyright (c) 2019 Mathias Doenitz
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
  def onBool(value: Boolean): Unit

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
  def onChars(buffer: Array[Char], from: Int, until: Int): Unit
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

  /**
    * The target [[Receiver]] is this [[Receiver]] wraps another one.
    * If it doesn't wrap another [[Receiver]] the method returns this instance.
    */
  def target: Receiver = this

  /**
    * Returns a deep copy of this [[Receiver]].
    * If this [[Receiver]] wraps another one then the copy must wrap a fresh copy of the wrapped target [[Receiver]].
    */
  def copy: Receiver = this
}

object Receiver {

  /**
    * Common parent type of [[io.bullet.borer.cbor.CborParser]] and [[io.bullet.borer.json.JsonParser]]
    */
  abstract class Parser {

    /**
      * Reads the next data item from the input and sends it to the given [[Receiver]].
      * The given [[Receiver]] receives exactly one call to one of its methods.
      * The returned `Long` is the index of the next byte to consume from the input
      * (and can be used for the subsequent call to this method).
      */
    def pull[Input: InputAccess](input: Input, index: Long, receiver: Receiver): Long
  }

  /**
    * Common parent type of [[io.bullet.borer.cbor.CborRenderer]] and [[io.bullet.borer.json.JsonRenderer]]
    */
  abstract class Renderer extends Receiver {
    def out: Output
  }

  /**
    * A creator function for [[Receiver]] instances.
    */
  type Creator = Receiver ⇒ Receiver

  /**
    * A function which applies a given [[Creator]] to a given [[Receiver]] and therefore has the chance
    * to wrap either the given [[Receiver]] or the created [[Receiver]] with custom logic, e.g. [[Logging]].
    */
  type Applier = (Creator, Receiver) ⇒ Receiver

  /**
    * The default [[Applier]] which simply applies the given [[Creator]] to the given [[Receiver]] without
    * any additional logic.
    */
  val defaultApplier: Applier = _(_)

  implicit final class ReceiverOps(val underlying: Receiver) extends AnyVal {

    def finalTarget: Receiver = {
      var result = underlying
      while (result.target ne result) result = result.target
      result
    }
  }

  abstract class Abstract extends Receiver {
    def onNull()                                            = default(DataItem.Null)
    def onUndefined()                                       = default(DataItem.Undefined)
    def onBool(value: Boolean)                              = default(DataItem.Bool)
    def onInt(value: Int)                                   = default(DataItem.Int)
    def onLong(value: Long)                                 = default(DataItem.Long)
    def onOverLong(negative: Boolean, value: Long)          = default(DataItem.OverLong)
    def onFloat16(value: Float)                             = default(DataItem.Float16)
    def onFloat(value: Float)                               = default(DataItem.Float)
    def onDouble(value: Double)                             = default(DataItem.Double)
    def onNumberString(value: String): Unit                 = default(DataItem.NumberString)
    def onBytes[Bytes: ByteAccess](value: Bytes)            = default(DataItem.Bytes)
    def onBytesStart()                                      = default(DataItem.BytesStart)
    def onString(value: String)                             = default(DataItem.String)
    def onChars(buffer: Array[Char], from: Int, until: Int) = default(DataItem.Chars)
    def onText[Bytes: ByteAccess](value: Bytes)             = default(DataItem.Text)
    def onTextStart()                                       = default(DataItem.TextStart)
    def onArrayHeader(length: Long)                         = default(DataItem.ArrayHeader)
    def onArrayStart()                                      = default(DataItem.ArrayStart)
    def onMapHeader(length: Long)                           = default(DataItem.MapHeader)
    def onMapStart()                                        = default(DataItem.MapStart)
    def onBreak()                                           = default(DataItem.Break)
    def onTag(value: Tag)                                   = default(DataItem.Tag)
    def onSimpleValue(value: Int)                           = default(DataItem.SimpleValue)
    def onEndOfInput()                                      = default(DataItem.EndOfInput)
    protected def default(dataItem: Int): Unit
  }
}
