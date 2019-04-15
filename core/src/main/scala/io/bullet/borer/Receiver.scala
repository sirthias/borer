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
  def onChars(length: Int, buffer: Array[Char]): Unit
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
  abstract class Parser[Input] {

    /**
      * The [[Input]] the parser is parsing from.
      */
    def input: Input

    /**
      * The input [[Position]] for the given index.
      */
    final def pos(index: Long): Position[Input] = Position(input, index)

    /**
      * Reads the next data item from the input and sends it to the given [[Receiver]].
      * The given [[Receiver]] receives exactly one call to one of its methods.
      * The returned `Long` is the index of the next byte to consume from the input
      * (and can be used for the subsequent call to this method).
      */
    def pull(index: Long, receiver: Receiver): Long
  }

  type ParserCreator[Input, Config] = (Input, Config, InputAccess[Input]) ⇒ Parser[Input]

  type Wrapper[Config] = (Receiver, Config) ⇒ Receiver
  private[this] val _nopWrapper: Wrapper[Any] = (receiver, _) ⇒ receiver

  def nopWrapper[Config]: Wrapper[Config] = _nopWrapper.asInstanceOf[Wrapper[Config]]

  /**
    * Common parent type of [[io.bullet.borer.cbor.CborRenderer]] and [[io.bullet.borer.json.JsonRenderer]]
    */
  abstract class Renderer extends Receiver {
    def out: Output
  }

  implicit final class ReceiverOps(val underlying: Receiver) extends AnyVal {

    def finalTarget: Receiver = {
      var result = underlying
      while (result.target ne result) result = result.target
      result
    }
  }
}
