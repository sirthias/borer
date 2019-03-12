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
  * The common interface of all types that consume CBOR data.
  * (On the reading as well as the writing side)
  */
trait Receiver[IO] {

  def onNull(io: IO): IO
  def onUndefined(io: IO): IO
  def onBool(io: IO, value: Boolean): IO

  def onInt(io: IO, value: Int): IO
  def onLong(io: IO, value: Long): IO
  def onOverLong(io: IO, negative: Boolean, value: Long): IO

  def onFloat16(io: IO, value: Float): IO
  def onFloat(io: IO, value: Float): IO
  def onDouble(io: IO, value: Double): IO

  def onBigInteger(io: IO, value: JBigInteger): IO
  def onBigDecimal(io: IO, value: JBigDecimal): IO

  def onBytes[Bytes: ByteAccess](io: IO, value: Bytes): IO
  def onBytesStart(io: IO): IO

  def onString(io: IO, value: String): IO
  def onText[Bytes: ByteAccess](io: IO, value: Bytes): IO
  def onTextStart(io: IO): IO

  def onArrayHeader(io: IO, length: Long): IO
  def onArrayStart(io: IO): IO

  def onMapHeader(io: IO, length: Long): IO
  def onMapStart(io: IO): IO

  def onBreak(io: IO): IO

  def onTag(io: IO, value: Tag): IO

  def onSimpleValue(io: IO, value: Int): IO

  def onEndOfInput(io: IO): IO

  /**
    * The target [[Receiver]] is this [[Receiver]] wraps another one.
    * If it doesn't wrap another [[Receiver]] the method returns this instance.
    */
  def target: Receiver[IO]

  /**
    * Returns a deep copy of this [[Receiver]].
    * If this [[Receiver]] wraps another one then the copy must wrap a fresh copy of the wrapped target [[Receiver]].
    */
  def copy: Receiver[IO]
}

object Receiver {

  /**
    * Common parent type of [[io.bullet.borer.cbor.CborParser]] and [[io.bullet.borer.json.JsonParser]]
    */
  abstract class Parser {

    /**
      * Reads the next data item from the input and sends it to the given [[Receiver]].
      * The given [[Receiver]] receives exactly one call to one of its methods,
      * whose result is also the return value of this call to `pull`.
      */
    def pull(input: Input, receiver: Receiver[Input]): Input
  }

  /**
    * A creator function for [[Receiver]] instances.
    */
  type Creator[IO] = Receiver[IO] ⇒ Receiver[IO]

  /**
    * A function which applies a given [[Creator]] to a given [[Receiver]] and therefore has the chance
    * to wrap either the given [[Receiver]] or the created [[Receiver]] with custom logic, e.g. [[Logging]].
    */
  type Applier[IO] = (Creator[IO], Receiver[IO]) ⇒ Receiver[IO]

  /**
    * The default [[Applier]] which simply applies the given [[Creator]] to the given [[Receiver]] without
    * any additional logic.
    */
  def defaultApplier[IO]: Applier[IO] = _defaultApplier.asInstanceOf[Applier[IO]]

  private[this] val _defaultApplier: Applier[Any] = _(_)

  implicit final class ReceiverOps[IO](val underlying: Receiver[IO]) extends AnyVal {

    def finalTarget: Receiver[IO] = {
      var result = underlying
      while (result.target ne result) result = result.target
      result
    }
  }

  abstract class Abstract[IO] extends Receiver[IO] {
    def onNull(io: IO)                                     = default(io, DataItem.Null)
    def onUndefined(io: IO)                                = default(io, DataItem.Undefined)
    def onBool(io: IO, value: Boolean)                     = default(io, DataItem.Bool)
    def onInt(io: IO, value: Int)                          = default(io, DataItem.Int)
    def onLong(io: IO, value: Long)                        = default(io, DataItem.Long)
    def onOverLong(io: IO, negative: Boolean, value: Long) = default(io, DataItem.OverLong)
    def onFloat16(io: IO, value: Float)                    = default(io, DataItem.Float16)
    def onFloat(io: IO, value: Float)                      = default(io, DataItem.Float)
    def onDouble(io: IO, value: Double)                    = default(io, DataItem.Double)
    def onBigInteger(io: IO, value: JBigInteger)           = default(io, DataItem.BigInteger)
    def onBigDecimal(io: IO, value: JBigDecimal)           = default(io, DataItem.BigDecimal)
    def onBytes[Bytes: ByteAccess](io: IO, value: Bytes)   = default(io, DataItem.Bytes)
    def onBytesStart(io: IO)                               = default(io, DataItem.BytesStart)
    def onString(io: IO, value: String)                    = default(io, DataItem.String)
    def onText[Bytes: ByteAccess](io: IO, value: Bytes)    = default(io, DataItem.Text)
    def onTextStart(io: IO)                                = default(io, DataItem.TextStart)
    def onArrayHeader(io: IO, length: Long)                = default(io, DataItem.ArrayHeader)
    def onArrayStart(io: IO)                               = default(io, DataItem.ArrayStart)
    def onMapHeader(io: IO, length: Long)                  = default(io, DataItem.MapHeader)
    def onMapStart(io: IO)                                 = default(io, DataItem.MapStart)
    def onBreak(io: IO)                                    = default(io, DataItem.Break)
    def onTag(io: IO, value: Tag)                          = default(io, DataItem.Tag)
    def onSimpleValue(io: IO, value: Int)                  = default(io, DataItem.SimpleValue)
    def onEndOfInput(io: IO)                               = default(io, DataItem.EndOfInput)
    def target                                             = throw new UnsupportedOperationException
    def copy                                               = throw new UnsupportedOperationException
    protected def default(io: IO, dataItem: Int): IO
  }
}
