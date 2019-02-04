/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.core

trait Receiver[IO, Bytes] {

  def onNull(io: IO): IO
  def onUndefined(io: IO): IO
  def onBool(io: IO, value: Boolean): IO

  def onInt(io: IO, value: Int): IO
  def onLong(io: IO, value: Long): IO
  def onPosOverLong(io: IO, value: Long): IO
  def onNegOverLong(io: IO, value: Long): IO

  def onFloat16(io: IO, value: Float): IO
  def onFloat(io: IO, value: Float): IO
  def onDouble(io: IO, value: Double): IO

  def onBytes(io: IO, value: Bytes): IO
  def onByteArray(io: IO, value: Array[Byte]): IO
  def onBytesStart(io: IO): IO

  def onText(io: IO, value: Bytes): IO
  def onTextByteArray(io: IO, value: Array[Byte]): IO
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
  def target: Receiver[IO, Bytes]

  /**
    * Returns a deep copy of this [[Receiver]].
    * If this [[Receiver]] wraps another one then the copy must wrap a fresh copy of the wrapped target [[Receiver]].
    */
  def copy: Receiver[IO, Bytes]
}

object Receiver {

  /**
    * A creator function for [[Receiver]] instances.
    */
  type Creator[IO[_], Bytes] = Receiver[IO[Bytes], Bytes] ⇒ Receiver[IO[Bytes], Bytes]

  /**
    * A function which applies a given [[Creator]] to a given [[Receiver]] and therefore has the chance
    * to wrap either the given [[Receiver]] or the created [[Receiver]] with custom logic, e.g. [[Logging]].
    */
  type Applier[IO[_], Bytes] = (Creator[IO, Bytes], Receiver[IO[Bytes], Bytes]) ⇒ Receiver[IO[Bytes], Bytes]

  /**
    * The default [[Applier]] which simply applies the given [[Creator]] to the given [[Receiver]] without
    * any additional logic.
    */
  def defaultApplier[IO[_], Bytes]: Applier[IO, Bytes] = _defaultApplier.asInstanceOf[Applier[IO, Bytes]]

  private[this] val _defaultApplier: Applier[List, Any] = _(_)
}
