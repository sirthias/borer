/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.core

trait Receiver[IO] {

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

  def onBytes[Bytes: ByteAccess](io: IO, value: Bytes): IO
  def onBytesStart(io: IO): IO

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
}
