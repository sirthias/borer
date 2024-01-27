/*
 * Copyright (c) 2019-2023 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import scala.language.implicitConversions

case class Default[+T](defaultValue: T)

object Default:
  given boolean: Default[Boolean] = Default(false)
  given byte: Default[Byte]       = Default(0: Byte)
  given short: Default[Short]     = Default(0: Short)
  given int: Default[Int]         = Default(0)
  given long: Default[Long]       = Default(0L)
  given string: Default[String]   = Default("")
  given float: Default[Float]     = Default(0.0f)
  given double: Default[Double]   = Default(0.0)

  inline def of[T](using d: Default[T]): T = d.defaultValue

  def orValue[T: Default](value: T): T = if (value == null) Default.of[T] else value

  private[this] val optionDefault     = Default(None)
  given option[T]: Default[Option[T]] = optionDefault

opaque type Nullable[T] <: T = T

object Nullable:

  implicit def apply[T](value: T): Nullable[T] = value

  def getOrDefault[T: Default](value: Nullable[T]): T = Default.orValue(value)

  given optionEncoder[T: Encoder]: Encoder[Nullable[Option[T]]] =
    Encoder {
      case (w, Some(x)) => w.write(x)
      case (w, None)    => w.writeNull()
    }

  given optionDecoder[T: Decoder]: Decoder[Nullable[Option[T]]] =
    Decoder(r => if (r.tryReadNull()) None else Some(r.read[T]()))

  given encoder[T: Encoder]: Encoder[Nullable[T]] =
    Encoder((w, x) => w.write(x))

  given decoder[T: Decoder: Default]: Decoder[Nullable[T]] =
    Decoder(r => if (r.tryReadNull()) Default.of[T] else r.read[T]())
