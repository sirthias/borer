/*
 * Copyright (c) 2019-2021 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

final case class Default[+T](defaultValue: T)

object Default {
  implicit val boolean: Default[Boolean] = Default(false)
  implicit val byte: Default[Byte]       = Default(0: Byte)
  implicit val short: Default[Short]     = Default(0: Short)
  implicit val int: Default[Int]         = Default(0)
  implicit val long: Default[Long]       = Default(0L)
  implicit val string: Default[String]   = Default("")
  implicit val float: Default[Float]     = Default(0.0f)
  implicit val double: Default[Double]   = Default(0.0)

  @inline def of[T](implicit d: Default[T]): T = d.defaultValue

  @inline def orValue[T: Default](value: T): T = if (value == null) Default.of[T] else value

  private[this] val optionDefault            = Default(None)
  implicit def option[T]: Default[Option[T]] = optionDefault
}

final case class Nullable[+T](value: T) { // extends AnyVal // FIXME: re-enable value class after fix of https://github.com/lampepfl/dotty/issues/11264

  // for efficient unapply
  def isEmpty = false
  def get: T  = value

  override def toString = s"Nullable($value)"
}

object Nullable extends LowPrioNullable {

  implicit def apply[T](value: T): Nullable[T]    = new Nullable(value)
  def unapply[T](value: Nullable[T]): Nullable[T] = value

  @inline def getOrDefault[T: Default](nullable: Nullable[T]): T = Default.orValue(nullable.value)

  implicit def optionEncoder[T: Encoder]: Encoder[Nullable[Option[T]]] =
    Encoder { (w, nullable) =>
      nullable.value match {
        case Some(x) => w.write(x)
        case None    => w.writeNull()
      }
    }

  implicit def optionDecoder[T: Decoder]: Decoder[Nullable[Option[T]]] =
    Decoder(r => new Nullable(if (r.tryReadNull()) None else Some(r.read[T]())))
}

sealed abstract class LowPrioNullable {

  implicit def encoder[T: Encoder]: Encoder[Nullable[T]] =
    Encoder((w, x) => w.write(x.value))

  implicit def decoder[T: Decoder: Default]: Decoder[Nullable[T]] =
    Decoder(r => new Nullable(if (r.tryReadNull()) Default.of[T] else r.read[T]()))
}
