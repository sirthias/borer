/*
 * Copyright (c) 2019-2024 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.derivation

// compile-time only
object Issue685:
  import io.bullet.borer.*
  import io.bullet.borer.derivation.MapBasedCodecs.*

  sealed trait Bar:
    def toEither: Either[Int, String] =
      this match
        case Bar.IntBar(int) => Left(int)
        case Bar.StrBar(str) => Right(str)

  object Bar:
    case class IntBar(int: Int)    extends Bar
    case class StrBar(str: String) extends Bar
    given Encoder[Bar] = Encoder.ForEither.default[Int, String].contramap[Bar](_.toEither)

  sealed trait Foo
  object Foo:
    case class Foo1[B <: Bar](b1: B) extends Foo
    case class Foo2[B <: Bar](b2: B) extends Foo

    given Encoder[Foo] = {
      given Encoder[Foo1[Bar]] = deriveEncoder
      given Encoder[Foo2[Bar]] = deriveEncoder
      deriveEncoder
    }
