/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import java.nio.charset.StandardCharsets.UTF_8

import utest._

object NullableSpec extends AbstractBorerSpec {

  def encode[T: Encoder](value: T): String = Json.encode(value).toUtf8String

  def decode[T: Decoder](encoded: String): T = Json.decode(encoded getBytes UTF_8).to[T].value

  case class Foo(int: Nullable[Int], string: Nullable[String])
  case class Bar(foo: Nullable[Option[Foo]])

  implicit val fooCodec = Codec(Encoder.from(Foo.unapply _), Decoder.from(Foo.apply _))
  implicit val barCodec = Codec(Encoder.from(Bar.unapply _), Decoder.from(Bar.apply _))

  val tests = Tests {

    "predefined default values" - {
      roundTrip("""[12,"foo"]""", Foo(12, "foo"))
      verifyDecoding("""[null,null]""", Foo(0, ""))
    }

    "options" - {
      roundTrip("""null""", Bar(None))
      roundTrip("""[12,"foo"]""", Bar(Some(Foo(12, "foo"))))
    }
  }
}
