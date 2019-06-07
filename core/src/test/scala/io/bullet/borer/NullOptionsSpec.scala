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

object NullOptionsSpec extends AbstractBorerSpec {

  def encode[T: Encoder](value: T): String = Json.encode(value).toUtf8String

  def decode[T: Decoder](encoded: String): T = Json.decode(encoded getBytes UTF_8).to[T].value

  case class Foo(int: Int, string: Option[String])

  implicit val fooCodec = {
    import NullOptions._

    Codec.forCaseClass[Foo]
  }

  val tests = Tests {

    "NullOptions" - {
      roundTrip("""[12,null]""", Foo(12, None))
      roundTrip("""[12,"foo"]""", Foo(12, Some("foo")))
    }
  }
}
