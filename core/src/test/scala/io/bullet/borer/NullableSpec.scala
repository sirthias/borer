/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import utest._

object NullableSpec extends AbstractJsonSpec {

  case class Foo(int: Nullable[Int], string: Nullable[String])

  // we cannot use `Codec.deriveForCaseClass` since we are in the same compilation module
  implicit val fooCodec = Codec(Encoder.from(Foo.unapply _), Decoder.from(Foo.apply _))

  val tests = Tests {

    "test" - {
      roundTrip("""[12,"foo"]""", Foo(12, "foo"))

      verifyDecoding("""[null,null]""", Foo(0, ""))
    }

  }
}
