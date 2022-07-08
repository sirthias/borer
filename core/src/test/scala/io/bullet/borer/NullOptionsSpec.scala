/*
 * Copyright (c) 2019-2022 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

class NullOptionsSpec extends ByteArrayJsonSpec:

  case class Foo(int: Int, string: Option[String])

  implicit val fooCodec: Codec[Foo] =
    import NullOptions._
    Codec.forProduct[Foo]

  test("NullOptions") {
    roundTrip("""[12,null]""", Foo(12, None))
    roundTrip("""[12,"foo"]""", Foo(12, Some("foo")))
  }
