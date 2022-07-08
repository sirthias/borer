/*
 * Copyright (c) 2019-2022 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.compat

import _root_.cats.data._
import _root_.cats.instances.string._
import _root_.cats.instances.int._
import io.bullet.borer._
import utest._

object CatsCompatJsonSpec extends ByteArrayJsonSpec {
  import cats._

  val tests = Tests {

    "Chain" - roundTrip("[1,2,3]", Chain(1, 2, 3))

    "Ior" - roundTrip("""[[0,1],[1,"foo"],[2,42,"bar"]]""", List(Ior.Left(1), Ior.Right("foo"), Ior.Both(42, "bar")))

    "NonEmptyChain" - roundTrip("[1,2,3]", NonEmptyChain(1, 2, 3))

    "NonEmptyList" - roundTrip("[1,2,3]", NonEmptyList.of(1, 2, 3))

    "NonEmptyMap" - roundTrip(
      """{"blue":100000,"green":1234,"red":18}""",
      NonEmptyMap.of("red" -> 18, "green" -> 1234, "blue" -> 100000))

    "NonEmptySet" - roundTrip("[1,2,3]", NonEmptySet.of(1, 2, 3))

    "NonEmptyVector" - roundTrip("[1,2,3]", NonEmptyVector.of(1, 2, 3))

    "Validated" - roundTrip("[[0,\"no\"],[1,42]]", List(Validated.Invalid("no"), Validated.Valid(42)))
  }
}
