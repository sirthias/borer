/*
 * Copyright (c) 2019-2021 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.site

import utest._

object NullableSpec extends TestSuite {

  val tests = Tests {

    "Example" - {
      // #example
      import io.bullet.borer.{Json, Nullable}
      import io.bullet.borer.derivation.MapBasedCodecs._

      case class Dog(age: Int, name: Nullable[String])

      implicit val dogCodec = deriveCodec[Dog]

      Json
        .decode("""{ "age": 4, "name": null }""" getBytes "UTF8")
        .to[Dog]
        .value ==>
      Dog(age = 4, name = "") // the `Default[String]` provides an empty String
      // #example
    }
  }
}
