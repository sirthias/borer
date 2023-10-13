/*
 * Copyright (c) 2019-2023 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.site

import io.bullet.borer.BorerSuite

class NullableSpec extends BorerSuite {

  test("Example") {
    // #example
    import io.bullet.borer.{Json, Codec, Nullable}
    import io.bullet.borer.derivation.MapBasedCodecs.*

    case class Dog(age: Int, name: Nullable[String]) derives Codec

    Json
      .decode("""{ "age": 4, "name": null }""" getBytes "UTF8")
      .to[Dog]
      .value ==>
    Dog(age = 4, name = "") // the `Default[String]` provides an empty String
    // #example
  }
}
