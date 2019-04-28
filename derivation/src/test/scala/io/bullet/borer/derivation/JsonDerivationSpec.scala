/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.derivation

import io.bullet.borer.{Dom, Json}

object JsonDerivationSpec extends DerivationSpec(Json) {
  import Dom._

  def arrayBasedFooDom =
    ArrayElem.Unsized(
      IntElem(120),
      IntElem(66),
      IntElem(-10000),
      IntElem(1234567),
      IntElem(-1),
      DoubleElem(1.5f),
      DoubleElem(26.8),
      StringElem("borer"),
      ArrayElem.Unsized(),
      ArrayElem.Unsized(
        ArrayElem.Unsized(IntElem(255), IntElem(0), IntElem(0), IntElem(255)),
        ArrayElem.Unsized(IntElem(0), IntElem(255), IntElem(0), IntElem(255)),
        ArrayElem.Unsized(IntElem(0), IntElem(0), IntElem(255), IntElem(255))
      ))

  def arrayBasedMissingElemErrorMsg = "Cannot convert int value -10000 to Byte [input position 18]"

  def mapBasedFooDom =
    MapElem.Unsized(
      "char"   → IntElem(120),
      "byte"   → IntElem(66),
      "short"  → IntElem(-10000),
      "int"    → IntElem(1234567),
      "long"   → IntElem(-1),
      "float"  → DoubleElem(1.5f),
      "double" → DoubleElem(26.8),
      "string" → StringElem("borer"),
      "empty"  → MapElem.Unsized(),
      "colors" → ArrayElem.Unsized(
        MapElem.Unsized("red" → IntElem(255), "green" → IntElem(0), "blue"   → IntElem(0), "alpha"   → IntElem(255)),
        MapElem.Unsized("red" → IntElem(0), "green"   → IntElem(255), "blue" → IntElem(0), "alpha"   → IntElem(255)),
        MapElem.Unsized("red" → IntElem(0), "green"   → IntElem(0), "blue"   → IntElem(255), "alpha" → IntElem(255))
      ))

  def animalsDom =
    ArrayElem.Unsized(
      ArrayElem.Unsized(StringElem("Dog"), ArrayElem.Unsized(IntElem(12), StringElem("Fred"))),
      ArrayElem
        .Unsized(StringElem("TheCAT"), ArrayElem.Unsized(DoubleElem(1.0f), StringElem("none"), StringElem("there"))),
      ArrayElem.Unsized(StringElem("Dog"), ArrayElem.Unsized(IntElem(4), StringElem("Lolle"))),
      ArrayElem.Unsized(IntElem(42), BoolElem.True))
}
