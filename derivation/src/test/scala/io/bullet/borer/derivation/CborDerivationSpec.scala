/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.derivation

import io.bullet.borer.{Cbor, Dom}

object CborDerivationSpec extends DerivationSpec(Cbor) {
  import Dom._

  def arrayBasedFooDom =
    ArrayElem.Sized(
      IntElem(120),
      IntElem(66),
      IntElem(-10000),
      IntElem(1234567),
      IntElem(-1),
      Float16Elem(1.5f),
      DoubleElem(26.8),
      StringElem("borer"),
      ArrayElem.Sized(),
      ArrayElem.Unsized(
        ArrayElem.Sized(IntElem(255), IntElem(0), IntElem(0), IntElem(255)),
        ArrayElem.Sized(IntElem(0), IntElem(255), IntElem(0), IntElem(255)),
        ArrayElem.Sized(IntElem(0), IntElem(0), IntElem(255), IntElem(255))
      ))

  def arrayBasedMissingElemErrorMsg =
    "Expected Array Start or Array Header(10) for decoding an instance of type " +
      "[io.bullet.borer.derivation.DerivationSpec.Foo] but got Array Header (9) [input position 0]"

  def mapBasedFooDom =
    MapElem.Sized(
      "char"   → IntElem(120),
      "byte"   → IntElem(66),
      "short"  → IntElem(-10000),
      "int"    → IntElem(1234567),
      "long"   → IntElem(-1),
      "float"  → Float16Elem(1.5f),
      "double" → DoubleElem(26.8),
      "string" → StringElem("borer"),
      "empty"  → MapElem.Sized(),
      "colors" → ArrayElem.Unsized(
        MapElem.Sized("red" → IntElem(255), "green" → IntElem(0), "blue"   → IntElem(0), "alpha"   → IntElem(255)),
        MapElem.Sized("red" → IntElem(0), "green"   → IntElem(255), "blue" → IntElem(0), "alpha"   → IntElem(255)),
        MapElem.Sized("red" → IntElem(0), "green"   → IntElem(0), "blue"   → IntElem(255), "alpha" → IntElem(255))
      ))

  def animalsDom =
    ArrayElem.Unsized(
      ArrayElem.Sized(StringElem("Dog"), ArrayElem.Sized(IntElem(12), StringElem("Fred"))),
      ArrayElem
        .Sized(StringElem("TheCAT"), ArrayElem.Sized(Float16Elem(1.0f), StringElem("none"), StringElem("there"))),
      ArrayElem.Sized(StringElem("Dog"), ArrayElem.Sized(IntElem(4), StringElem("Lolle"))),
      ArrayElem.Sized(IntElem(42), BoolElem.True))
}
