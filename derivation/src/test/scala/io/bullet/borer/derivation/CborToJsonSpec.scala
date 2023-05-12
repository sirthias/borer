/*
 * Copyright (c) 2019-2023 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.derivation

import io.bullet.borer._
import io.bullet.borer.Dom.Transformer

class CborToJsonSpec extends BorerSuite {

  case class Foo(x: Int, y: Float16, z: Vector[String])

  given Codec[Foo] = MapBasedCodecs.deriveCodec[Foo]

  test("CBOR to JSON via DOM") {
    val value = List(Foo(18, Float16(2.0f), Vector("foo")), Foo(21, Float16(5.0f), Vector("bar")))

    val bytes          = Cbor.encode(value).toByteArray
    val dom            = Cbor.decode(bytes).to[Dom.Element].value
    val transformer    = new Transformer.ToJsonSubset {}
    val transformedDom = transformer(dom)
    Json.encode(transformedDom).toUtf8String ==> """[{"x":18,"y":2.0,"z":["foo"]},{"x":21,"y":5.0,"z":["bar"]}]"""
  }
}
