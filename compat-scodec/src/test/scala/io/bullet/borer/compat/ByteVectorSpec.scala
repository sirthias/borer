/*
 * Copyright (c) 2019-2023 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.compat

import _root_.scodec.bits.ByteVector
import io.bullet.borer._
import io.bullet.borer.derivation.ArrayBasedCodecs

class ByteVectorSpec extends AbstractBorerSpec {
  import scodec._

  def encode[T: Encoder](value: T): String   = toHexString(Cbor.encode(value).to[ByteVector].result.toArray)
  def decode[T: Decoder](encoded: String): T = Cbor.decode(ByteVector(hexBytes(encoded))).to[T].value

  case class Foo(int: Int, content: ByteVector)

  implicit val fooCodec: Codec[Foo] = ArrayBasedCodecs.deriveCodec[Foo]

  test("basic roundtrip") {
    roundTrip(
      "83820b40820c476f682079656168820d43ff0001",
      Vector(
        Foo(11, ByteVector.empty),
        Foo(12, ByteVector("oh yeah" getBytes "UTF8")),
        Foo(13, ByteVector(-1, 0, 1))
      )
    )
  }
}
