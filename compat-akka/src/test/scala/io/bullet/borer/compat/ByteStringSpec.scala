/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.compat

import _root_.akka.util.ByteString
import io.bullet.borer._
import io.bullet.borer.derivation.ArrayBasedCodecs
import utest._

object ByteStringSpec extends AbstractBorerSpec {
  import akka._

  def encode[T: Encoder](value: T): String   = toHexString(Cbor.encode(value).to[ByteString].result.toArray)
  def decode[T: Decoder](encoded: String): T = Cbor.decode(ByteString(hexBytes(encoded))).to[T].value

  case class Foo(int: Int, content: ByteString)

  implicit val fooCodec = ArrayBasedCodecs.deriveCodec[Foo]

  val tests = Tests {

    "basic roundtrip" - roundTrip(
      "83820b40820c476f682079656168820d43ff0001",
      Vector(
        Foo(11, ByteString.empty),
        Foo(12, ByteString("oh yeah")),
        Foo(13, ByteString(-1, 0, 1))
      )
    )
  }
}
