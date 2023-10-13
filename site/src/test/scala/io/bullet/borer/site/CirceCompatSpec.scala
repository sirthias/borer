/*
 * Copyright (c) 2019-2023 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.site

import io.bullet.borer.internal.Util.*
import io.bullet.borer.BorerSuite

class CirceCompatSpec extends BorerSuite {

  test("Example") {
    // #example
    import io.circe.{Decoder, Encoder} // NOTE: circe (!) Encoders / Decoders
    import io.bullet.borer.Cbor
    import io.bullet.borer.compat.circe.* // the borer codec for the circe AST

    // serializes a value to CBOR given that a circe `Encoder` is available
    def serializeToCbor[T: Encoder](value: T): Array[Byte] =
      Cbor.encode(value).toByteArray

    // serializes a value from CBOR given that a circe `Decoder` is available
    def deserializeFromCbor[T: Decoder](bytes: Array[Byte]): T =
      Cbor.decode(bytes).to[T].value

    val value = List("foo", "bar", "baz") // example value

    val bytes = serializeToCbor(value)
    bytes ==> hex"8363666f6f636261726362617a"

    deserializeFromCbor[List[String]](bytes) ==> value
    // #example
  }
}
