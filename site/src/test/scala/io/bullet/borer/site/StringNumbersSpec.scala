/*
 * Copyright (c) 2019-2023 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.site

import io.bullet.borer.BorerSuite

class StringNumbersSpec extends BorerSuite {

  test("Imports") {
    // #imports
    import io.bullet.borer.{Decoder, Encoder}

    import Encoder.StringNumbers.given  // enables number-as-strings encoding
    import Encoder.StringBooleans.given // enables booleans-as-strings encoding
    import Encoder.StringNulls.given    // enables null-as-strings encoding

    import Decoder.StringNumbers.given  // enables number-as-strings decoding
    import Decoder.StringBooleans.given // enables booleans-as-strings decoding
    import Decoder.StringNulls.given    // enables null-as-strings decoding
    // #imports

    intEncoder.hashCode() ^
    intDecoder.hashCode() ^
    booleanEncoder.hashCode() ^
    booleanDecoder.hashCode() ^
    nullEncoder.hashCode() ^
    nullDecoder.hashCode()
  }

  test("Example") {
    // #example
    import io.bullet.borer.{Json, Encoder, Codec}
    import io.bullet.borer.derivation.MapBasedCodecs.*

    import Encoder.StringNumbers.given  // enables number-as-strings encoding
    import Encoder.StringBooleans.given // enables booleans-as-strings encoding

    case class Dog(age: Int, male: Boolean, name: String) derives Codec

    val dog = Dog(2, false, "Lolle")

    // note how the Int and Boolean are rendered as JSON strings
    Json.encode(dog).toUtf8String ==>
    """{"age":"2","male":"false","name":"Lolle"}"""
    // #example
  }
}
