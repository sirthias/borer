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

class OutputExamplesSpec extends BorerSuite {

  test("Examples") {

    val filename = java.io.File.createTempFile("borer-OutputExamplesSpec", ".bin").getAbsolutePath

    // #examples
    import io.bullet.borer.Cbor

    val value = List("foo", "bar", "baz") // example value

    // encodes into a new byte array instance,
    // relies on the `Output.ToTypeProvider[Array[Byte]]` type class instance
    Cbor.encode(value).to[Array[Byte]].result ==> hex"9f63666f6f636261726362617aff"

    // same as above but slightly more convenient
    Cbor.encode(value).toByteArray ==> hex"9f63666f6f636261726362617aff"

    // encodes into an existing file,
    // relies on the `Output.ToValueProvider[File]` type class instance
    val file = new java.io.File(filename)
    Cbor.encode(value).to(file).result ==> file
    // #examples

    file.delete()
  }
}
