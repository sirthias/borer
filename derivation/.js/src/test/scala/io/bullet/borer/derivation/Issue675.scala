/*
 * Copyright (c) 2019-2024 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.derivation

import io.bullet.borer.*
import io.bullet.borer.derivation.MapBasedCodecs.*

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import java.nio.{ByteBuffer, ByteOrder}

case class Foo(id: String) derives Decoder

class Issue675 extends BorerSuite:

  // given Decoder[Foo] = deriveDecoder

  val str          = """{"id": "foo"}"""
  val strByteArray = str.getBytes

  test("A") {
    val byteBuffer = ByteBuffer.wrap(strByteArray)
    Json.decode(byteBuffer).to[Foo].value
  }

  test("B") {
    val x         = strByteArray.toJSArray
    val int8Array = new js.typedarray.Int8Array(x)
    val buffer    = js.typedarray.TypedArrayBuffer.wrap(int8Array)
    buffer.order(ByteOrder.BIG_ENDIAN)
    Json.decode(buffer).to[Foo].value
  }
