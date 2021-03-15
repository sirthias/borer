/*
 * Copyright (c) 2019-2021 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.site

import io.bullet.borer.internal.Util._
import utest._

object JsonSpecificsSpec extends TestSuite {

  val tests = Tests {

    "JSON specifics" - {

      "writeEmptyArray" - {
        //#writeEmptyArray
        import io.bullet.borer.Writer

        def writeEmptyArray(w: Writer): w.type =
          if (w.writingJson) w.writeArrayStart().writeBreak()
          else w.writeArrayHeader(0) // fixed-sized Arrays are not supported in JSON
        //#writeEmptyArray

        import io.bullet.borer._

        object Foo

        implicit val fooEncoder: Encoder[Foo.type] =
          Encoder((w, _) => writeEmptyArray(w))

        Cbor.encode(Foo).toByteArray ==> hex"80"
        Json.encode(Foo).toUtf8String ==> "[]"
      }

      "writeArrayOpen-close" - {
        //#writeArrayOpen-close
        import io.bullet.borer.Writer

        def writeAsUnaryArray(w: Writer, s: String): w.type =
          w.writeArrayOpen(1) // automatically chooses the most efficient
            .writeString(s)
            .writeArrayClose() // way to write an array of size one
        //#writeArrayOpen-close

        import io.bullet.borer._

        object Foo

        implicit val fooEncoder: Encoder[Foo.type] =
          Encoder((w, _) => writeAsUnaryArray(w, "foo"))

        Cbor.encode(Foo).toByteArray ==> hex"8163666F6F"
        Json.encode(Foo).toUtf8String ==> """["foo"]"""
      }

      "alternative base encoding" - {
        //#alternative-base-encoding
        import io.bullet.borer.{Decoder, Encoder, Json}
        import io.bullet.borer.encodings.BaseEncoding

        val binaryData = hex"DEADBEEF"

        // Json.encode(binaryData).toByteArray or
        Json.encode(binaryData).toUtf8String ==> """"3q2+7w==""""

        // we need to explicitly define the encoder as well as the decoder
        // in order to "override" the defaults for Array[Byte] on either side
        implicit val byteArrayEncoder = Encoder.forByteArray(BaseEncoding.zbase32)
        implicit val byteArrayDecoder = Decoder.forByteArray(BaseEncoding.zbase32)

        Json.encode(binaryData).toUtf8String ==> """"54s575a""""
        //#alternative-base-encoding
      }
    }
  }
}
