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

import scala.util.Try

object EncodingDecodingSpec extends TestSuite {

  val tests = Tests {

    "Encoding" - {

      "CBOR" - {
        //#encoding-cbor
        import io.bullet.borer.Cbor

        val value = List("foo", "bar", "baz") // example value

        val bytes: Array[Byte] =
          Cbor.encode(value).toByteArray // throws on error

        bytes ==> hex"9f63666f6f636261726362617aff"
        //#encoding-cbor
      }

      "JSON" - {
        //#encoding-json
        import io.bullet.borer.Json

        val value = List("foo", "bar", "baz") // example value

        val bytes: Array[Byte] =
          Json.encode(value).toByteArray // throws on error

        // or immediately decode the bytes into a String:

        val json: String =
          Json.encode(value).toUtf8String // throws on error

        json ==> """["foo","bar","baz"]"""
        bytes ==> json.getBytes("UTF8")
        //#encoding-json
      }
    }

    "Decoding" - {

      "CBOR" - {
        val bytes = hex"9f63666f6f636261726362617aff"

        //#decoding-cbor
        import io.bullet.borer.Cbor

        val list: List[String] =
          Cbor.decode(bytes).to[List[String]].value // throws on error
        //#decoding-cbor

        list ==> List("foo", "bar", "baz")
      }

      "JSON" - {
        val bytes = """["foo","bar","baz"]""" getBytes "UTF8"

        //#decoding-json
        import io.bullet.borer.Json

        val list: List[String] =
          Json.decode(bytes).to[List[String]].value // throws on error
        //#decoding-json

        list ==> List("foo", "bar", "baz")
      }
    }

    "Try" - {

      "CBOR" - {
        val value = List("foo", "bar", "baz")
        val bytes = hex"9f63666f6f636261726362617aff"

        //#try-cbor
        import io.bullet.borer.Cbor

        val encoded: Try[Array[Byte]] =
          Cbor.encode(value).toByteArrayTry

        val decoded: Try[List[String]] =
          Cbor.decode(bytes).to[List[String]].valueTry
        //#try-cbor

        encoded.get ==> bytes
        decoded.get ==> value
      }

      "JSON" - {
        val value = List("foo", "bar", "baz")
        val bytes = """["foo","bar","baz"]""" getBytes "UTF8"

        //#try-json
        import io.bullet.borer.Json

        val encoded: Try[Array[Byte]] =
          Json.encode(value).toByteArrayTry

        val decoded: Try[List[String]] =
          Json.decode(bytes).to[List[String]].valueTry
        //#try-json

        encoded.get ==> bytes
        decoded.get ==> value
      }
    }

    "Either" - {

      "CBOR" - {
        val value = List("foo", "bar", "baz")
        val bytes = hex"9f63666f6f636261726362617aff"

        //#either-cbor
        import io.bullet.borer._

        val encoded: Either[Borer.Error[Output], Array[Byte]] =
          Cbor.encode(value).to[Array[Byte]].resultEither

        val decoded: Either[Borer.Error[Input.Position], List[String]] =
          Cbor.decode(bytes).to[List[String]].valueEither
        //#either-cbor

        encoded.getOrElse(sys.error("fail")) ==> bytes
        decoded.getOrElse(sys.error("fail")) ==> value
      }

      "JSON" - {
        val value = List("foo", "bar", "baz")
        val bytes = """["foo","bar","baz"]""" getBytes "UTF8"

        //#either-json
        import io.bullet.borer._

        val encoded: Either[Borer.Error[Output], Array[Byte]] =
          Json.encode(value).to[Array[Byte]].resultEither

        val decoded: Either[Borer.Error[Input.Position], List[String]] =
          Json.decode(bytes).to[List[String]].valueEither
        //#either-json

        encoded.getOrElse(sys.error("fail")) ==> bytes
        decoded.getOrElse(sys.error("fail")) ==> value
      }
    }
  }
}
