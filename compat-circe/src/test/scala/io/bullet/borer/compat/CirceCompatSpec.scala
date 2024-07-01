/*
 * Copyright (c) 2019-2024 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.compat

import io.bullet.borer.{BorerSuite, Cbor, Json}
import io.circe.*
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}

class CirceCompatSpec extends BorerSuite {
  import circe.*

  final case class Foo(byte: Byte, short: Short, char: Char, int: Int, long: Long, list: List[String], bar: Bar)
  final case class Bar(boolean: Boolean, nll: Null, float: Float, double: Double, string: String, bd: BigDecimal)

  implicit val nullEncoder: Encoder[Null] =
    new Encoder[Null] {
      final def apply(x: Null) = io.circe.Json.Null
    }

  implicit val nullDecoder: Decoder[Null] =
    new Decoder[Null] {

      final def apply(c: HCursor): Decoder.Result[Null] =
        c.value match {
          case io.circe.Json.Null => Right(null)
          case _                  => Left(DecodingFailure("Null", c.history))
        }
    }

  implicit val fooDecoder: Decoder[Foo] = deriveDecoder[Foo]
  implicit val barDecoder: Decoder[Bar] = deriveDecoder[Bar]
  implicit val fooEncoder: Encoder[Foo] = deriveEncoder[Foo]
  implicit val barEncoder: Encoder[Bar] = deriveEncoder[Bar]

  val foo =
    Foo(
      byte = 1,
      short = -26,
      char = 'x',
      int = 1234567,
      long = 98765432123456789L,
      list = List("red", "green", "blue"),
      bar = Bar(
        boolean = true,
        nll = null,
        float = 1.25f,
        double = -0.12345,
        string = "borer rocks!",
        bd = BigDecimal("1.23456789"))
    )

  test("JSON round trip") {
    val encoding =
      """{"byte":1,"short":-26,"char":"x","int":1234567,"long":98765432123456789,"list":["red","green","blue"],"bar":{"boolean":true,"nll":null,"float":1.25,"double":-0.12345,"string":"borer rocks!","bd":1.23456789}}"""

    Json.encode(foo).toUtf8String ==> encoding
    Json.decode(encoding.getBytes("UTF8")).to[Foo].value ==> foo
  }

  test("CBOR round trip") {
    val encoding = Cbor.encode(foo).toByteArray
    Cbor.decode(encoding).to[Foo].value ==> foo
  }
}
