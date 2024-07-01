/*
 * Copyright (c) 2019-2024 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

class TranscodingSpec extends BorerSuite:

  case class Foo(int: Int, string: String, doubleOpt: Option[java.lang.Double])
  case class Bar(foo: Foo, optFoo: Option[Foo], stringSeq: Seq[String])

  // derivation makes this easy but we don't want to depend on it here
  implicit val fooCodec: Codec[Foo] = Codec.forProduct[Foo]
  implicit val barCodec: Codec[Bar] = Codec.forProduct[Bar]

  import Dom._

  test("To and From Dom") {
    val foo = Foo(42, "yeah", Some(1.8))
    val dom = ArrayElem.Sized(IntElem(42), StringElem("yeah"), ArrayElem.Sized(DoubleElem(1.8)))
    Cbor.transEncode(foo).transDecode.to[Dom.Element].value ==> dom
    Cbor.transEncode(dom).transDecode.to[Foo].value ==> foo
  }

  test("Roundtrip") {
    val bar = Bar(Foo(42, "yeah", Some(1.8)), None, List("abc", "", "z"))
    Cbor.transEncode(bar).transDecode.to[Bar].value ==> bar
  }

  test("Roundtrip") {
    intercept[Borer.Error.InvalidInputData[_]](
      Cbor.transEncode(Map(1 -> "yes")).transDecode.to[Map[String, String]].value
    ).getMessage.takeWhile(_ != '(') ==> "Expected String or Text-Bytes but got Int "
  }
