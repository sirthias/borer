/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.benchmarks

import org.openjdk.jmh.annotations._
import spray.json._
import java.nio.charset.StandardCharsets.UTF_8

object SprayCodecs {
  import spray.json.DefaultJsonProtocol._

  implicit val fooFormat  = jsonFormat5(Foo.apply)
  implicit val foosFormat = implicitly[RootJsonFormat[Map[String, Foo]]]
  implicit val intsFormat = implicitly[RootJsonFormat[List[Int]]]
}

import SprayCodecs._

class SprayEncodingBenchmark extends EncodingBenchmark {

  @Benchmark
  def encodeFoos: Array[Byte] = foosFormat.write(foos).compactPrint.getBytes(UTF_8)

  @Benchmark
  def encodeInts: Array[Byte] = intsFormat.write(ints).compactPrint.getBytes(UTF_8)

  @Benchmark
  def encodeEmptyArray: Array[Byte] = intsFormat.write(Nil).compactPrint.getBytes(UTF_8)
}

class SprayDecodingBenchmark extends DecodingBenchmark {

  @Benchmark
  def decodeFoos: Map[String, Foo] = JsonParser(new String(foosJson, UTF_8)).convertTo[Map[String, Foo]]

  @Benchmark
  def decodeInts: List[Int] = JsonParser(new String(intsJson, UTF_8)).convertTo[List[Int]]

  @Benchmark
  def decodeEmptyArray: List[Int] = JsonParser(new String(emptyArrayJson, UTF_8)).convertTo[List[Int]]
}

class SprayDomBenchmark extends DomBenchmark {

  private var root: JsValue = _
  def setup(): Unit         = root = JsonParser(new String(fileBytes, UTF_8))

  @Benchmark
  def encodeDom: Array[Byte] = root.compactPrint.getBytes(UTF_8)

  @Benchmark
  def decodeDom: JsValue = JsonParser(new String(fileBytes, UTF_8))
}
