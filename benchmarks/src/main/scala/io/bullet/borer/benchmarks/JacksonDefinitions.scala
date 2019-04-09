/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.benchmarks

import com.fasterxml.jackson.core.`type`.TypeReference
import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import org.openjdk.jmh.annotations._

object JacksonCodecs {
  val mapper: ObjectMapper = new ObjectMapper()
  mapper.registerModule(DefaultScalaModule)

  val foosTypeRef = new TypeReference[Map[String, Foo]] {}
  val intsTypeRef = new TypeReference[List[Int]]        {}
}

import io.bullet.borer.benchmarks.JacksonCodecs._

class JacksonEncodingBenchmark extends EncodingBenchmark {

  @Benchmark
  def encodeFoos: Array[Byte] = mapper.writeValueAsBytes(foos)

  @Benchmark
  def encodeInts: Array[Byte] = mapper.writeValueAsBytes(ints)

  @Benchmark
  def encodeEmptyArray: Array[Byte] = mapper.writeValueAsBytes(List.empty[Int])
}

class JacksonDecodingBenchmark extends DecodingBenchmark {

  @Benchmark
  def decodeFoos: Map[String, Foo] = mapper.readValue(foosJson, foosTypeRef)

  @Benchmark
  def decodeInts: List[Int] = mapper.readValue(intsJson, intsTypeRef)

  @Benchmark
  def decodeEmptyArray: List[Int] = mapper.readValue(emptyArrayJson, intsTypeRef)
}

class JacksonDomBenchmark extends DomBenchmark {

  private var root: JsonNode = _
  def setup(): Unit          = root = mapper.readTree(fileBytes)

  @Benchmark
  def encodeDom: Array[Byte] = mapper.writeValueAsBytes(root)

  @Benchmark
  def decodeDom: JsonNode = mapper.readTree(fileBytes)
}
