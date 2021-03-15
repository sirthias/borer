/*
 * Copyright (c) 2019-2021 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.benchmarks

import com.fasterxml.jackson.core.`type`.TypeReference
import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}
import com.fasterxml.jackson.module.afterburner.AfterburnerModule
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import org.openjdk.jmh.annotations._

object JacksonCodecs {
  val mapper: ObjectMapper = new ObjectMapper().registerModule(DefaultScalaModule).registerModule(new AfterburnerModule)

  val foosTypeRef = new TypeReference[Map[String, Foo]] {}
  val intsTypeRef = new TypeReference[List[Int]] {}
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

class JacksonModelBenchmark extends DomBenchmark {

  private var root: Product = _

  lazy val typeRef: TypeReference[Product] = {
    val c = fileName match {
      case "australia-abc.json"      => new TypeReference[Australia.RootInterface] {}
      case "bitcoin.json"            => new TypeReference[Bitcoin.RootInterface] {}
      case "doj-blog.json"           => new TypeReference[DojBlog.RootInterface] {}
      case "eu-lobby-country.json"   => new TypeReference[EuLobbyCountry.RootInterface] {}
      case "eu-lobby-financial.json" => new TypeReference[EuLobbyFinancial.RootInterface] {}
      case "eu-lobby-repr.json"      => new TypeReference[EuLobbyRepr.RootInterface] {}
      case "github-gists.json"       => new TypeReference[List[GithubGists.RootInterface]] {}
      case "json-generator.json"     => new TypeReference[List[JsonGenerator.RootInterface]] {}
      case "meteorites.json"         => new TypeReference[List[Meteorites.RootInterface]] {}
      case "movies.json"             => new TypeReference[List[Movies.RootInterface]] {}
      case "reddit-scala.json"       => new TypeReference[Reddit.RootInterface[Reddit.Data]] {}
      case "rick-morty.json"         => new TypeReference[RickMorty.RootInterface] {}
      case "temp-anomaly.json"       => new TypeReference[TempAnomaly.RootInterface] {}
      case "thai-cinemas.json"       => new TypeReference[ThaiCinemas.RootInterface] {}
      case "turkish.json"            => new TypeReference[Turkish.RootInterface] {}

      case "github-events.json" =>
        new TypeReference[
          List[GithubEvents.RootInterface[GithubEvents.Head[GithubEvents.Repo1], GithubEvents.Forkee]]] {}

      case "twitter_api_compact_response.json" | "twitter_api_response.json" =>
        new TypeReference[List[TwitterApiResponse.RootInterface]] {}
    }
    c.asInstanceOf[TypeReference[Product]]
  }

  def setup(): Unit =
    root =
      try mapper.readValue(fileBytes, typeRef)
      finally ()

  @Benchmark
  def encodeModel: Array[Byte] = mapper.writeValueAsBytes(root)

  @Benchmark
  def decodeModel: Product = mapper.readValue(fileBytes, typeRef)
}
