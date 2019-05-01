/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.benchmarks

import java.nio.charset.StandardCharsets.UTF_8

import io.bullet.borer.{Default, Nullable}
import io.circe._
import io.circe.derivation.{deriveDecoder, deriveEncoder}
import io.circe.jawn.{decode, parse}
import org.openjdk.jmh.annotations._

object CirceCodecs {
  implicit val fooEncoder: Encoder[Foo] = deriveEncoder
  implicit val fooDecoder: Decoder[Foo] = deriveDecoder

  implicit val foosEncoder = implicitly[Encoder[Map[String, Foo]]]
  implicit val foosDecoder = implicitly[Decoder[Map[String, Foo]]]

  implicit val intsEncoder = implicitly[Encoder[List[Int]]]
  implicit val intsDecoder = implicitly[Decoder[List[Int]]]
}

import io.bullet.borer.benchmarks.CirceCodecs._

class CirceEncodingBenchmark extends EncodingBenchmark {

  @Benchmark
  def encodeFoos: Array[Byte] = foosEncoder(foos).noSpaces.getBytes(UTF_8)

  @Benchmark
  def encodeInts: Array[Byte] = intsEncoder(ints).noSpaces.getBytes(UTF_8)

  @Benchmark
  def encodeEmptyArray: Array[Byte] = intsEncoder(Nil).noSpaces.getBytes(UTF_8)
}

class CirceDecodingBenchmark extends DecodingBenchmark {

  @Benchmark
  def decodeFoos: Map[String, Foo] = decode[Map[String, Foo]](new String(foosJson, UTF_8)).right.get

  @Benchmark
  def decodeInts: List[Int] = decode[List[Int]](new String(intsJson, UTF_8)).right.get

  @Benchmark
  def decodeEmptyArray: List[Int] = decode[List[Int]](new String(emptyArrayJson, UTF_8)).right.get
}

class CirceDomBenchmark extends DomBenchmark {

  private var root: Json = _
  def setup(): Unit      = root = parse(new String(fileBytes, UTF_8)).right.get

  @Benchmark
  def encodeDom: Array[Byte] = root.noSpaces.getBytes(UTF_8)

  @Benchmark
  def decodeDom: Json =
    // first decoding into a string and then parsing appears to be faster than directly parsing from the bytes
    parse(new String(fileBytes, UTF_8)).right.get
}

class CirceModelBenchmark extends DomBenchmark {

  private var root: Product = _

  implicit lazy val encoder: Encoder[Product] = {

    implicit def nullableEncoder[T](implicit enc: Encoder[T]): Encoder[Nullable[T]] = enc.contramap(_.value)

    val c = fileName match {
      case "australia-abc.json" ⇒
        implicit val a = deriveEncoder[Australia.Geometry]
        implicit val b = deriveEncoder[Australia.Properties]
        implicit val c = deriveEncoder[Australia.Properties1]
        implicit val d = deriveEncoder[Australia.Features]
        implicit val e = deriveEncoder[Australia.Crs]
        deriveEncoder[Australia.RootInterface]

      case "bitcoin.json" ⇒
        implicit val a = deriveEncoder[Bitcoin.SpendingOutpoints]
        implicit val b = deriveEncoder[Bitcoin.PrevOut]
        implicit val c = deriveEncoder[Bitcoin.Inputs]
        implicit val d = deriveEncoder[Bitcoin.Out]
        implicit val e = deriveEncoder[Bitcoin.Txs]
        deriveEncoder[Bitcoin.RootInterface]

      case "doj-blog.json" ⇒
        implicit val a = deriveEncoder[DojBlog.ResponseInfo]
        implicit val b = deriveEncoder[DojBlog.Resultset]
        implicit val c = deriveEncoder[DojBlog.Metadata]
        implicit val d = deriveEncoder[DojBlog.Component]
        implicit val e = deriveEncoder[DojBlog.Results]
        deriveEncoder[DojBlog.RootInterface]

      case "eu-lobby-country.json" ⇒
        implicit val a = deriveEncoder[EuLobbyCountry.Facets]
        implicit val b = deriveEncoder[EuLobbyCountry.Results]
        deriveEncoder[EuLobbyCountry.RootInterface]

      case "eu-lobby-financial.json" ⇒
        implicit val a = deriveEncoder[EuLobbyFinancial.Facets]
        implicit val b = deriveEncoder[EuLobbyFinancial.CustomIncomes]
        implicit val c = deriveEncoder[EuLobbyFinancial.Results]
        deriveEncoder[EuLobbyFinancial.RootInterface]

      case "eu-lobby-repr.json" ⇒
        implicit val a = deriveEncoder[EuLobbyRepr.Facets]
        implicit val b = deriveEncoder[EuLobbyRepr.Results]
        deriveEncoder[EuLobbyRepr.RootInterface]

      case "github-events.json" ⇒
        implicit val a = deriveEncoder[GithubEvents.Actor]
        implicit val b = deriveEncoder[GithubEvents.Author]
        implicit val c = deriveEncoder[GithubEvents.Self]
        implicit val d = deriveEncoder[GithubEvents.Owner]
        implicit val e = deriveEncoder[GithubEvents.Org]
        implicit val f = deriveEncoder[GithubEvents.Links]
        implicit val g = deriveEncoder[GithubEvents.Links1]
        implicit val h = deriveEncoder[GithubEvents.Labels]
        implicit val i = deriveEncoder[GithubEvents.License]
        implicit val j = deriveEncoder[GithubEvents.Repo]
        implicit val k = deriveEncoder[GithubEvents.Repo1]
        implicit val l = deriveEncoder[GithubEvents.Comment]
        implicit val m = deriveEncoder[GithubEvents.Comment1]
        implicit val n = deriveEncoder[GithubEvents.Commits]
        implicit val o = deriveEncoder[GithubEvents.CreateEvent]
        implicit val p = deriveEncoder[GithubEvents.Forkee]
        implicit val q = deriveEncoder[GithubEvents.ForkEvent]
        implicit val r = deriveEncoder[GithubEvents.Head]
        implicit val s = deriveEncoder[GithubEvents.PullRequest]
        implicit val t = deriveEncoder[GithubEvents.PullRequest1]
        implicit val u = deriveEncoder[GithubEvents.PullRequest2]
        implicit val v = deriveEncoder[GithubEvents.PullRequestEvent]
        implicit val w = deriveEncoder[GithubEvents.PullRequestReviewCommentEvent]
        implicit val x = deriveEncoder[GithubEvents.Issue]
        implicit val y = deriveEncoder[GithubEvents.Issue1]
        implicit val z = deriveEncoder[GithubEvents.IssueCommentEvent]
        implicit val A = deriveEncoder[GithubEvents.IssuesEvent]
        implicit val B = deriveEncoder[GithubEvents.PushEvent]
        implicit val C = deriveEncoder[GithubEvents.WatchEvent]
        implicit val D = deriveEncoder[GithubEvents.RootInterface]
        implicitly[Encoder[List[GithubEvents.RootInterface]]]

      case "github-gists.json" ⇒
        implicit val a = deriveEncoder[GithubGists.Owner]
        implicit val b = deriveEncoder[GithubGists.FileData]
        implicit val c = deriveEncoder[GithubGists.RootInterface]
        implicitly[Encoder[List[GithubGists.RootInterface]]]

      case "json-generator.json" ⇒
        implicit val a = deriveEncoder[JsonGenerator.Friends]
        implicit val b = deriveEncoder[JsonGenerator.Name]
        implicit val c = deriveEncoder[JsonGenerator.RootInterface]
        implicitly[Encoder[List[JsonGenerator.RootInterface]]]

      case "meteorites.json" ⇒
        implicit val a = deriveEncoder[Meteorites.Geolocation]
        implicit val b = deriveEncoder[Meteorites.RootInterface]
        implicitly[Encoder[List[Meteorites.RootInterface]]]

      case "movies.json" ⇒
        implicit val a = deriveEncoder[Movies.RootInterface]
        implicitly[Encoder[List[Movies.RootInterface]]]

      case "reddit-scala.json" ⇒
        implicit val a = deriveEncoder[Reddit.Oembed]
        implicit val b = deriveEncoder[Reddit.SecureMedia]
        implicit val c = deriveEncoder[Reddit.MediaEmbed]
        implicit val d = deriveEncoder[Reddit.Gildings]
        implicit val e = deriveEncoder[Reddit.Data]
        implicit val f = deriveEncoder[Reddit.Child]
        implicit val g = deriveEncoder[Reddit.Data0]
        deriveEncoder[Reddit.RootInterface]

      case "rick-morty.json" ⇒
        implicit val a = deriveEncoder[RickMorty.Rating]
        implicit val b = deriveEncoder[RickMorty.Schedule]
        implicit val c = deriveEncoder[RickMorty.Country]
        implicit val d = deriveEncoder[RickMorty.Network]
        implicit val e = deriveEncoder[RickMorty.Image]
        implicit val f = deriveEncoder[RickMorty.Externals]
        implicit val g = deriveEncoder[RickMorty.Self]
        implicit val h = deriveEncoder[RickMorty.Links]
        implicit val i = deriveEncoder[RickMorty.Links1]
        implicit val j = deriveEncoder[RickMorty.Episodes]
        implicit val k = deriveEncoder[RickMorty.Embedded]
        deriveEncoder[RickMorty.RootInterface]

      case "temp-anomaly.json" ⇒
        implicit val a = deriveEncoder[TempAnomaly.Description]
        deriveEncoder[TempAnomaly.RootInterface]

      case "thai-cinemas.json" ⇒
        implicit val a = deriveEncoder[ThaiCinemas.Group]
        implicit val b = deriveEncoder[ThaiCinemas.Results]
        deriveEncoder[ThaiCinemas.RootInterface]

      case "turkish.json" ⇒
        implicit val a = deriveEncoder[Turkish.Event]
        implicit val b = deriveEncoder[Turkish.Result]
        deriveEncoder[Turkish.RootInterface]

      case "twitter_api_compact_response.json" | "twitter_api_response.json" ⇒
        implicit val a = deriveEncoder[TwitterApiResponse.Urls]
        implicit val b = deriveEncoder[TwitterApiResponse.Url]
        implicit val c = deriveEncoder[TwitterApiResponse.UserMentions]
        implicit val d = deriveEncoder[TwitterApiResponse.Entities]
        implicit val e = deriveEncoder[TwitterApiResponse.Entities1]
        implicit val f = deriveEncoder[TwitterApiResponse.User]
        implicit val g = deriveEncoder[TwitterApiResponse.RetweetedStatus]
        implicit val h = deriveEncoder[TwitterApiResponse.RootInterface]
        implicitly[Encoder[List[TwitterApiResponse.RootInterface]]]
    }
    c.asInstanceOf[Encoder[Product]]
  }

  implicit lazy val decoder: Decoder[Product] = {

    implicit def nullableDecoder[T: Decoder: Default]: Decoder[Nullable[T]] =
      Decoder.instance { cursor ⇒
        if (cursor.value.isNull) Right(new Nullable(Default.get[T]))
        else cursor.as[T].right.map(new Nullable(_))
      }

    val c = fileName match {
      case "australia-abc.json" ⇒
        implicit val a = deriveDecoder[Australia.Geometry]
        implicit val b = deriveDecoder[Australia.Properties]
        implicit val c = deriveDecoder[Australia.Properties1]
        implicit val d = deriveDecoder[Australia.Features]
        implicit val e = deriveDecoder[Australia.Crs]
        deriveDecoder[Australia.RootInterface]

      case "bitcoin.json" ⇒
        implicit val a = deriveDecoder[Bitcoin.SpendingOutpoints]
        implicit val b = deriveDecoder[Bitcoin.PrevOut]
        implicit val c = deriveDecoder[Bitcoin.Inputs]
        implicit val d = deriveDecoder[Bitcoin.Out]
        implicit val e = deriveDecoder[Bitcoin.Txs]
        deriveDecoder[Bitcoin.RootInterface]

      case "doj-blog.json" ⇒
        implicit val a = deriveDecoder[DojBlog.ResponseInfo]
        implicit val b = deriveDecoder[DojBlog.Resultset]
        implicit val c = deriveDecoder[DojBlog.Metadata]
        implicit val d = deriveDecoder[DojBlog.Component]
        implicit val e = deriveDecoder[DojBlog.Results]
        deriveDecoder[DojBlog.RootInterface]

      case "eu-lobby-country.json" ⇒
        implicit val a = deriveDecoder[EuLobbyCountry.Facets]
        implicit val b = deriveDecoder[EuLobbyCountry.Results]
        deriveDecoder[EuLobbyCountry.RootInterface]

      case "eu-lobby-financial.json" ⇒
        implicit val a = deriveDecoder[EuLobbyFinancial.Facets]
        implicit val b = deriveDecoder[EuLobbyFinancial.CustomIncomes]
        implicit val c = deriveDecoder[EuLobbyFinancial.Results]
        deriveDecoder[EuLobbyFinancial.RootInterface]

      case "eu-lobby-repr.json" ⇒
        implicit val a = deriveDecoder[EuLobbyRepr.Facets]
        implicit val b = deriveDecoder[EuLobbyRepr.Results]
        deriveDecoder[EuLobbyRepr.RootInterface]

      case "github-events.json" ⇒
        implicit val a = deriveDecoder[GithubEvents.Actor]
        implicit val b = deriveDecoder[GithubEvents.Author]
        implicit val c = deriveDecoder[GithubEvents.Self]
        implicit val d = deriveDecoder[GithubEvents.Owner]
        implicit val e = deriveDecoder[GithubEvents.Org]
        implicit val f = deriveDecoder[GithubEvents.Links]
        implicit val g = deriveDecoder[GithubEvents.Links1]
        implicit val h = deriveDecoder[GithubEvents.Labels]
        implicit val i = deriveDecoder[GithubEvents.License]
        implicit val j = deriveDecoder[GithubEvents.Repo]
        implicit val k = deriveDecoder[GithubEvents.Repo1]
        implicit val l = deriveDecoder[GithubEvents.Comment]
        implicit val m = deriveDecoder[GithubEvents.Comment1]
        implicit val n = deriveDecoder[GithubEvents.Commits]
        implicit val o = deriveDecoder[GithubEvents.CreateEvent]
        implicit val p = deriveDecoder[GithubEvents.Forkee]
        implicit val q = deriveDecoder[GithubEvents.ForkEvent]
        implicit val r = deriveDecoder[GithubEvents.Head]
        implicit val s = deriveDecoder[GithubEvents.PullRequest]
        implicit val t = deriveDecoder[GithubEvents.PullRequest1]
        implicit val u = deriveDecoder[GithubEvents.PullRequest2]
        implicit val v = deriveDecoder[GithubEvents.PullRequestEvent]
        implicit val w = deriveDecoder[GithubEvents.PullRequestReviewCommentEvent]
        implicit val x = deriveDecoder[GithubEvents.Issue]
        implicit val y = deriveDecoder[GithubEvents.Issue1]
        implicit val z = deriveDecoder[GithubEvents.IssueCommentEvent]
        implicit val A = deriveDecoder[GithubEvents.IssuesEvent]
        implicit val B = deriveDecoder[GithubEvents.PushEvent]
        implicit val C = deriveDecoder[GithubEvents.WatchEvent]
        implicit val D = deriveDecoder[GithubEvents.RootInterface]
        implicitly[Decoder[List[GithubEvents.RootInterface]]]

      case "github-gists.json" ⇒
        implicit val a = deriveDecoder[GithubGists.Owner]
        implicit val b = deriveDecoder[GithubGists.FileData]
        implicit val c = deriveDecoder[GithubGists.RootInterface]
        implicitly[Decoder[List[GithubGists.RootInterface]]]

      case "json-generator.json" ⇒
        implicit val a = deriveDecoder[JsonGenerator.Friends]
        implicit val b = deriveDecoder[JsonGenerator.Name]
        implicit val c = deriveDecoder[JsonGenerator.RootInterface]
        implicitly[Decoder[List[JsonGenerator.RootInterface]]]

      case "meteorites.json" ⇒
        implicit val a = deriveDecoder[Meteorites.Geolocation]
        implicit val b = deriveDecoder[Meteorites.RootInterface]
        implicitly[Decoder[List[Meteorites.RootInterface]]]

      case "movies.json" ⇒
        implicit val a = deriveDecoder[Movies.RootInterface]
        implicitly[Decoder[List[Movies.RootInterface]]]

      case "reddit-scala.json" ⇒
        implicit val a = deriveDecoder[Reddit.Oembed]
        implicit val b = deriveDecoder[Reddit.SecureMedia]
        implicit val c = deriveDecoder[Reddit.MediaEmbed]
        implicit val d = deriveDecoder[Reddit.Gildings]
        implicit val e = deriveDecoder[Reddit.Data]
        implicit val f = deriveDecoder[Reddit.Child]
        implicit val g = deriveDecoder[Reddit.Data0]
        deriveDecoder[Reddit.RootInterface]

      case "rick-morty.json" ⇒
        implicit val a = deriveDecoder[RickMorty.Rating]
        implicit val b = deriveDecoder[RickMorty.Schedule]
        implicit val c = deriveDecoder[RickMorty.Country]
        implicit val d = deriveDecoder[RickMorty.Network]
        implicit val e = deriveDecoder[RickMorty.Image]
        implicit val f = deriveDecoder[RickMorty.Externals]
        implicit val g = deriveDecoder[RickMorty.Self]
        implicit val h = deriveDecoder[RickMorty.Links]
        implicit val i = deriveDecoder[RickMorty.Links1]
        implicit val j = deriveDecoder[RickMorty.Episodes]
        implicit val k = deriveDecoder[RickMorty.Embedded]
        deriveDecoder[RickMorty.RootInterface]

      case "temp-anomaly.json" ⇒
        implicit val a = deriveDecoder[TempAnomaly.Description]
        deriveDecoder[TempAnomaly.RootInterface]

      case "thai-cinemas.json" ⇒
        implicit val a = deriveDecoder[ThaiCinemas.Group]
        implicit val b = deriveDecoder[ThaiCinemas.Results]
        deriveDecoder[ThaiCinemas.RootInterface]

      case "turkish.json" ⇒
        implicit val a = deriveDecoder[Turkish.Event]
        implicit val b = deriveDecoder[Turkish.Result]
        deriveDecoder[Turkish.RootInterface]

      case "twitter_api_compact_response.json" | "twitter_api_response.json" ⇒
        implicit val a = deriveDecoder[TwitterApiResponse.Urls]
        implicit val b = deriveDecoder[TwitterApiResponse.Url]
        implicit val c = deriveDecoder[TwitterApiResponse.UserMentions]
        implicit val d = deriveDecoder[TwitterApiResponse.Entities]
        implicit val e = deriveDecoder[TwitterApiResponse.Entities1]
        implicit val f = deriveDecoder[TwitterApiResponse.User]
        implicit val g = deriveDecoder[TwitterApiResponse.RetweetedStatus]
        implicit val h = deriveDecoder[TwitterApiResponse.RootInterface]
        implicitly[Decoder[List[TwitterApiResponse.RootInterface]]]
    }
    c.asInstanceOf[Decoder[Product]]
  }

  def setup(): Unit = root = decode[Product](new String(fileBytes, UTF_8)) match {
    case Right(x) ⇒ x
    case Left(e)  ⇒ throw e
  }

  @Benchmark
  def encodeModel: Array[Byte] = encoder(root).noSpaces.getBytes(UTF_8)

  @Benchmark
  def decodeModel: Product =
    // first decoding into a string and then parsing appears to be faster than directly parsing from the bytes
    decode[Product](new String(fileBytes, UTF_8)).right.get
}
