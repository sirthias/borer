/*
 * Copyright (c) 2019-2024 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.benchmarks

import java.nio.charset.StandardCharsets.UTF_8

import io.bullet.borer.{Default, Nullable}
import io.circe._
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.jawn.{decode, parse}
import org.openjdk.jmh.annotations._

object CirceCodecs {
  implicit val fooEncoder: Encoder[Foo] = deriveEncoder
  implicit val fooDecoder: Decoder[Foo] = deriveDecoder

  implicit val foosEncoder: Encoder[Map[String, Foo]] = implicitly[Encoder[Map[String, Foo]]]
  implicit val foosDecoder: Decoder[Map[String, Foo]] = implicitly[Decoder[Map[String, Foo]]]

  implicit val intsEncoder: Encoder[List[Int]] = implicitly[Encoder[List[Int]]]
  implicit val intsDecoder: Decoder[List[Int]] = implicitly[Decoder[List[Int]]]

  val throwError = () => throw new IllegalStateException
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
  def decodeFoos: Map[String, Foo] = decode[Map[String, Foo]](new String(foosJson, UTF_8)).getOrElse(throwError())

  @Benchmark
  def decodeInts: List[Int] = decode[List[Int]](new String(intsJson, UTF_8)).getOrElse(throwError())

  @Benchmark
  def decodeEmptyArray: List[Int] = decode[List[Int]](new String(emptyArrayJson, UTF_8)).getOrElse(throwError())
}

class CirceDomBenchmark extends DomBenchmark {

  private var root: Json = _
  def setup(): Unit      = root = parse(new String(fileBytes, UTF_8)).getOrElse(throwError())

  @Benchmark
  def encodeDom: Array[Byte] = root.noSpaces.getBytes(UTF_8)

  @Benchmark
  def decodeDom: Json =
    // first decoding into a string and then parsing appears to be faster than directly parsing from the bytes
    parse(new String(fileBytes, UTF_8)).getOrElse(throwError())
}

class CirceModelBenchmark extends DomBenchmark {

  private var root: Product = _

  implicit lazy val encoder: Encoder[Product] = {

    implicit def nullableEncoder[T](implicit enc: Encoder[T]): Encoder[Nullable[T]] = enc.contramap(_.value)

    // format: OFF
    val c = fileName match {
      case "australia-abc.json" =>
        implicit val a: Encoder[Australia.Geometry] = deriveEncoder[Australia.Geometry]
        implicit val b: Encoder[Australia.Properties] = deriveEncoder[Australia.Properties]
        implicit val c: Encoder[Australia.Properties1] = deriveEncoder[Australia.Properties1]
        implicit val d: Encoder[Australia.Features] = deriveEncoder[Australia.Features]
        implicit val e: Encoder[Australia.Crs] = deriveEncoder[Australia.Crs]
        deriveEncoder[Australia.RootInterface]

      case "bitcoin.json" =>
        implicit val a: Encoder[Bitcoin.SpendingOutpoints] = deriveEncoder[Bitcoin.SpendingOutpoints]
        implicit val b: Encoder[Bitcoin.PrevOut] = deriveEncoder[Bitcoin.PrevOut]
        implicit val c: Encoder[Bitcoin.Inputs] = deriveEncoder[Bitcoin.Inputs]
        implicit val d: Encoder[Bitcoin.Out] = deriveEncoder[Bitcoin.Out]
        implicit val e: Encoder[Bitcoin.Txs] = deriveEncoder[Bitcoin.Txs]
        deriveEncoder[Bitcoin.RootInterface]

      case "doj-blog.json" =>
        implicit val a: Encoder[DojBlog.ResponseInfo] = deriveEncoder[DojBlog.ResponseInfo]
        implicit val b: Encoder[DojBlog.Resultset] = deriveEncoder[DojBlog.Resultset]
        implicit val c: Encoder[DojBlog.Metadata] = deriveEncoder[DojBlog.Metadata]
        implicit val d: Encoder[DojBlog.Component] = deriveEncoder[DojBlog.Component]
        implicit val e: Encoder[DojBlog.Results] = deriveEncoder[DojBlog.Results]
        deriveEncoder[DojBlog.RootInterface]

      case "eu-lobby-country.json" =>
        implicit val a: Encoder[EuLobbyCountry.Facets] = deriveEncoder[EuLobbyCountry.Facets]
        implicit val b: Encoder[EuLobbyCountry.Results] = deriveEncoder[EuLobbyCountry.Results]
        deriveEncoder[EuLobbyCountry.RootInterface]

      case "eu-lobby-financial.json" =>
        implicit val a: Encoder[EuLobbyFinancial.Facets] = deriveEncoder[EuLobbyFinancial.Facets]
        implicit val b: Encoder[EuLobbyFinancial.CustomIncomes] = deriveEncoder[EuLobbyFinancial.CustomIncomes]
        implicit val c: Encoder[EuLobbyFinancial.Results] = deriveEncoder[EuLobbyFinancial.Results]
        deriveEncoder[EuLobbyFinancial.RootInterface]

      case "eu-lobby-repr.json" =>
        implicit val a: Encoder[EuLobbyRepr.Facets] = deriveEncoder[EuLobbyRepr.Facets]
        implicit val b: Encoder[EuLobbyRepr.Results] = deriveEncoder[EuLobbyRepr.Results]
        deriveEncoder[EuLobbyRepr.RootInterface]

      case "github-events.json" =>
        import GithubEvents._
        implicit val a: Encoder[Actor] = deriveEncoder[Actor]
        implicit val b: Encoder[Author] = deriveEncoder[Author]
        implicit val c: Encoder[Self] = deriveEncoder[Self]
        implicit val d: Encoder[Owner] = deriveEncoder[Owner]
        implicit val e: Encoder[Org] = deriveEncoder[Org]
        implicit val f: Encoder[Links] = deriveEncoder[Links]
        implicit val g: Encoder[Links1] = deriveEncoder[Links1]
        implicit val h: Encoder[Labels] = deriveEncoder[Labels]
        implicit val i: Encoder[License] = deriveEncoder[License]
        implicit val j: Encoder[Repo] = deriveEncoder[Repo]
        implicit val k: Encoder[Repo1] = deriveEncoder[Repo1]
        implicit val l: Encoder[Comment] = deriveEncoder[Comment]
        implicit val m: Encoder[Comment1] = deriveEncoder[Comment1]
        implicit val n: Encoder[Commits] = deriveEncoder[Commits]
        implicit val o: Encoder[CreateEvent] = deriveEncoder[CreateEvent]
        implicit val p: Encoder[Forkee] = deriveEncoder[Forkee]
        implicit val q: Encoder[ForkEvent[Forkee]] = deriveEncoder[ForkEvent[Forkee]]
        implicit val r: Encoder[Head[Repo1]] = deriveEncoder[Head[Repo1]]
        implicit val s: Encoder[PullRequest] = deriveEncoder[PullRequest]
        implicit val t: Encoder[PullRequest1[Head[Repo1]]] = deriveEncoder[PullRequest1[Head[Repo1]]]
        implicit val u: Encoder[PullRequest2[Head[Repo1]]] = deriveEncoder[PullRequest2[Head[Repo1]]]
        implicit val v: Encoder[PullRequestEvent[Head[Repo1]]] = deriveEncoder[PullRequestEvent[Head[Repo1]]]
        implicit val w: Encoder[PullRequestReviewCommentEvent[Head[Repo1]]] = deriveEncoder[PullRequestReviewCommentEvent[Head[Repo1]]]
        implicit val x: Encoder[Issue] = deriveEncoder[Issue]
        implicit val y: Encoder[Issue1] = deriveEncoder[Issue1]
        implicit val z: Encoder[IssueCommentEvent] = deriveEncoder[IssueCommentEvent]
        implicit val A: Encoder[IssuesEvent] = deriveEncoder[IssuesEvent]
        implicit val B: Encoder[PushEvent] = deriveEncoder[PushEvent]
        implicit val C: Encoder[WatchEvent] = deriveEncoder[WatchEvent]
        implicit val D: Encoder[RootInterface[Head[Repo1], Forkee]] = deriveEncoder[RootInterface[Head[Repo1], Forkee]]
        implicitly[Encoder[List[RootInterface[Head[Repo1], Forkee]]]]

      case "github-gists.json" =>
        implicit val a: Encoder[GithubGists.Owner] = deriveEncoder[GithubGists.Owner]
        implicit val b: Encoder[GithubGists.FileData] = deriveEncoder[GithubGists.FileData]
        implicit val c: Encoder[GithubGists.RootInterface] = deriveEncoder[GithubGists.RootInterface]
        implicitly[Encoder[List[GithubGists.RootInterface]]]

      case "json-generator.json" =>
        implicit val a: Encoder[JsonGenerator.Friends] = deriveEncoder[JsonGenerator.Friends]
        implicit val b: Encoder[JsonGenerator.Name] = deriveEncoder[JsonGenerator.Name]
        implicit val c: Encoder[JsonGenerator.RootInterface] = deriveEncoder[JsonGenerator.RootInterface]
        implicitly[Encoder[List[JsonGenerator.RootInterface]]]

      case "meteorites.json" =>
        implicit val a: Encoder[Meteorites.Geolocation] = deriveEncoder[Meteorites.Geolocation]
        implicit val b: Encoder[Meteorites.RootInterface] = deriveEncoder[Meteorites.RootInterface]
        implicitly[Encoder[List[Meteorites.RootInterface]]]

      case "movies.json" =>
        implicit val a: Encoder[Movies.RootInterface] = deriveEncoder[Movies.RootInterface]
        implicitly[Encoder[List[Movies.RootInterface]]]

      case "reddit-scala.json" =>
        implicit val a: Encoder[Reddit.Oembed] = deriveEncoder[Reddit.Oembed]
        implicit val b: Encoder[Reddit.SecureMedia] = deriveEncoder[Reddit.SecureMedia]
        implicit val c: Encoder[Reddit.MediaEmbed] = deriveEncoder[Reddit.MediaEmbed]
        implicit val d: Encoder[Reddit.Gildings] = deriveEncoder[Reddit.Gildings]
        implicit val e: Encoder[Reddit.Data] = deriveEncoder[Reddit.Data]
        implicit val f: Encoder[Reddit.Child[Reddit.Data]] = deriveEncoder[Reddit.Child[Reddit.Data]]
        implicit val g: Encoder[Reddit.Data0[Reddit.Data]] = deriveEncoder[Reddit.Data0[Reddit.Data]]
        deriveEncoder[Reddit.RootInterface[Reddit.Data]]

      case "rick-morty.json" =>
        implicit val a: Encoder[RickMorty.Rating] = deriveEncoder[RickMorty.Rating]
        implicit val b: Encoder[RickMorty.Schedule] = deriveEncoder[RickMorty.Schedule]
        implicit val c: Encoder[RickMorty.Country] = deriveEncoder[RickMorty.Country]
        implicit val d: Encoder[RickMorty.Network] = deriveEncoder[RickMorty.Network]
        implicit val e: Encoder[RickMorty.Image] = deriveEncoder[RickMorty.Image]
        implicit val f: Encoder[RickMorty.Externals] = deriveEncoder[RickMorty.Externals]
        implicit val g: Encoder[RickMorty.Self] = deriveEncoder[RickMorty.Self]
        implicit val h: Encoder[RickMorty.Links] = deriveEncoder[RickMorty.Links]
        implicit val i: Encoder[RickMorty.Links1] = deriveEncoder[RickMorty.Links1]
        implicit val j: Encoder[RickMorty.Episodes] = deriveEncoder[RickMorty.Episodes]
        implicit val k: Encoder[RickMorty.Embedded] = deriveEncoder[RickMorty.Embedded]
        deriveEncoder[RickMorty.RootInterface]

      case "temp-anomaly.json" =>
        implicit val a: Encoder[TempAnomaly.Description] = deriveEncoder[TempAnomaly.Description]
        deriveEncoder[TempAnomaly.RootInterface]

      case "thai-cinemas.json" =>
        implicit val a: Encoder[ThaiCinemas.Group] = deriveEncoder[ThaiCinemas.Group]
        implicit val b: Encoder[ThaiCinemas.Results] = deriveEncoder[ThaiCinemas.Results]
        deriveEncoder[ThaiCinemas.RootInterface]

      case "turkish.json" =>
        implicit val a: Encoder[Turkish.Event] = deriveEncoder[Turkish.Event]
        implicit val b: Encoder[Turkish.Result] = deriveEncoder[Turkish.Result]
        deriveEncoder[Turkish.RootInterface]

      case "twitter_api_compact_response.json" | "twitter_api_response.json" =>
        implicit val a: Encoder[TwitterApiResponse.Urls] = deriveEncoder[TwitterApiResponse.Urls]
        implicit val b: Encoder[TwitterApiResponse.Url] = deriveEncoder[TwitterApiResponse.Url]
        implicit val c: Encoder[TwitterApiResponse.UserMentions] = deriveEncoder[TwitterApiResponse.UserMentions]
        implicit val d: Encoder[TwitterApiResponse.Entities] = deriveEncoder[TwitterApiResponse.Entities]
        implicit val e: Encoder[TwitterApiResponse.Entities1] = deriveEncoder[TwitterApiResponse.Entities1]
        implicit val f: Encoder[TwitterApiResponse.User] = deriveEncoder[TwitterApiResponse.User]
        implicit val g: Encoder[TwitterApiResponse.RetweetedStatus] = deriveEncoder[TwitterApiResponse.RetweetedStatus]
        implicit val h: Encoder[TwitterApiResponse.RootInterface] = deriveEncoder[TwitterApiResponse.RootInterface]
        implicitly[Encoder[List[TwitterApiResponse.RootInterface]]]
    }
    c.asInstanceOf[Encoder[Product]]
    // format: ON
  }

  implicit lazy val decoder: Decoder[Product] = {

    implicit def nullableDecoder[T: Decoder: Default]: Decoder[Nullable[T]] =
      Decoder.instance { cursor =>
        if (cursor.value.isNull) Right(new Nullable(Default.of[T]))
        else cursor.as[T].map(new Nullable(_))
      }

    // format: OFF
    val c = fileName match {
      case "australia-abc.json" =>
        implicit val a: Decoder[Australia.Geometry] = deriveDecoder[Australia.Geometry]
        implicit val b: Decoder[Australia.Properties] = deriveDecoder[Australia.Properties]
        implicit val c: Decoder[Australia.Properties1] = deriveDecoder[Australia.Properties1]
        implicit val d: Decoder[Australia.Features] = deriveDecoder[Australia.Features]
        implicit val e: Decoder[Australia.Crs] = deriveDecoder[Australia.Crs]
        deriveDecoder[Australia.RootInterface]

      case "bitcoin.json" =>
        implicit val a: Decoder[Bitcoin.SpendingOutpoints] = deriveDecoder[Bitcoin.SpendingOutpoints]
        implicit val b: Decoder[Bitcoin.PrevOut] = deriveDecoder[Bitcoin.PrevOut]
        implicit val c: Decoder[Bitcoin.Inputs] = deriveDecoder[Bitcoin.Inputs]
        implicit val d: Decoder[Bitcoin.Out] = deriveDecoder[Bitcoin.Out]
        implicit val e: Decoder[Bitcoin.Txs] = deriveDecoder[Bitcoin.Txs]
        deriveDecoder[Bitcoin.RootInterface]

      case "doj-blog.json" =>
        implicit val a: Decoder[DojBlog.ResponseInfo] = deriveDecoder[DojBlog.ResponseInfo]
        implicit val b: Decoder[DojBlog.Resultset] = deriveDecoder[DojBlog.Resultset]
        implicit val c: Decoder[DojBlog.Metadata] = deriveDecoder[DojBlog.Metadata]
        implicit val d: Decoder[DojBlog.Component] = deriveDecoder[DojBlog.Component]
        implicit val e: Decoder[DojBlog.Results] = deriveDecoder[DojBlog.Results]
        deriveDecoder[DojBlog.RootInterface]

      case "eu-lobby-country.json" =>
        implicit val a: Decoder[EuLobbyCountry.Facets] = deriveDecoder[EuLobbyCountry.Facets]
        implicit val b: Decoder[EuLobbyCountry.Results] = deriveDecoder[EuLobbyCountry.Results]
        deriveDecoder[EuLobbyCountry.RootInterface]

      case "eu-lobby-financial.json" =>
        implicit val a: Decoder[EuLobbyFinancial.Facets] = deriveDecoder[EuLobbyFinancial.Facets]
        implicit val b: Decoder[EuLobbyFinancial.CustomIncomes] = deriveDecoder[EuLobbyFinancial.CustomIncomes]
        implicit val c: Decoder[EuLobbyFinancial.Results] = deriveDecoder[EuLobbyFinancial.Results]
        deriveDecoder[EuLobbyFinancial.RootInterface]

      case "eu-lobby-repr.json" =>
        implicit val a: Decoder[EuLobbyRepr.Facets] = deriveDecoder[EuLobbyRepr.Facets]
        implicit val b: Decoder[EuLobbyRepr.Results] = deriveDecoder[EuLobbyRepr.Results]
        deriveDecoder[EuLobbyRepr.RootInterface]

      case "github-events.json" =>
        import GithubEvents._
        implicit val a: Decoder[Actor] = deriveDecoder[Actor]
        implicit val b: Decoder[Author] = deriveDecoder[Author]
        implicit val c: Decoder[Self] = deriveDecoder[Self]
        implicit val d: Decoder[Owner] = deriveDecoder[Owner]
        implicit val e: Decoder[Org] = deriveDecoder[Org]
        implicit val f: Decoder[Links] = deriveDecoder[Links]
        implicit val g: Decoder[Links1] = deriveDecoder[Links1]
        implicit val h: Decoder[Labels] = deriveDecoder[Labels]
        implicit val i: Decoder[License] = deriveDecoder[License]
        implicit val j: Decoder[Repo] = deriveDecoder[Repo]
        implicit val k: Decoder[Repo1] = deriveDecoder[Repo1]
        implicit val l: Decoder[Comment] = deriveDecoder[Comment]
        implicit val m: Decoder[Comment1] = deriveDecoder[Comment1]
        implicit val n: Decoder[Commits] = deriveDecoder[Commits]
        implicit val o: Decoder[CreateEvent] = deriveDecoder[CreateEvent]
        implicit val p: Decoder[Forkee] = deriveDecoder[Forkee]
        implicit val q: Decoder[ForkEvent[Forkee]] = deriveDecoder[ForkEvent[Forkee]]
        implicit val r: Decoder[Head[Repo1]] = deriveDecoder[Head[Repo1]]
        implicit val s: Decoder[PullRequest] = deriveDecoder[PullRequest]
        implicit val t: Decoder[PullRequest1[Head[Repo1]]] = deriveDecoder[PullRequest1[Head[Repo1]]]
        implicit val u: Decoder[PullRequest2[Head[Repo1]]] = deriveDecoder[PullRequest2[Head[Repo1]]]
        implicit val v: Decoder[PullRequestEvent[Head[Repo1]]] = deriveDecoder[PullRequestEvent[Head[Repo1]]]
        implicit val w: Decoder[PullRequestReviewCommentEvent[Head[Repo1]]] = deriveDecoder[PullRequestReviewCommentEvent[Head[Repo1]]]
        implicit val x: Decoder[Issue] = deriveDecoder[Issue]
        implicit val y: Decoder[Issue1] = deriveDecoder[Issue1]
        implicit val z: Decoder[IssueCommentEvent] = deriveDecoder[IssueCommentEvent]
        implicit val A: Decoder[IssuesEvent] = deriveDecoder[IssuesEvent]
        implicit val B: Decoder[PushEvent] = deriveDecoder[PushEvent]
        implicit val C: Decoder[WatchEvent] = deriveDecoder[WatchEvent]
        implicit val D: Decoder[RootInterface[Head[Repo1], Forkee]] = deriveDecoder[RootInterface[Head[Repo1], Forkee]]
        implicitly[Decoder[List[RootInterface[Head[Repo1], Forkee]]]]

      case "github-gists.json" =>
        implicit val a: Decoder[GithubGists.Owner] = deriveDecoder[GithubGists.Owner]
        implicit val b: Decoder[GithubGists.FileData] = deriveDecoder[GithubGists.FileData]
        implicit val c: Decoder[GithubGists.RootInterface] = deriveDecoder[GithubGists.RootInterface]
        implicitly[Decoder[List[GithubGists.RootInterface]]]

      case "json-generator.json" =>
        implicit val a: Decoder[JsonGenerator.Friends] = deriveDecoder[JsonGenerator.Friends]
        implicit val b: Decoder[JsonGenerator.Name] = deriveDecoder[JsonGenerator.Name]
        implicit val c: Decoder[JsonGenerator.RootInterface] = deriveDecoder[JsonGenerator.RootInterface]
        implicitly[Decoder[List[JsonGenerator.RootInterface]]]

      case "meteorites.json" =>
        implicit val a: Decoder[Meteorites.Geolocation] = deriveDecoder[Meteorites.Geolocation]
        implicit val b: Decoder[Meteorites.RootInterface] = deriveDecoder[Meteorites.RootInterface]
        implicitly[Decoder[List[Meteorites.RootInterface]]]

      case "movies.json" =>
        implicit val a: Decoder[Movies.RootInterface] = deriveDecoder[Movies.RootInterface]
        implicitly[Decoder[List[Movies.RootInterface]]]

      case "reddit-scala.json" =>
        implicit val a: Decoder[Reddit.Oembed] = deriveDecoder[Reddit.Oembed]
        implicit val b: Decoder[Reddit.SecureMedia] = deriveDecoder[Reddit.SecureMedia]
        implicit val c: Decoder[Reddit.MediaEmbed] = deriveDecoder[Reddit.MediaEmbed]
        implicit val d: Decoder[Reddit.Gildings] = deriveDecoder[Reddit.Gildings]
        implicit val e: Decoder[Reddit.Data] = deriveDecoder[Reddit.Data]
        implicit val f: Decoder[Reddit.Child[Reddit.Data]] = deriveDecoder[Reddit.Child[Reddit.Data]]
        implicit val g: Decoder[Reddit.Data0[Reddit.Data]] = deriveDecoder[Reddit.Data0[Reddit.Data]]
        deriveDecoder[Reddit.RootInterface[Reddit.Data]]

      case "rick-morty.json" =>
        implicit val a: Decoder[RickMorty.Rating] = deriveDecoder[RickMorty.Rating]
        implicit val b: Decoder[RickMorty.Schedule] = deriveDecoder[RickMorty.Schedule]
        implicit val c: Decoder[RickMorty.Country] = deriveDecoder[RickMorty.Country]
        implicit val d: Decoder[RickMorty.Network] = deriveDecoder[RickMorty.Network]
        implicit val e: Decoder[RickMorty.Image] = deriveDecoder[RickMorty.Image]
        implicit val f: Decoder[RickMorty.Externals] = deriveDecoder[RickMorty.Externals]
        implicit val g: Decoder[RickMorty.Self] = deriveDecoder[RickMorty.Self]
        implicit val h: Decoder[RickMorty.Links] = deriveDecoder[RickMorty.Links]
        implicit val i: Decoder[RickMorty.Links1] = deriveDecoder[RickMorty.Links1]
        implicit val j: Decoder[RickMorty.Episodes] = deriveDecoder[RickMorty.Episodes]
        implicit val k: Decoder[RickMorty.Embedded] = deriveDecoder[RickMorty.Embedded]
        deriveDecoder[RickMorty.RootInterface]

      case "temp-anomaly.json" =>
        implicit val a: Decoder[TempAnomaly.Description] = deriveDecoder[TempAnomaly.Description]
        deriveDecoder[TempAnomaly.RootInterface]

      case "thai-cinemas.json" =>
        implicit val a: Decoder[ThaiCinemas.Group] = deriveDecoder[ThaiCinemas.Group]
        implicit val b: Decoder[ThaiCinemas.Results] = deriveDecoder[ThaiCinemas.Results]
        deriveDecoder[ThaiCinemas.RootInterface]

      case "turkish.json" =>
        implicit val a: Decoder[Turkish.Event] = deriveDecoder[Turkish.Event]
        implicit val b: Decoder[Turkish.Result] = deriveDecoder[Turkish.Result]
        deriveDecoder[Turkish.RootInterface]

      case "twitter_api_compact_response.json" | "twitter_api_response.json" =>
        implicit val a: Decoder[TwitterApiResponse.Urls] = deriveDecoder[TwitterApiResponse.Urls]
        implicit val b: Decoder[TwitterApiResponse.Url] = deriveDecoder[TwitterApiResponse.Url]
        implicit val c: Decoder[TwitterApiResponse.UserMentions] = deriveDecoder[TwitterApiResponse.UserMentions]
        implicit val d: Decoder[TwitterApiResponse.Entities] = deriveDecoder[TwitterApiResponse.Entities]
        implicit val e: Decoder[TwitterApiResponse.Entities1] = deriveDecoder[TwitterApiResponse.Entities1]
        implicit val f: Decoder[TwitterApiResponse.User] = deriveDecoder[TwitterApiResponse.User]
        implicit val g: Decoder[TwitterApiResponse.RetweetedStatus] = deriveDecoder[TwitterApiResponse.RetweetedStatus]
        implicit val h: Decoder[TwitterApiResponse.RootInterface] = deriveDecoder[TwitterApiResponse.RootInterface]
        implicitly[Decoder[List[TwitterApiResponse.RootInterface]]]
    }
    c.asInstanceOf[Decoder[Product]]
    // format: ON
  }

  def setup(): Unit =
    root = decode[Product](new String(fileBytes, UTF_8)) match {
      case Right(x) => x
      case Left(e)  => throw e
    }

  @Benchmark
  def encodeModel: Array[Byte] = encoder(root).noSpaces.getBytes(UTF_8)

  @Benchmark
  def decodeModel: Product =
    // first decoding into a string and then parsing appears to be faster than directly parsing from the bytes
    decode[Product](new String(fileBytes, UTF_8)).getOrElse(throwError())
}
