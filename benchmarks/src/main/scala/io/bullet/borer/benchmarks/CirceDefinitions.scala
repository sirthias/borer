/*
 * Copyright (c) 2019-2026 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.benchmarks

import java.nio.charset.StandardCharsets.UTF_8

import io.bullet.borer.{Default, Nullable}
import io.circe.*
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.jawn.{decode, parse}
import org.openjdk.jmh.annotations.*

import scala.compiletime.uninitialized

object CirceCodecs:
  given Encoder[Foo] = deriveEncoder
  given Decoder[Foo] = deriveDecoder

  val foosEncoder: Encoder[Map[String, Foo]] = summon[Encoder[Map[String, Foo]]]
  val foosDecoder: Decoder[Map[String, Foo]] = summon[Decoder[Map[String, Foo]]]

  val intsEncoder: Encoder[List[Int]] = summon[Encoder[List[Int]]]
  val intsDecoder: Decoder[List[Int]] = summon[Decoder[List[Int]]]

  val throwError: () => Nothing = () => throw new IllegalStateException

import io.bullet.borer.benchmarks.CirceCodecs.*

class CirceEncodingBenchmark extends EncodingBenchmark:

  @Benchmark
  def encodeFoos: Array[Byte] = foosEncoder(foos).noSpaces.getBytes(UTF_8)

  @Benchmark
  def encodeInts: Array[Byte] = intsEncoder(ints).noSpaces.getBytes(UTF_8)

  @Benchmark
  def encodeEmptyArray: Array[Byte] = intsEncoder(Nil).noSpaces.getBytes(UTF_8)

class CirceDecodingBenchmark extends DecodingBenchmark:

  @Benchmark
  def decodeFoos: Map[String, Foo] =
    decode[Map[String, Foo]](new String(foosJson, UTF_8))(using foosDecoder).getOrElse(throwError())

  @Benchmark
  def decodeInts: List[Int] = decode[List[Int]](new String(intsJson, UTF_8))(using intsDecoder).getOrElse(throwError())

  @Benchmark
  def decodeEmptyArray: List[Int] =
    decode[List[Int]](new String(emptyArrayJson, UTF_8))(using intsDecoder).getOrElse(throwError())

class CirceDomBenchmark extends DomBenchmark:

  private var root: Json = uninitialized
  def setup(): Unit      = root = parse(new String(fileBytes, UTF_8)).getOrElse(throwError())

  @Benchmark
  def encodeDom: Array[Byte] = root.noSpaces.getBytes(UTF_8)

  @Benchmark
  def decodeDom: Json =
    // first decoding into a string and then parsing appears to be faster than directly parsing from the bytes
    parse(new String(fileBytes, UTF_8)).getOrElse(throwError())

class CirceModelBenchmark extends DomBenchmark:

  private var root: Product = uninitialized

  private lazy val encoder: Encoder[Product] =

    given nullableEncoder[T](using enc: Encoder[T]): Encoder[Nullable[T]] = enc.contramap(x => x)

    // format: OFF
    val c = fileName match
      case "australia-abc.json" =>
        given Encoder[Australia.Geometry] = deriveEncoder[Australia.Geometry]
        given Encoder[Australia.Properties] = deriveEncoder[Australia.Properties]
        given Encoder[Australia.Properties1] = deriveEncoder[Australia.Properties1]
        given Encoder[Australia.Features] = deriveEncoder[Australia.Features]
        given Encoder[Australia.Crs] = deriveEncoder[Australia.Crs]
        deriveEncoder[Australia.RootInterface]

      case "bitcoin.json" =>
        given Encoder[Bitcoin.SpendingOutpoints] = deriveEncoder[Bitcoin.SpendingOutpoints]
        given Encoder[Bitcoin.PrevOut] = deriveEncoder[Bitcoin.PrevOut]
        given Encoder[Bitcoin.Inputs] = deriveEncoder[Bitcoin.Inputs]
        given Encoder[Bitcoin.Out] = deriveEncoder[Bitcoin.Out]
        given Encoder[Bitcoin.Txs] = deriveEncoder[Bitcoin.Txs]
        deriveEncoder[Bitcoin.RootInterface]

      case "doj-blog.json" =>
        given Encoder[DojBlog.ResponseInfo] = deriveEncoder[DojBlog.ResponseInfo]
        given Encoder[DojBlog.Resultset] = deriveEncoder[DojBlog.Resultset]
        given Encoder[DojBlog.Metadata] = deriveEncoder[DojBlog.Metadata]
        given Encoder[DojBlog.Component] = deriveEncoder[DojBlog.Component]
        given Encoder[DojBlog.Results] = deriveEncoder[DojBlog.Results]
        deriveEncoder[DojBlog.RootInterface]

      case "eu-lobby-country.json" =>
        given Encoder[EuLobbyCountry.Facets] = deriveEncoder[EuLobbyCountry.Facets]
        given Encoder[EuLobbyCountry.Results] = deriveEncoder[EuLobbyCountry.Results]
        deriveEncoder[EuLobbyCountry.RootInterface]

      case "eu-lobby-financial.json" =>
        given Encoder[EuLobbyFinancial.Facets] = deriveEncoder[EuLobbyFinancial.Facets]
        given Encoder[EuLobbyFinancial.CustomIncomes] = deriveEncoder[EuLobbyFinancial.CustomIncomes]
        given Encoder[EuLobbyFinancial.Results] = deriveEncoder[EuLobbyFinancial.Results]
        deriveEncoder[EuLobbyFinancial.RootInterface]

      case "eu-lobby-repr.json" =>
        given Encoder[EuLobbyRepr.Facets] = deriveEncoder[EuLobbyRepr.Facets]
        given Encoder[EuLobbyRepr.Results] = deriveEncoder[EuLobbyRepr.Results]
        deriveEncoder[EuLobbyRepr.RootInterface]

      case "github-events.json" =>
        import GithubEvents.*
        given Encoder[Actor] = deriveEncoder[Actor]
        given Encoder[Author] = deriveEncoder[Author]
        given Encoder[Self] = deriveEncoder[Self]
        given Encoder[Owner] = deriveEncoder[Owner]
        given Encoder[Org] = deriveEncoder[Org]
        given Encoder[Links] = deriveEncoder[Links]
        given Encoder[Links1] = deriveEncoder[Links1]
        given Encoder[Labels] = deriveEncoder[Labels]
        given Encoder[License] = deriveEncoder[License]
        given Encoder[Repo] = deriveEncoder[Repo]
        given Encoder[Repo1] = deriveEncoder[Repo1]
        given Encoder[Comment] = deriveEncoder[Comment]
        given Encoder[Comment1] = deriveEncoder[Comment1]
        given Encoder[Commits] = deriveEncoder[Commits]
        given Encoder[CreateEvent] = deriveEncoder[CreateEvent]
        given Encoder[Forkee] = deriveEncoder[Forkee]
        given Encoder[ForkEvent[Forkee]] = deriveEncoder[ForkEvent[Forkee]]
        given Encoder[Head[Repo1]] = deriveEncoder[Head[Repo1]]
        given Encoder[PullRequest] = deriveEncoder[PullRequest]
        given Encoder[PullRequest1[Head[Repo1]]] = deriveEncoder[PullRequest1[Head[Repo1]]]
        given Encoder[PullRequest2[Head[Repo1]]] = deriveEncoder[PullRequest2[Head[Repo1]]]
        given Encoder[PullRequestEvent[Head[Repo1]]] = deriveEncoder[PullRequestEvent[Head[Repo1]]]
        given Encoder[PullRequestReviewCommentEvent[Head[Repo1]]] = deriveEncoder[PullRequestReviewCommentEvent[Head[Repo1]]]
        given Encoder[Issue] = deriveEncoder[Issue]
        given Encoder[Issue1] = deriveEncoder[Issue1]
        given Encoder[IssueCommentEvent] = deriveEncoder[IssueCommentEvent]
        given Encoder[IssuesEvent] = deriveEncoder[IssuesEvent]
        given Encoder[PushEvent] = deriveEncoder[PushEvent]
        given Encoder[WatchEvent] = deriveEncoder[WatchEvent]
        given Encoder[RootInterface[Head[Repo1], Forkee]] = deriveEncoder[RootInterface[Head[Repo1], Forkee]]
        summon[Encoder[List[RootInterface[Head[Repo1], Forkee]]]]

      case "github-gists.json" =>
        given Encoder[GithubGists.Owner] = deriveEncoder[GithubGists.Owner]
        given Encoder[GithubGists.FileData] = deriveEncoder[GithubGists.FileData]
        given Encoder[GithubGists.RootInterface] = deriveEncoder[GithubGists.RootInterface]
        summon[Encoder[List[GithubGists.RootInterface]]]

      case "json-generator.json" =>
        given Encoder[JsonGenerator.Friends] = deriveEncoder[JsonGenerator.Friends]
        given Encoder[JsonGenerator.Name] = deriveEncoder[JsonGenerator.Name]
        given Encoder[JsonGenerator.RootInterface] = deriveEncoder[JsonGenerator.RootInterface]
        summon[Encoder[List[JsonGenerator.RootInterface]]]

      case "meteorites.json" =>
        given Encoder[Meteorites.Geolocation] = deriveEncoder[Meteorites.Geolocation]
        given Encoder[Meteorites.RootInterface] = deriveEncoder[Meteorites.RootInterface]
        summon[Encoder[List[Meteorites.RootInterface]]]

      case "movies.json" =>
        given Encoder[Movies.RootInterface] = deriveEncoder[Movies.RootInterface]
        summon[Encoder[List[Movies.RootInterface]]]

      case "reddit-scala.json" =>
        given Encoder[Reddit.Oembed] = deriveEncoder[Reddit.Oembed]
        given Encoder[Reddit.SecureMedia] = deriveEncoder[Reddit.SecureMedia]
        given Encoder[Reddit.MediaEmbed] = deriveEncoder[Reddit.MediaEmbed]
        given Encoder[Reddit.Gildings] = deriveEncoder[Reddit.Gildings]
        given Encoder[Reddit.Data] = deriveEncoder[Reddit.Data]
        given Encoder[Reddit.Child[Reddit.Data]] = deriveEncoder[Reddit.Child[Reddit.Data]]
        given Encoder[Reddit.Data0[Reddit.Data]] = deriveEncoder[Reddit.Data0[Reddit.Data]]
        deriveEncoder[Reddit.RootInterface[Reddit.Data]]

      case "rick-morty.json" =>
        given Encoder[RickMorty.Rating] = deriveEncoder[RickMorty.Rating]
        given Encoder[RickMorty.Schedule] = deriveEncoder[RickMorty.Schedule]
        given Encoder[RickMorty.Country] = deriveEncoder[RickMorty.Country]
        given Encoder[RickMorty.Network] = deriveEncoder[RickMorty.Network]
        given Encoder[RickMorty.Image] = deriveEncoder[RickMorty.Image]
        given Encoder[RickMorty.Externals] = deriveEncoder[RickMorty.Externals]
        given Encoder[RickMorty.Self] = deriveEncoder[RickMorty.Self]
        given Encoder[RickMorty.Links] = deriveEncoder[RickMorty.Links]
        given Encoder[RickMorty.Links1] = deriveEncoder[RickMorty.Links1]
        given Encoder[RickMorty.Episodes] = deriveEncoder[RickMorty.Episodes]
        given Encoder[RickMorty.Embedded] = deriveEncoder[RickMorty.Embedded]
        deriveEncoder[RickMorty.RootInterface]

      case "temp-anomaly.json" =>
        given Encoder[TempAnomaly.Description] = deriveEncoder[TempAnomaly.Description]
        deriveEncoder[TempAnomaly.RootInterface]

      case "thai-cinemas.json" =>
        given Encoder[ThaiCinemas.Group] = deriveEncoder[ThaiCinemas.Group]
        given Encoder[ThaiCinemas.Results] = deriveEncoder[ThaiCinemas.Results]
        deriveEncoder[ThaiCinemas.RootInterface]

      case "turkish.json" =>
        given Encoder[Turkish.Event] = deriveEncoder[Turkish.Event]
        given Encoder[Turkish.Result] = deriveEncoder[Turkish.Result]
        deriveEncoder[Turkish.RootInterface]

      case "twitter_api_compact_response.json" | "twitter_api_response.json" =>
        given Encoder[TwitterApiResponse.Urls] = deriveEncoder[TwitterApiResponse.Urls]
        given Encoder[TwitterApiResponse.Url] = deriveEncoder[TwitterApiResponse.Url]
        given Encoder[TwitterApiResponse.UserMentions] = deriveEncoder[TwitterApiResponse.UserMentions]
        given Encoder[TwitterApiResponse.Entities] = deriveEncoder[TwitterApiResponse.Entities]
        given Encoder[TwitterApiResponse.Entities1] = deriveEncoder[TwitterApiResponse.Entities1]
        given Encoder[TwitterApiResponse.User] = deriveEncoder[TwitterApiResponse.User]
        given Encoder[TwitterApiResponse.RetweetedStatus] = deriveEncoder[TwitterApiResponse.RetweetedStatus]
        given Encoder[TwitterApiResponse.RootInterface] = deriveEncoder[TwitterApiResponse.RootInterface]
        summon[Encoder[List[TwitterApiResponse.RootInterface]]]
    c.asInstanceOf[Encoder[Product]]
    // format: ON

  private lazy val decoder: Decoder[Product] =

    given nullableDecoder[T: {Decoder, Default}]: Decoder[Nullable[T]] =
      Decoder.instance { cursor =>
        if (cursor.value.isNull) Right(Nullable(Default.of[T]))
        else cursor.as[T].map(Nullable(_))
      }

    // format: OFF
    val c = fileName match
      case "australia-abc.json" =>
        given Decoder[Australia.Geometry] = deriveDecoder[Australia.Geometry]
        given Decoder[Australia.Properties] = deriveDecoder[Australia.Properties]
        given Decoder[Australia.Properties1] = deriveDecoder[Australia.Properties1]
        given Decoder[Australia.Features] = deriveDecoder[Australia.Features]
        given Decoder[Australia.Crs] = deriveDecoder[Australia.Crs]
        deriveDecoder[Australia.RootInterface]

      case "bitcoin.json" =>
        given Decoder[Bitcoin.SpendingOutpoints] = deriveDecoder[Bitcoin.SpendingOutpoints]
        given Decoder[Bitcoin.PrevOut] = deriveDecoder[Bitcoin.PrevOut]
        given Decoder[Bitcoin.Inputs] = deriveDecoder[Bitcoin.Inputs]
        given Decoder[Bitcoin.Out] = deriveDecoder[Bitcoin.Out]
        given Decoder[Bitcoin.Txs] = deriveDecoder[Bitcoin.Txs]
        deriveDecoder[Bitcoin.RootInterface]

      case "doj-blog.json" =>
        given Decoder[DojBlog.ResponseInfo] = deriveDecoder[DojBlog.ResponseInfo]
        given Decoder[DojBlog.Resultset] = deriveDecoder[DojBlog.Resultset]
        given Decoder[DojBlog.Metadata] = deriveDecoder[DojBlog.Metadata]
        given Decoder[DojBlog.Component] = deriveDecoder[DojBlog.Component]
        given Decoder[DojBlog.Results] = deriveDecoder[DojBlog.Results]
        deriveDecoder[DojBlog.RootInterface]

      case "eu-lobby-country.json" =>
        given Decoder[EuLobbyCountry.Facets] = deriveDecoder[EuLobbyCountry.Facets]
        given Decoder[EuLobbyCountry.Results] = deriveDecoder[EuLobbyCountry.Results]
        deriveDecoder[EuLobbyCountry.RootInterface]

      case "eu-lobby-financial.json" =>
        given Decoder[EuLobbyFinancial.Facets] = deriveDecoder[EuLobbyFinancial.Facets]
        given Decoder[EuLobbyFinancial.CustomIncomes] = deriveDecoder[EuLobbyFinancial.CustomIncomes]
        given Decoder[EuLobbyFinancial.Results] = deriveDecoder[EuLobbyFinancial.Results]
        deriveDecoder[EuLobbyFinancial.RootInterface]

      case "eu-lobby-repr.json" =>
        given Decoder[EuLobbyRepr.Facets] = deriveDecoder[EuLobbyRepr.Facets]
        given Decoder[EuLobbyRepr.Results] = deriveDecoder[EuLobbyRepr.Results]
        deriveDecoder[EuLobbyRepr.RootInterface]

      case "github-events.json" =>
        import GithubEvents.*
        given Decoder[Actor] = deriveDecoder[Actor]
        given Decoder[Author] = deriveDecoder[Author]
        given Decoder[Self] = deriveDecoder[Self]
        given Decoder[Owner] = deriveDecoder[Owner]
        given Decoder[Org] = deriveDecoder[Org]
        given Decoder[Links] = deriveDecoder[Links]
        given Decoder[Links1] = deriveDecoder[Links1]
        given Decoder[Labels] = deriveDecoder[Labels]
        given Decoder[License] = deriveDecoder[License]
        given Decoder[Repo] = deriveDecoder[Repo]
        given Decoder[Repo1] = deriveDecoder[Repo1]
        given Decoder[Comment] = deriveDecoder[Comment]
        given Decoder[Comment1] = deriveDecoder[Comment1]
        given Decoder[Commits] = deriveDecoder[Commits]
        given Decoder[CreateEvent] = deriveDecoder[CreateEvent]
        given Decoder[Forkee] = deriveDecoder[Forkee]
        given Decoder[ForkEvent[Forkee]] = deriveDecoder[ForkEvent[Forkee]]
        given Decoder[Head[Repo1]] = deriveDecoder[Head[Repo1]]
        given Decoder[PullRequest] = deriveDecoder[PullRequest]
        given Decoder[PullRequest1[Head[Repo1]]] = deriveDecoder[PullRequest1[Head[Repo1]]]
        given Decoder[PullRequest2[Head[Repo1]]] = deriveDecoder[PullRequest2[Head[Repo1]]]
        given Decoder[PullRequestEvent[Head[Repo1]]] = deriveDecoder[PullRequestEvent[Head[Repo1]]]
        given Decoder[PullRequestReviewCommentEvent[Head[Repo1]]] = deriveDecoder[PullRequestReviewCommentEvent[Head[Repo1]]]
        given Decoder[Issue] = deriveDecoder[Issue]
        given Decoder[Issue1] = deriveDecoder[Issue1]
        given Decoder[IssueCommentEvent] = deriveDecoder[IssueCommentEvent]
        given Decoder[IssuesEvent] = deriveDecoder[IssuesEvent]
        given Decoder[PushEvent] = deriveDecoder[PushEvent]
        given Decoder[WatchEvent] = deriveDecoder[WatchEvent]
        given Decoder[RootInterface[Head[Repo1], Forkee]] = deriveDecoder[RootInterface[Head[Repo1], Forkee]]
        summon[Decoder[List[RootInterface[Head[Repo1], Forkee]]]]

      case "github-gists.json" =>
        given Decoder[GithubGists.Owner] = deriveDecoder[GithubGists.Owner]
        given Decoder[GithubGists.FileData] = deriveDecoder[GithubGists.FileData]
        given Decoder[GithubGists.RootInterface] = deriveDecoder[GithubGists.RootInterface]
        summon[Decoder[List[GithubGists.RootInterface]]]

      case "json-generator.json" =>
        given Decoder[JsonGenerator.Friends] = deriveDecoder[JsonGenerator.Friends]
        given Decoder[JsonGenerator.Name] = deriveDecoder[JsonGenerator.Name]
        given Decoder[JsonGenerator.RootInterface] = deriveDecoder[JsonGenerator.RootInterface]
        summon[Decoder[List[JsonGenerator.RootInterface]]]

      case "meteorites.json" =>
        given Decoder[Meteorites.Geolocation] = deriveDecoder[Meteorites.Geolocation]
        given Decoder[Meteorites.RootInterface] = deriveDecoder[Meteorites.RootInterface]
        summon[Decoder[List[Meteorites.RootInterface]]]

      case "movies.json" =>
        given Decoder[Movies.RootInterface] = deriveDecoder[Movies.RootInterface]
        summon[Decoder[List[Movies.RootInterface]]]

      case "reddit-scala.json" =>
        given Decoder[Reddit.Oembed] = deriveDecoder[Reddit.Oembed]
        given Decoder[Reddit.SecureMedia] = deriveDecoder[Reddit.SecureMedia]
        given Decoder[Reddit.MediaEmbed] = deriveDecoder[Reddit.MediaEmbed]
        given Decoder[Reddit.Gildings] = deriveDecoder[Reddit.Gildings]
        given Decoder[Reddit.Data] = deriveDecoder[Reddit.Data]
        given Decoder[Reddit.Child[Reddit.Data]] = deriveDecoder[Reddit.Child[Reddit.Data]]
        given Decoder[Reddit.Data0[Reddit.Data]] = deriveDecoder[Reddit.Data0[Reddit.Data]]
        deriveDecoder[Reddit.RootInterface[Reddit.Data]]

      case "rick-morty.json" =>
        given Decoder[RickMorty.Rating] = deriveDecoder[RickMorty.Rating]
        given Decoder[RickMorty.Schedule] = deriveDecoder[RickMorty.Schedule]
        given Decoder[RickMorty.Country] = deriveDecoder[RickMorty.Country]
        given Decoder[RickMorty.Network] = deriveDecoder[RickMorty.Network]
        given Decoder[RickMorty.Image] = deriveDecoder[RickMorty.Image]
        given Decoder[RickMorty.Externals] = deriveDecoder[RickMorty.Externals]
        given Decoder[RickMorty.Self] = deriveDecoder[RickMorty.Self]
        given Decoder[RickMorty.Links] = deriveDecoder[RickMorty.Links]
        given Decoder[RickMorty.Links1] = deriveDecoder[RickMorty.Links1]
        given Decoder[RickMorty.Episodes] = deriveDecoder[RickMorty.Episodes]
        given Decoder[RickMorty.Embedded] = deriveDecoder[RickMorty.Embedded]
        deriveDecoder[RickMorty.RootInterface]

      case "temp-anomaly.json" =>
        given Decoder[TempAnomaly.Description] = deriveDecoder[TempAnomaly.Description]
        deriveDecoder[TempAnomaly.RootInterface]

      case "thai-cinemas.json" =>
        given Decoder[ThaiCinemas.Group] = deriveDecoder[ThaiCinemas.Group]
        given Decoder[ThaiCinemas.Results] = deriveDecoder[ThaiCinemas.Results]
        deriveDecoder[ThaiCinemas.RootInterface]

      case "turkish.json" =>
        given Decoder[Turkish.Event] = deriveDecoder[Turkish.Event]
        given Decoder[Turkish.Result] = deriveDecoder[Turkish.Result]
        deriveDecoder[Turkish.RootInterface]

      case "twitter_api_compact_response.json" | "twitter_api_response.json" =>
        given Decoder[TwitterApiResponse.Urls] = deriveDecoder[TwitterApiResponse.Urls]
        given Decoder[TwitterApiResponse.Url] = deriveDecoder[TwitterApiResponse.Url]
        given Decoder[TwitterApiResponse.UserMentions] = deriveDecoder[TwitterApiResponse.UserMentions]
        given Decoder[TwitterApiResponse.Entities] = deriveDecoder[TwitterApiResponse.Entities]
        given Decoder[TwitterApiResponse.Entities1] = deriveDecoder[TwitterApiResponse.Entities1]
        given Decoder[TwitterApiResponse.User] = deriveDecoder[TwitterApiResponse.User]
        given Decoder[TwitterApiResponse.RetweetedStatus] = deriveDecoder[TwitterApiResponse.RetweetedStatus]
        given Decoder[TwitterApiResponse.RootInterface] = deriveDecoder[TwitterApiResponse.RootInterface]
        summon[Decoder[List[TwitterApiResponse.RootInterface]]]
    c.asInstanceOf[Decoder[Product]]
    // format: ON

  def setup(): Unit =
    root = decode[Product](new String(fileBytes, UTF_8))(using decoder) match
      case Right(x) => x
      case Left(e)  => throw e

  @Benchmark
  def encodeModel: Array[Byte] = encoder(root).noSpaces.getBytes(UTF_8)

  @Benchmark
  def decodeModel: Product =
    // first decoding into a string and then parsing appears to be faster than directly parsing from the bytes
    decode[Product](new String(fileBytes, UTF_8))(using decoder).getOrElse(throwError())
