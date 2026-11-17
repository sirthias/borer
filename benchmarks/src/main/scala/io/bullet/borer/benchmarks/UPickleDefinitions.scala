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
import org.openjdk.jmh.annotations.*
import upickle.default.*

import scala.compiletime.uninitialized

object UPickleCodecs:
  given ReadWriter[Foo] = macroRW

  val foosWriter: Writer[Map[String, Foo]] = summon[Writer[Map[String, Foo]]]
  val intsWriter: Writer[List[Int]]        = summon[Writer[List[Int]]]

  val foosReader: Reader[Map[String, Foo]] = summon[Reader[Map[String, Foo]]]
  val intsReader: Reader[List[Int]]        = summon[Reader[List[Int]]]

import io.bullet.borer.benchmarks.UPickleCodecs.*

class UPickleEncodingBenchmark extends EncodingBenchmark:

  @Benchmark
  def encodeFoos: Array[Byte] = write(foos)(using foosWriter).getBytes(UTF_8)

  @Benchmark
  def encodeInts: Array[Byte] = write(ints)(using intsWriter).getBytes(UTF_8)

  @Benchmark
  def encodeEmptyArray: Array[Byte] = write(List.empty[Int])(using intsWriter).getBytes(UTF_8)

class UPickleDecodingBenchmark extends DecodingBenchmark:

  @Benchmark
  def decodeFoos: Map[String, Foo] = read[Map[String, Foo]](new String(foosJson, UTF_8))(using foosReader)

  @Benchmark
  def decodeInts: List[Int] = read[List[Int]](new String(intsJson, UTF_8))(using intsReader)

  @Benchmark
  def decodeEmptyArray: List[Int] = read[List[Int]](new String(emptyArrayJson, UTF_8))(using intsReader)

class UPickleDomBenchmark extends DomBenchmark:

  private var root: ujson.Value = uninitialized
  def setup(): Unit             = root = read[ujson.Value](new String(fileBytes, UTF_8))

  @Benchmark
  def encodeDom: Array[Byte] = write(root).getBytes(UTF_8)

  @Benchmark
  def decodeDom: ujson.Value = read[ujson.Value](new String(fileBytes, UTF_8))

class UPickleModelBenchmark extends DomBenchmark:

  private var root: Product = uninitialized

  given nullableWriter[T: Writer]: Writer[Nullable[T]] = summon[Writer[T]].comap(x => x)

  given nullableReader[T: {Reader, Default}]: Reader[Nullable[T]] =
    summon[Reader[T]].mapNulls(x => Nullable(if (x == null) Default.of[T] else x))

  // format: OFF
  private lazy val codec: ReadWriter[Product] =
    val c = fileName match
      case "australia-abc.json" =>
        given ReadWriter[Australia.Geometry] = macroRW[Australia.Geometry]
        given ReadWriter[Australia.Properties] = macroRW[Australia.Properties]
        given ReadWriter[Australia.Properties1] = macroRW[Australia.Properties1]
        given ReadWriter[Australia.Features] = macroRW[Australia.Features]
        given ReadWriter[Australia.Crs] = macroRW[Australia.Crs]
        macroRW[Australia.RootInterface]

      case "bitcoin.json" =>
        given ReadWriter[Bitcoin.SpendingOutpoints] = macroRW[Bitcoin.SpendingOutpoints]
        given ReadWriter[Bitcoin.PrevOut] = macroRW[Bitcoin.PrevOut]
        given ReadWriter[Bitcoin.Inputs] = macroRW[Bitcoin.Inputs]
        given ReadWriter[Bitcoin.Out] = macroRW[Bitcoin.Out]
        given ReadWriter[Bitcoin.Txs] = macroRW[Bitcoin.Txs]
        macroRW[Bitcoin.RootInterface]

      case "doj-blog.json" =>
        given ReadWriter[DojBlog.ResponseInfo] = macroRW[DojBlog.ResponseInfo]
        given ReadWriter[DojBlog.Resultset] = macroRW[DojBlog.Resultset]
        given ReadWriter[DojBlog.Metadata] = macroRW[DojBlog.Metadata]
        given ReadWriter[DojBlog.Component] = macroRW[DojBlog.Component]
        given ReadWriter[DojBlog.Results] = macroRW[DojBlog.Results]
        macroRW[DojBlog.RootInterface]

      case "eu-lobby-country.json" =>
        given ReadWriter[EuLobbyCountry.Facets] = macroRW[EuLobbyCountry.Facets]
        given ReadWriter[EuLobbyCountry.Results] = macroRW[EuLobbyCountry.Results]
        macroRW[EuLobbyCountry.RootInterface]

      case "eu-lobby-financial.json" =>
        given ReadWriter[EuLobbyFinancial.Facets] = macroRW[EuLobbyFinancial.Facets]
        given ReadWriter[EuLobbyFinancial.CustomIncomes] = macroRW[EuLobbyFinancial.CustomIncomes]
        given ReadWriter[EuLobbyFinancial.Results] = macroRW[EuLobbyFinancial.Results]
        macroRW[EuLobbyFinancial.RootInterface]

      case "eu-lobby-repr.json" =>
        given ReadWriter[EuLobbyRepr.Facets] = macroRW[EuLobbyRepr.Facets]
        given ReadWriter[EuLobbyRepr.Results] = macroRW[EuLobbyRepr.Results]
        macroRW[EuLobbyRepr.RootInterface]

      case "github-events.json" =>
        import GithubEvents.*
        given ReadWriter[Actor] = macroRW[Actor]
        given ReadWriter[Author] = macroRW[Author]
        given ReadWriter[Self] = macroRW[Self]
        given ReadWriter[Owner] = macroRW[Owner]
        given ReadWriter[Org] = macroRW[Org]
        given ReadWriter[Links] = macroRW[Links]
        given ReadWriter[Links1] = macroRW[Links1]
        given ReadWriter[Labels] = macroRW[Labels]
        given ReadWriter[License] = macroRW[License]
        given ReadWriter[Repo] = macroRW[Repo]
        given ReadWriter[Repo1] = macroRW[Repo1]
        given ReadWriter[Comment] = macroRW[Comment]
        given ReadWriter[Comment1] = macroRW[Comment1]
        given ReadWriter[Commits] = macroRW[Commits]
        given ReadWriter[CreateEvent] = macroRW[CreateEvent]
        given ReadWriter[Forkee] = macroRW[Forkee]
        given ReadWriter[ForkEvent[Forkee]] = macroRW[ForkEvent[Forkee]]
        given ReadWriter[Head[Repo1]] = macroRW[Head[Repo1]]
        given ReadWriter[PullRequest] = macroRW[PullRequest]
        given ReadWriter[PullRequest1[Head[Repo1]]] = macroRW[PullRequest1[Head[Repo1]]]
        given ReadWriter[PullRequest2[Head[Repo1]]] = macroRW[PullRequest2[Head[Repo1]]]
        given ReadWriter[PullRequestEvent[Head[Repo1]]] = macroRW[PullRequestEvent[Head[Repo1]]]
        given ReadWriter[PullRequestReviewCommentEvent[Head[Repo1]]] = macroRW[PullRequestReviewCommentEvent[Head[Repo1]]]
        given ReadWriter[Issue] = macroRW[Issue]
        given ReadWriter[Issue1] = macroRW[Issue1]
        given ReadWriter[IssueCommentEvent] = macroRW[IssueCommentEvent]
        given ReadWriter[IssuesEvent] = macroRW[IssuesEvent]
        given ReadWriter[PushEvent] = macroRW[PushEvent]
        given ReadWriter[WatchEvent] = macroRW[WatchEvent]
        given ReadWriter[RootInterface[Head[Repo1], Forkee]] = macroRW[RootInterface[Head[Repo1], Forkee]]
        ReadWriter.join[List[RootInterface[Head[Repo1], Forkee]]]

      case "github-gists.json" =>
        given ReadWriter[GithubGists.Owner] = macroRW[GithubGists.Owner]
        given ReadWriter[GithubGists.FileData] = macroRW[GithubGists.FileData]
        given ReadWriter[GithubGists.RootInterface] = macroRW[GithubGists.RootInterface]
        ReadWriter.join[List[GithubGists.RootInterface]]

      case "json-generator.json" =>
        given ReadWriter[JsonGenerator.Friends] = macroRW[JsonGenerator.Friends]
        given ReadWriter[JsonGenerator.Name] = macroRW[JsonGenerator.Name]
        given ReadWriter[JsonGenerator.RootInterface] = macroRW[JsonGenerator.RootInterface]
        ReadWriter.join[List[JsonGenerator.RootInterface]]

      case "meteorites.json" =>
        given ReadWriter[Meteorites.Geolocation] = macroRW[Meteorites.Geolocation]
        given ReadWriter[Meteorites.RootInterface] = macroRW[Meteorites.RootInterface]
        ReadWriter.join[List[Meteorites.RootInterface]]

      case "movies.json" =>
        given ReadWriter[Movies.RootInterface] = macroRW[Movies.RootInterface]
        ReadWriter.join[List[Movies.RootInterface]]

      case "reddit-scala.json" =>
        given ReadWriter[Reddit.Oembed] = macroRW[Reddit.Oembed]
        given ReadWriter[Reddit.SecureMedia] = macroRW[Reddit.SecureMedia]
        given ReadWriter[Reddit.MediaEmbed] = macroRW[Reddit.MediaEmbed]
        given ReadWriter[Reddit.Gildings] = macroRW[Reddit.Gildings]
        given ReadWriter[Reddit.Data] = macroRW[Reddit.Data]
        given ReadWriter[Reddit.Child[Reddit.Data]] = macroRW[Reddit.Child[Reddit.Data]]
        given ReadWriter[Reddit.Data0[Reddit.Data]] = macroRW[Reddit.Data0[Reddit.Data]]
        macroRW[Reddit.RootInterface[Reddit.Data]]

      case "rick-morty.json" =>
        given ReadWriter[RickMorty.Rating] = macroRW[RickMorty.Rating]
        given ReadWriter[RickMorty.Schedule] = macroRW[RickMorty.Schedule]
        given ReadWriter[RickMorty.Country] = macroRW[RickMorty.Country]
        given ReadWriter[RickMorty.Network] = macroRW[RickMorty.Network]
        given ReadWriter[RickMorty.Image] = macroRW[RickMorty.Image]
        given ReadWriter[RickMorty.Externals] = macroRW[RickMorty.Externals]
        given ReadWriter[RickMorty.Self] = macroRW[RickMorty.Self]
        given ReadWriter[RickMorty.Links] = macroRW[RickMorty.Links]
        given ReadWriter[RickMorty.Links1] = macroRW[RickMorty.Links1]
        given ReadWriter[RickMorty.Episodes] = macroRW[RickMorty.Episodes]
        given ReadWriter[RickMorty.Embedded] = macroRW[RickMorty.Embedded]
        macroRW[RickMorty.RootInterface]

      case "temp-anomaly.json" =>
        given ReadWriter[TempAnomaly.Description] = macroRW[TempAnomaly.Description]
        macroRW[TempAnomaly.RootInterface]

      case "thai-cinemas.json" =>
        given ReadWriter[ThaiCinemas.Group] = macroRW[ThaiCinemas.Group]
        given ReadWriter[ThaiCinemas.Results] = macroRW[ThaiCinemas.Results]
        macroRW[ThaiCinemas.RootInterface]

      case "turkish.json" =>
        given ReadWriter[Turkish.Event] = macroRW[Turkish.Event]
        given ReadWriter[Turkish.Result] = macroRW[Turkish.Result]
        macroRW[Turkish.RootInterface]

      case "twitter_api_compact_response.json" | "twitter_api_response.json" =>
        given ReadWriter[TwitterApiResponse.Urls] = macroRW[TwitterApiResponse.Urls]
        given ReadWriter[TwitterApiResponse.Url] = macroRW[TwitterApiResponse.Url]
        given ReadWriter[TwitterApiResponse.UserMentions] = macroRW[TwitterApiResponse.UserMentions]
        given ReadWriter[TwitterApiResponse.Entities] = macroRW[TwitterApiResponse.Entities]
        given ReadWriter[TwitterApiResponse.Entities1] = macroRW[TwitterApiResponse.Entities1]
        given ReadWriter[TwitterApiResponse.User] = macroRW[TwitterApiResponse.User]
        given ReadWriter[TwitterApiResponse.RetweetedStatus] = macroRW[TwitterApiResponse.RetweetedStatus]
        given ReadWriter[TwitterApiResponse.RootInterface] = macroRW[TwitterApiResponse.RootInterface]
        ReadWriter.join[List[TwitterApiResponse.RootInterface]]
    c.asInstanceOf[ReadWriter[Product]]
  // format: ON

  private given ReadWriter[Product] = codec

  def setup(): Unit = root = read[Product](fileBytes)

  @Benchmark
  def encodeModel: Array[Byte] = write(root).getBytes(UTF_8)

  @Benchmark
  def decodeModel: Product = read[Product](fileBytes)
