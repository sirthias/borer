/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.benchmarks

import io.bullet.borer._
import org.openjdk.jmh.annotations._
import io.bullet.borer.derivation.MapBasedCodecs._

object BorerCodecs {

  implicit val intsEncoder = implicitly[Encoder[List[Int]]]
  implicit val intsDecoder = implicitly[Decoder[List[Int]]]

  object Derived {
    implicit val fooCodec    = deriveCodec[Foo]
    implicit val foosEncoder = implicitly[Encoder[Map[String, Foo]]]
    implicit val foosDecoder = implicitly[Decoder[Map[String, Foo]]]
  }

  object Manual {
    implicit val fooCodec = Codec[Foo](
      encoder = (w, x) ⇒ {
        w.writeMapStart()
          .writeString("string")
          .writeString(x.string)
          .writeString("double")
          .writeDouble(x.double)
          .writeString("int")
          .writeInt(x.int)
          .writeString("long")
          .writeLong(x.long)
        x.listOfBools.foldLeft(w.writeString("listOfBools").writeArrayStart())(_ writeBool _).writeBreak().writeBreak()
      },
      decoder = { r ⇒
        r.readMapStart()
        val foo = Foo(
          r.readString("string").readString(),
          r.readString("double").readDouble(),
          r.readString("int").readInt(),
          r.readString("long").readLong(),
          r.readString("listOfBools").read[List[Boolean]]()
        )
        r.readBreak()
        foo
      }
    )
    implicit val foosEncoder = implicitly[Encoder[Map[String, Foo]]]
    implicit val foosDecoder = implicitly[Decoder[Map[String, Foo]]]
  }
}

import BorerCodecs._

class BorerEncodingBenchmark extends EncodingBenchmark {

  @Benchmark
  def encodeFoosD: Array[Byte] = Json.encode(foos)(Derived.foosEncoder).toByteArray

  @Benchmark
  def encodeFoosM: Array[Byte] = Json.encode(foos)(Manual.foosEncoder).toByteArray

  @Benchmark
  def encodeInts: Array[Byte] = Json.encode(ints).toByteArray

  @Benchmark
  def encodeEmptyArray: Array[Byte] = Json.encode(List.empty[Int]).toByteArray
}

class BorerDecodingBenchmark extends DecodingBenchmark {

  @Benchmark
  def decodeFoosD: Map[String, Foo] = Json.decode(foosJson).to[Map[String, Foo]](Derived.foosDecoder).value

  @Benchmark
  def decodeFoosM: Map[String, Foo] = Json.decode(foosJson).to[Map[String, Foo]](Manual.foosDecoder).value

  @Benchmark
  def decodeInts: List[Int] = Json.decode(intsJson).to[List[Int]].value

  @Benchmark
  def decodeEmptyArray: List[Int] = Json.decode(emptyArrayJson).to[List[Int]].value
}

class BorerDomBenchmark extends DomBenchmark {

  private var root: Dom.Element = _
  def setup(): Unit             = root = Json.decode(fileBytes).to[Dom.Element].value

  @Benchmark
  def encodeDom: Array[Byte] = Json.encode(root).toByteArray

  @Benchmark
  def decodeDom: Dom.Element = Json.decode(fileBytes).to[Dom.Element].value
}

class BorerModelBenchmark extends DomBenchmark {

  private var root: Product = _

  // format: OFF
  implicit lazy val codec: Codec[Product] = {
    val c = fileName match {
      case "australia-abc.json" ⇒
        implicit val a = deriveCodec[Australia.Geometry]
        implicit val b = deriveCodec[Australia.Properties]
        implicit val c = deriveCodec[Australia.Properties1]
        implicit val d = deriveCodec[Australia.Features]
        implicit val e = deriveCodec[Australia.Crs]
        deriveCodec[Australia.RootInterface]

      case "bitcoin.json" ⇒
        implicit val a = deriveCodec[Bitcoin.SpendingOutpoints]
        implicit val b = deriveCodec[Bitcoin.PrevOut]
        implicit val c = deriveCodec[Bitcoin.Inputs]
        implicit val d = deriveCodec[Bitcoin.Out]
        implicit val e = deriveCodec[Bitcoin.Txs]
        deriveCodec[Bitcoin.RootInterface]

      case "doj-blog.json" ⇒
        implicit val a = deriveCodec[DojBlog.ResponseInfo]
        implicit val b = deriveCodec[DojBlog.Resultset]
        implicit val c = deriveCodec[DojBlog.Metadata]
        implicit val d = deriveCodec[DojBlog.Component]
        implicit val e = deriveCodec[DojBlog.Results]
        deriveCodec[DojBlog.RootInterface]

      case "eu-lobby-country.json" ⇒
        implicit val a = deriveCodec[EuLobbyCountry.Facets]
        implicit val b = deriveCodec[EuLobbyCountry.Results]
        deriveCodec[EuLobbyCountry.RootInterface]

      case "eu-lobby-financial.json" ⇒
        implicit val a = deriveCodec[EuLobbyFinancial.Facets]
        implicit val b = deriveCodec[EuLobbyFinancial.CustomIncomes]
        implicit val c = deriveCodec[EuLobbyFinancial.Results]
        deriveCodec[EuLobbyFinancial.RootInterface]

      case "eu-lobby-repr.json" ⇒
        implicit val a = deriveCodec[EuLobbyRepr.Facets]
        implicit val b = deriveCodec[EuLobbyRepr.Results]
        deriveCodec[EuLobbyRepr.RootInterface]

      case "github-events.json" ⇒
        implicit val a = deriveCodec[GithubEvents.Actor]
        implicit val b = deriveCodec[GithubEvents.Author]
        implicit val c = deriveCodec[GithubEvents.Self]
        implicit val d = deriveCodec[GithubEvents.Owner]
        implicit val e = deriveCodec[GithubEvents.Org]
        implicit val f = deriveCodec[GithubEvents.Links]
        implicit val g = deriveCodec[GithubEvents.Links1]
        implicit val h = deriveCodec[GithubEvents.Labels]
        implicit val i = deriveCodec[GithubEvents.License]
        implicit val j = deriveCodec[GithubEvents.Repo]
        implicit val k = deriveCodec[GithubEvents.Repo1]
        implicit val l = deriveCodec[GithubEvents.Comment]
        implicit val m = deriveCodec[GithubEvents.Comment1]
        implicit val n = deriveCodec[GithubEvents.Commits]
        implicit val o = deriveCodec[GithubEvents.CreateEvent]
        implicit val p = deriveCodec[GithubEvents.Forkee]
        implicit val q = deriveCodec[GithubEvents.ForkEvent]
        implicit val r = deriveCodec[GithubEvents.Head]
        implicit val s = deriveCodec[GithubEvents.PullRequest]
        implicit val t = deriveCodec[GithubEvents.PullRequest1]
        implicit val u = deriveCodec[GithubEvents.PullRequest2]
        implicit val v = deriveCodec[GithubEvents.PullRequestEvent]
        implicit val w = deriveCodec[GithubEvents.PullRequestReviewCommentEvent]
        implicit val x = deriveCodec[GithubEvents.Issue]
        implicit val y = deriveCodec[GithubEvents.Issue1]
        implicit val z = deriveCodec[GithubEvents.IssueCommentEvent]
        implicit val A = deriveCodec[GithubEvents.IssuesEvent]
        implicit val B = deriveCodec[GithubEvents.PushEvent]
        implicit val C = deriveCodec[GithubEvents.WatchEvent]
        implicit val D = deriveCodec[GithubEvents.RootInterface]
        Codec.implicitly[List[GithubEvents.RootInterface]]

      case "github-gists.json" ⇒
        implicit val a = deriveCodec[GithubGists.Owner]
        implicit val b = deriveCodec[GithubGists.FileData]
        implicit val c = deriveCodec[GithubGists.RootInterface]
        Codec.implicitly[List[GithubGists.RootInterface]]

      case "json-generator.json" ⇒
        implicit val a = deriveCodec[JsonGenerator.Friends]
        implicit val b = deriveCodec[JsonGenerator.Name]
        implicit val c = deriveCodec[JsonGenerator.RootInterface]
        Codec.implicitly[List[JsonGenerator.RootInterface]]

      case "meteorites.json" ⇒
        implicit val a = deriveCodec[Meteorites.Geolocation]
        implicit val b = deriveCodec[Meteorites.RootInterface]
        Codec.implicitly[List[Meteorites.RootInterface]]

      case "movies.json" ⇒
        implicit val a = deriveCodec[Movies.RootInterface]
        Codec.implicitly[List[Movies.RootInterface]]

      case "reddit-scala.json" ⇒
        implicit val a = deriveCodec[Reddit.Oembed]
        implicit val b = deriveCodec[Reddit.SecureMedia]
        implicit val c = deriveCodec[Reddit.MediaEmbed]
        implicit val d = deriveCodec[Reddit.Gildings]
        implicit val e = deriveCodec[Reddit.Data]
        implicit val f = deriveCodec[Reddit.Child]
        implicit val g = deriveCodec[Reddit.Data0]
        deriveCodec[Reddit.RootInterface]

      case "rick-morty.json" ⇒
        implicit val a = deriveCodec[RickMorty.Rating]
        implicit val b = deriveCodec[RickMorty.Schedule]
        implicit val c = deriveCodec[RickMorty.Country]
        implicit val d = deriveCodec[RickMorty.Network]
        implicit val e = deriveCodec[RickMorty.Image]
        implicit val f = deriveCodec[RickMorty.Externals]
        implicit val g = deriveCodec[RickMorty.Self]
        implicit val h = deriveCodec[RickMorty.Links]
        implicit val i = deriveCodec[RickMorty.Links1]
        implicit val j = deriveCodec[RickMorty.Episodes]
        implicit val k = deriveCodec[RickMorty.Embedded]
        deriveCodec[RickMorty.RootInterface]

      case "temp-anomaly.json" ⇒
        implicit val a = deriveCodec[TempAnomaly.Description]
        deriveCodec[TempAnomaly.RootInterface]

      case "thai-cinemas.json" ⇒
        implicit val a = deriveCodec[ThaiCinemas.Group]
        implicit val b = deriveCodec[ThaiCinemas.Results]
        deriveCodec[ThaiCinemas.RootInterface]

      case "turkish.json" ⇒
        implicit val a = deriveCodec[Turkish.Event]
        implicit val b = deriveCodec[Turkish.Result]
        deriveCodec[Turkish.RootInterface]

      case "twitter_api_compact_response.json" | "twitter_api_response.json" ⇒
        implicit val a = deriveCodec[TwitterApiResponse.Urls]
        implicit val b = deriveCodec[TwitterApiResponse.Url]
        implicit val c = deriveCodec[TwitterApiResponse.UserMentions]
        implicit val d = deriveCodec[TwitterApiResponse.Entities]
        implicit val e = deriveCodec[TwitterApiResponse.Entities1]
        implicit val f = deriveCodec[TwitterApiResponse.User]
        implicit val g = deriveCodec[TwitterApiResponse.RetweetedStatus]
        implicit val h = deriveCodec[TwitterApiResponse.RootInterface]
        Codec.implicitly[List[TwitterApiResponse.RootInterface]]
    }
    c.asInstanceOf[Codec[Product]]
  }
  // format: ON

  def setup(): Unit = root = Json.decode(fileBytes).to[Product].value

  @Benchmark
  def encodeModel: Array[Byte] = Json.encode(root).toByteArray

  @Benchmark
  def decodeModel: Product = Json.decode(fileBytes).to[Product].value
}
