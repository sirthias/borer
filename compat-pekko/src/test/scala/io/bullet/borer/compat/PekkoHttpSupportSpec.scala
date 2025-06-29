/*
 * Copyright (c) 2019-2024 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.compat

import _root_.org.apache.pekko.actor.ActorSystem
import _root_.org.apache.pekko.http.scaladsl.marshalling.{Marshal, ToEntityMarshaller}
import _root_.org.apache.pekko.http.scaladsl.model._
import _root_.org.apache.pekko.http.scaladsl.unmarshalling.{Unmarshal, Unmarshaller}
import _root_.org.apache.pekko.http.scaladsl.unmarshalling.Unmarshaller.UnsupportedContentTypeException
import _root_.org.apache.pekko.stream.scaladsl.{Sink, Source}
import _root_.org.apache.pekko.util.ByteString
import _root_.org.apache.pekko.NotUsed
import _root_.org.apache.pekko.http.scaladsl.model.MediaTypes.{`application/cbor`, `application/json`}
import io.bullet.borer.{BorerSuite, Cbor, Codec, Encoder}
import io.bullet.borer.derivation.MapBasedCodecs

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt

/**
 * More or less direct adaptation of
 * https://github.com/hseeberger/akka-http-json/blob/7a700166e962ec77cdea1a222e219ec78ca1175c/akka-http-jsoniter-scala/src/test/scala/de/heikoseeberger/akkahttpjsoniterscala/JsoniterScalaSupportSpec.scala
 * which is copyright by Heiko Seeberger and licensed under Apache License, Version 2.0.
 */
class PekkoHttpSupportSpec extends BorerSuite {
  import pekkoHttp._

  final case class Foo(bar: String) {
    require(bar == "bar", "bar must be 'bar'!")
  }

  final case class Num(x: Int)

  implicit private val system: ActorSystem = ActorSystem()
  import system.dispatcher

  implicit private val fooCodec: Codec[Foo] = MapBasedCodecs.deriveCodec[Foo]
  implicit private val numCodec: Codec[Num] = MapBasedCodecs.deriveCodec[Num]

  test("marshalling and unmarshalling (CBOR)") {
    implicit def borerToEntityMarshaller[T: Encoder]: ToEntityMarshaller[T] = borerMarshaller(prefer = Cbor)

    val foo = Foo("bar")
    Marshal(foo)
      .to[RequestEntity]
      .map { entity =>
        entity.contentType ==> ContentType(`application/cbor`)
        entity
      }
      .flatMap(Unmarshal(_).to[Foo])
      .map(_ ==> foo)
  }

  test("marshalling and unmarshalling (JSON)") {
    val foo = Foo("bar")
    Marshal(foo)
      .to[RequestEntity]
      .map { entity =>
        entity.contentType ==> ContentType(`application/json`)
        entity
      }
      .flatMap(Unmarshal(_).to[Foo])
      .map(_ ==> foo)
  }

  test("prefer JSON on equal q-value (by default)") {
    val foo          = Foo("bar")
    val acceptHeader = _root_.org.apache.pekko.http.scaladsl.model.headers.Accept(
      MediaRange.One(`application/json`, 1.0f),
      MediaRanges.`*/*`
    )
    Marshal(foo)
      .toResponseFor(HttpRequest(headers = acceptHeader :: Nil))
      .flatMap(_.entity.toStrict(1.second))
      .map(_ ==> HttpEntity.Strict(`application/json`, ByteString("""{"bar":"bar"}""")))
  }

  test("surface error message for requirement errors") {
    val entity = HttpEntity(`application/json`, """{ "bar": "baz" }""")
    Unmarshal(entity)
      .to[Foo]
      .failed
      .map(_.getMessage ==> "java.lang.IllegalArgumentException: requirement failed: bar must be 'bar'! (input position 15)")
  }

  test("fail with NoContentException when unmarshalling empty entities") {
    val entity = HttpEntity.empty(ContentTypes.`application/json`)
    Unmarshal(entity)
      .to[Foo]
      .failed
      .map(_ ==> Unmarshaller.NoContentException)
  }

  test("fail with UnsupportedContentTypeException when Content-Type is not `application/json`") {
    val entity = HttpEntity("""{ "bar": "bar" }""")
    Unmarshal(entity)
      .to[Foo]
      .failed
      .map(_ ==> UnsupportedContentTypeException(ContentType(`application/cbor`), ContentTypes.`application/json`))
  }

  test("stream unmarshalling (JSON)") {
    val entity = HttpEntity(`application/json`, """[{"x":1},{"x":2},{"x":3}]""")
    Unmarshal(entity)
      .to[Source[Num, NotUsed]]
      .flatMap(_.runWith(Sink.seq))
      .map(_ ==> List(Num(1), Num(2), Num(3)))
  }

  test("stream marshalling (JSON)") {
    val nums         = List(Num(1), Num(2), Num(3))
    val acceptHeader =
      _root_.org.apache.pekko.http.scaladsl.model.headers.Accept(MediaRange.One(`application/json`, 1.0f))
    Marshal(nums)
      .toResponseFor(HttpRequest(headers = acceptHeader :: Nil))
      .flatMap(_.entity.toStrict(1.second))
      .map { entity =>
        entity.contentType ==> ContentTypes.`application/json`
        entity.data.utf8String ==> """[{"x":1},{"x":2},{"x":3}]"""
      }
  }

  override def afterAll(): Unit = Await.ready(system.terminate(), 4.seconds)
}
