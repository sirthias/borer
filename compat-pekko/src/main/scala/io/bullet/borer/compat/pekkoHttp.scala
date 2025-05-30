/*
 * Copyright (c) 2019-2024 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.compat

import scala.concurrent.Future
import scala.reflect.ClassTag
import _root_.org.apache.pekko.http.scaladsl.marshalling.{Marshaller, ToEntityMarshaller}
import _root_.org.apache.pekko.http.scaladsl.unmarshalling.{
  FromEntityUnmarshaller,
  FromMessageUnmarshaller,
  Unmarshaller
}
import _root_.org.apache.pekko.http.scaladsl.common.EntityStreamingSupport
import _root_.org.apache.pekko.http.scaladsl.marshalling._
import _root_.org.apache.pekko.http.scaladsl.model._
import _root_.org.apache.pekko.http.scaladsl.util.FastFuture
import _root_.org.apache.pekko.http.scaladsl.util.FastFuture._
import _root_.org.apache.pekko.stream.scaladsl.{Flow, Keep, Source}
import _root_.org.apache.pekko.util.ByteString
import _root_.org.apache.pekko.NotUsed
import io.bullet.borer._

trait PekkoHttpCompat {

  // brevity aliases
  type CborDecodingSetup = DecodingSetup.Api[Cbor.DecodingConfig]
  type JsonDecodingSetup = DecodingSetup.Api[Json.DecodingConfig]

  /**
   * Override with a parameterized call to the `borerUnmarshaller` method to apply a custom configuration.
   */
  implicit def borerFromEntityUnmarshaller[T: Decoder]: FromEntityUnmarshaller[T] = borerUnmarshaller()

  implicit final def borerFromMessageUnmarshaller[T: Decoder]: FromMessageUnmarshaller[T] =
    Unmarshaller.messageUnmarshallerFromEntityUnmarshaller(borerFromEntityUnmarshaller)

  /**
   * Provides a [[FromEntityUnmarshaller]] for [[T]] given an implicit borer Decoder for [[T]].
   * Supports both CBOR and JSON with the given MediaTypes.
   */
  final def borerUnmarshaller[T: Decoder](
      cborMediaType: MediaType = MediaTypes.`application/cbor`,
      jsonMediaType: MediaType = MediaTypes.`application/json`,
      configureCbor: CborDecodingSetup => CborDecodingSetup = identity,
      configureJson: JsonDecodingSetup => JsonDecodingSetup = identity
  ): FromEntityUnmarshaller[T] =
    Unmarshaller.withMaterializer { implicit ec => implicit mat => httpEntity =>
      pekkoHttp.byteArrayUnmarshaller(httpEntity).fast.flatMap { bytes =>
        if (bytes.length > 0) {
          httpEntity.contentType.mediaType match {
            case `cborMediaType` => FastFuture(configureCbor(Cbor.decode(bytes)).to[T].valueTry)
            case `jsonMediaType` => FastFuture(configureJson(Json.decode(bytes)).to[T].valueTry)
            case _ => FastFuture.failed(Unmarshaller.UnsupportedContentTypeException(cborMediaType, jsonMediaType))
          }
        } else FastFuture.failed(Unmarshaller.NoContentException)
      }
    }

  /**
   * Provides a [[FromEntityUnmarshaller]] for [[T]] given an implicit borer Decoder for [[T]].
   * Supports only CBOR with the given MediaType.
   */
  final def borerCborUnmarshaller[T: Decoder](
      cborMediaType: MediaType = MediaTypes.`application/cbor`,
      configureCbor: CborDecodingSetup => CborDecodingSetup = identity): FromEntityUnmarshaller[T] =
    Unmarshaller.withMaterializer { implicit ec => implicit mat => httpEntity =>
      pekkoHttp.byteArrayUnmarshaller(httpEntity).fast.flatMap { bytes =>
        if (bytes.length > 0) {
          httpEntity.contentType.mediaType match {
            case `cborMediaType` => FastFuture(configureCbor(Cbor.decode(bytes)).to[T].valueTry)
            case _               => FastFuture.failed(Unmarshaller.UnsupportedContentTypeException(cborMediaType))
          }
        } else FastFuture.failed(Unmarshaller.NoContentException)
      }
    }

  /**
   * Provides a [[FromEntityUnmarshaller]] for [[T]] given an implicit borer Decoder for [[T]].
   * Supports only JSON with the given MediaType.
   */
  final def borerJsonUnmarshaller[T: Decoder](
      jsonMediaType: MediaType = MediaTypes.`application/json`,
      configureJson: JsonDecodingSetup => JsonDecodingSetup = identity
  ): FromEntityUnmarshaller[T] =
    Unmarshaller.withMaterializer { implicit ec => implicit mat => httpEntity =>
      pekkoHttp.byteArrayUnmarshaller(httpEntity).fast.flatMap { bytes =>
        if (bytes.length > 0) {
          httpEntity.contentType.mediaType match {
            case `jsonMediaType` => FastFuture(configureJson(Json.decode(bytes)).to[T].valueTry)
            case _               => FastFuture.failed(Unmarshaller.UnsupportedContentTypeException(jsonMediaType))
          }
        } else FastFuture.failed(Unmarshaller.NoContentException)
      }
    }

  // brevity aliases
  type CborEncodingSetup = EncodingSetup.Api[Cbor.EncodingConfig]
  type JsonEncodingSetup = EncodingSetup.Api[Json.EncodingConfig]

  /**
   * Override with a parameterized call to the `borerMarshaller` method to apply a custom configuration.
   */
  implicit def borerToEntityMarshaller[T: Encoder]: ToEntityMarshaller[T] = borerMarshaller()

  implicit final def borerToResponseMarshaller[T: Encoder]: ToResponseMarshaller[T] =
    Marshaller.liftMarshaller(borerToEntityMarshaller)

  /**
   * Provides a [[ToEntityMarshaller]] for [[T]] given an implicit borer Encoder for [[T]].
   * Supports both CBOR and JSON with the given MediaTypes.
   * Content negotiation will determine, whether CBOR or JSON is produced.
   * If the client accepts both formats with equal q-value the marshaller will produce
   * whatever the `prefer` parameter specifies.
   */
  final def borerMarshaller[T: Encoder](
      cborContentType: ContentType = MediaTypes.`application/cbor`,
      jsonContentType: ContentType = MediaTypes.`application/json`,
      configureCbor: CborEncodingSetup => CborEncodingSetup = identity,
      configureJson: JsonEncodingSetup => JsonEncodingSetup = identity,
      prefer: Target = Json
  ): ToEntityMarshaller[T] = {
    val cborMarshaller = borerCborMarshaller(cborContentType, configureCbor)
    val jsonMarshaller = borerJsonMarshaller(jsonContentType, configureJson)
    if (prefer == Cbor)
      Marshaller.oneOf(cborMarshaller, jsonMarshaller)
    else
      Marshaller.oneOf(jsonMarshaller, cborMarshaller)
  }

  /**
   * Provides a [[ToEntityMarshaller]] for [[T]] given an implicit borer Encoder for [[T]].
   * Supports only CBOR with the given MediaType.
   */
  final def borerCborMarshaller[T: Encoder](
      cborContentType: ContentType = MediaTypes.`application/cbor`,
      configureCbor: CborEncodingSetup => CborEncodingSetup = identity): ToEntityMarshaller[T] =
    Marshaller.byteArrayMarshaller(cborContentType).compose[T] { value =>
      configureCbor(Cbor.encode(value)).toByteArray
    }

  /**
   * Provides a [[ToEntityMarshaller]] for [[T]] given an implicit borer Encoder for [[T]].
   * Supports only JSON with the given MediaType.
   */
  final def borerJsonMarshaller[T: Encoder](
      jsonContentType: ContentType = MediaTypes.`application/json`,
      configureJson: JsonEncodingSetup => JsonEncodingSetup = identity
  ): ToEntityMarshaller[T] =
    Marshaller.byteArrayMarshaller(jsonContentType).compose[T] { value =>
      configureJson(Json.encode(value)).toByteArray
    }

  /**
   * Override with a parameterized call to the `borerStreamUnmarshaller` method to apply a custom configuration.
   */
  implicit def borerJsonStreamFromEntityUnmarshaller[T: Decoder]: FromEntityUnmarshaller[Source[T, NotUsed]] =
    borerStreamUnmarshaller(EntityStreamingSupport.json())

  implicit final def borerJsonStreamFromMessageUnmarshaller[T: Decoder]: FromMessageUnmarshaller[Source[T, NotUsed]] =
    Unmarshaller.messageUnmarshallerFromEntityUnmarshaller(borerJsonStreamFromEntityUnmarshaller)

  /**
   * Provides a [[FromEntityUnmarshaller]] which produces streams of [[T]] given an implicit borer Decoder
   * for [[T]]. Supports JSON or CSV, depending on the given [[EntityStreamingSupport]].
   *
   * @see https://doc.pekko.io/api/pekko-http/10.1.9/pekko/http/scaladsl/common/EntityStreamingSupport.html
   */
  final def borerStreamUnmarshaller[T: Decoder](
      ess: EntityStreamingSupport): FromEntityUnmarshaller[Source[T, NotUsed]] =
    Unmarshaller.withMaterializer { implicit ec => implicit mat =>
      val unmarshalSync     = (byteString: ByteString) => Json.decode(byteString.toArray[Byte]).to[T].value
      val unmarshallingFlow =
        if (ess.parallelism > 1) {
          val unmarshalAsync = (byteString: ByteString) => Future(unmarshalSync(byteString))
          if (ess.unordered) Flow[ByteString].mapAsyncUnordered(ess.parallelism)(unmarshalAsync)
          else Flow[ByteString].mapAsync(ess.parallelism)(unmarshalAsync)
        } else Flow[ByteString].map(unmarshalSync)

      httpEntity => {
        if (ess.supported matches httpEntity.contentType) {
          val frames = httpEntity.dataBytes.via(ess.framingDecoder)
          FastFuture.successful(frames.viaMat(unmarshallingFlow)(Keep.right))
        } else FastFuture.failed(Unmarshaller.UnsupportedContentTypeException(ess.supported))
      }
    }

  /**
   * Override with a parameterized call to the `borerStreamMarshaller` method to apply a custom configuration.
   */
  implicit def borerJsonStreamToEntityMarshaller[T: ToEntityMarshaller: ClassTag]
      : ToEntityMarshaller[Source[T, NotUsed]] =
    borerStreamMarshaller[T](EntityStreamingSupport.json())

  implicit final def borerJsonStreamToResponseMarshaller[T: ToEntityMarshaller: ClassTag]
      : ToResponseMarshaller[Source[T, NotUsed]] =
    Marshaller.liftMarshaller(borerJsonStreamToEntityMarshaller)

  /**
   * Provides a [[ToEntityMarshaller]] for streams of [[T]] given an implicit borer Encoder for [[T]].
   * Supports JSON or CSV, depending on the given [[EntityStreamingSupport]].
   */
  final def borerStreamMarshaller[T](ess: EntityStreamingSupport)(
      implicit marshaller: ToEntityMarshaller[T],
      classTag: ClassTag[T]): ToEntityMarshaller[Source[T, NotUsed]] = {

    type Marshallings = List[Marshalling[MessageEntity]]
    val contentType                                                    = ess.contentType
    val selectMarshalling: Marshallings => Option[() => MessageEntity] =
      contentType match {
        case _: ContentType.Binary | _: ContentType.WithFixedCharset | _: ContentType.WithMissingCharset =>
          (_: Marshallings).collectFirst { case Marshalling.WithFixedContentType(`contentType`, marshal) => marshal }
        case ContentType.WithCharset(mediaType, charset) =>
          (_: Marshallings).collectFirst {
            case Marshalling.WithFixedContentType(`contentType`, marshal) => marshal
            case Marshalling.WithOpenCharset(`mediaType`, marshal)        => () => marshal(charset)
          }
      }

    Marshaller[Source[T, NotUsed], MessageEntity] { implicit ec => source =>
      FastFuture successful {
        Marshalling.WithFixedContentType(
          contentType,
          () => {
            val byteStream =
              source
                .mapAsync(1)(value => marshaller(value)(ec))
                .map { marshallings =>
                  selectMarshalling(marshallings)
                    .orElse(marshallings collectFirst { case Marshalling.Opaque(x) => x })
                    .getOrElse(
                      throw new NoStrictlyCompatibleElementMarshallingAvailableException[T](contentType, marshallings))
                }
                .flatMapConcat(_.apply().dataBytes) // marshal!
                .via(ess.framingRenderer)

            HttpEntity(contentType, byteStream)
          }) :: Nil
      }
    }
  }
}

/**
 * Automatic to and from CBOR and/or JSON marshalling/unmarshalling using in-scope borer Encoders / Decoders.
 */
object pekkoHttp extends PekkoHttpCompat {

  private[compat] val byteArrayUnmarshaller: FromEntityUnmarshaller[Array[Byte]] =
    Unmarshaller.byteArrayUnmarshaller
}
