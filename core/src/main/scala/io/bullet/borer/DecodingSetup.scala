/*
 * Copyright (c) 2019-2021 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import io.bullet.borer.internal.Parser

import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

object DecodingSetup {

  sealed trait Api[Config <: Borer.DecodingConfig] extends CommonApi[Config] {

    /**
      * Indicates that this decoding run is not expected to consume the complete [[Input]].
      */
    def withPrefixOnly: this.type

    /**
      * Decodes an instance of [[T]] from the configured [[Input]] using the configured options.
      */
    def to[T: Decoder]: Sealed[T]
  }

  sealed trait Sealed[T] {

    def value: T

    def valueTry: Try[T]

    def valueEither: Either[Borer.Error[Input.Position], T]

    def valueAndInput: (T, Input[_])

    def valueAndInputTry: Try[(T, Input[_])]

    def valueAndInputEither: Either[Borer.Error[Input.Position], (T, Input[_])]
  }

  final private[borer] class Impl[V, Bytes, Config <: Borer.DecodingConfig](
      inputValue: V,
      defaultConfig: Config,
      defaultWrapper: Receiver.Transformer[Config],
      parserCreator: Parser.Creator[Bytes, Config],
      target: Target)(implicit p: Input.Provider[V])
      extends CommonApi.Impl[Config](defaultConfig, defaultWrapper) with Api[Config] with Sealed[AnyRef] {

    private[this] var prefixOnly: Boolean      = _
    private[this] var decoder: Decoder[AnyRef] = _

    def withPrefixOnly: this.type = {
      this.prefixOnly = true
      this
    }

    def to[T](implicit decoder: Decoder[T]): Sealed[T] = {
      this.decoder = decoder.asInstanceOf[Decoder[AnyRef]]
      this.asInstanceOf[Sealed[T]]
    }

    def value: AnyRef = {
      val reader = newReader()
      try {
        decodeFrom(reader)
      } catch {
        case e: Borer.Error[_] => throw e.withPosOf(reader)
        case NonFatal(e)       => throw new Borer.Error.General(reader.position, e)
      }
    }

    def valueTry: Try[AnyRef] = {
      val reader = newReader()
      try {
        Success(decodeFrom(reader))
      } catch {
        case e: Borer.Error[_] => Failure(e.withPosOf(reader))
        case NonFatal(e)       => Failure(new Borer.Error.General(reader.position, e))
      }
    }

    def valueEither: Either[Borer.Error[Input.Position], AnyRef] = {
      val reader = newReader()
      try {
        Right(decodeFrom(reader))
      } catch {
        case e: Borer.Error[_] => Left(e.withPosOf(reader))
        case NonFatal(e)       => Left(new Borer.Error.General(reader.position, e))
      }
    }

    def valueAndInput: (AnyRef, Input[_]) = {
      val reader = newReader()
      try {
        decodeFrom(reader) -> reader.input
      } catch {
        case e: Borer.Error[_] => throw e.withPosOf(reader)
        case NonFatal(e)       => throw new Borer.Error.General(reader.position, e)
      }
    }

    def valueAndInputTry: Try[(AnyRef, Input[_])] = {
      val reader = newReader()
      try {
        Success(decodeFrom(reader) -> reader.input)
      } catch {
        case e: Borer.Error[_] => Failure(e.withPosOf(reader))
        case NonFatal(e)       => Failure(new Borer.Error.General(reader.position, e))
      }
    }

    def valueAndInputEither: Either[Borer.Error[Input.Position], (AnyRef, Input[_])] = {
      val reader = newReader()
      try {
        Right(decodeFrom(reader) -> reader.input)
      } catch {
        case e: Borer.Error[_] => Left(e.withPosOf(reader))
        case NonFatal(e)       => Left(new Borer.Error.General(reader.position, e))
      }
    }

    private def newReader(): Reader = {
      val directParser = config match {
        case x: Json.DecodingConfig => io.bullet.borer.json.DirectParser(inputValue, x)
        case _                      => null
      }
      val parser =
        if (directParser ne null) null
        else
          parserCreator(p(inputValue).asInstanceOf[Input[Bytes]], p.byteAccess.asInstanceOf[ByteAccess[Bytes]], config)
      new InputReader(parser, directParser, receiverTransformer, config, target)
    }

    private def decodeFrom(reader: Reader): AnyRef =
      try {
        val value = decoder.read(reader)
        if (!prefixOnly) reader.readEndOfInput()
        value
      } finally reader.release()
  }
}
