/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import java.lang.{StringBuilder ⇒ JStringBuilder}

import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

object DecodingSetup {

  sealed trait Api[Input] {

    /**
      * Indicates that this decoding run is not expected to consume the complete [[Input]].
      */
    def withStartIndex(index: Long): this.type

    /**
      * Indicates that this decoding run is not expected to consume the complete [[Input]].
      */
    def withPrefixOnly: this.type

    /**
      * Configures the [[Reader.Config]] for this decoding run.
      */
    def withConfig(config: Reader.Config): this.type

    /**
      * Enables logging of this decoding run to the console.
      * Each data item that is consumed from the underlying CBOR stream is pretty printed to the console
      * on its own line.
      */
    def withPrintLogging(maxShownByteArrayPrefixLen: Int = 20, maxShownStringPrefixLen: Int = 50): this.type

    /**
      * Enables logging of this decoding run to the given [[JStringBuilder]].
      * Each data item that is consumed from the underlying CBOR stream is formatted and appended as its own line.
      */
    def withStringLogging(stringBuilder: JStringBuilder,
                          maxShownByteArrayPrefixLen: Int = 20,
                          maxShownStringPrefixLen: Int = 50,
                          lineSeparator: String = System.lineSeparator()): this.type

    /**
      * Allows for customizing the injection points around input validation.
      * Used, for example, for on-the-side [[Logging]] of the decoding process.
      */
    def withValidationApplier(validationApplier: Receiver.Applier): this.type

    /**
      * Decodes an instance of [[T]] from the configured [[Input]] using the configured options.
      */
    def to[T: Decoder]: Sealed[Input, T]
  }

  sealed trait Sealed[Input, T] {

    def value: T

    def valueTry: Try[T]

    def valueEither: Either[Borer.Error[Position[Input]], T]

    def valueAndIndex: (T, Long)

    def valueAndIndexTry: Try[(T, Long)]

    def valueAndIndexEither: Either[Borer.Error[Position[Input]], (T, Long)]
  }

  private[borer] final class Impl[Input: InputAccess](input: Input, target: Borer.Target, parser: Receiver.Parser)
      extends Borer.AbstractSetup with Api[Input] with Sealed[Input, AnyRef] {

    private[this] var startIndex: Long         = _
    private[this] var prefixOnly: Boolean      = _
    private[this] var config: Reader.Config    = Reader.Config.default
    private[this] var decoder: Decoder[AnyRef] = _

    def withStartIndex(index: Long): this.type = {
      this.startIndex = index
      this
    }

    def withPrefixOnly: this.type = {
      this.prefixOnly = true
      this
    }

    def withConfig(config: Reader.Config): this.type = {
      this.config = config
      this
    }

    def to[T](implicit decoder: Decoder[T]): Sealed[Input, T] = {
      this.decoder = decoder.asInstanceOf[Decoder[AnyRef]]
      this.asInstanceOf[Sealed[Input, T]]
    }

    def value: AnyRef = decodeFrom(newReader())

    def valueTry: Try[AnyRef] = {
      val reader = newReader()
      try {
        Success(decodeFrom(reader))
      } catch {
        case e: Borer.Error[_] ⇒ Failure(e)
        case NonFatal(e)       ⇒ Failure(Borer.Error.General(Position(input, reader.cursor), e))
      }
    }

    def valueEither: Either[Borer.Error[Position[Input]], AnyRef] = {
      val reader = newReader()
      try {
        Right(decodeFrom(reader))
      } catch {
        case e: Borer.Error[_] ⇒ Left(e.asInstanceOf[Borer.Error[Position[Input]]])
        case NonFatal(e)       ⇒ Left(Borer.Error.General(Position(input, reader.cursor), e))
      }
    }

    def valueAndIndex: (AnyRef, Long) = {
      val reader = newReader()
      decodeFrom(reader) → reader.cursor
    }

    def valueAndIndexTry: Try[(AnyRef, Long)] = {
      val reader = newReader()
      try {
        Success(decodeFrom(reader) → reader.cursor)
      } catch {
        case e: Borer.Error[_] ⇒ Failure(e)
        case NonFatal(e)       ⇒ Failure(Borer.Error.General(Position(input, reader.cursor), e))
      }
    }

    def valueAndIndexEither: Either[Borer.Error[Position[Input]], (AnyRef, Long)] = {
      val reader = newReader()
      try {
        Right(decodeFrom(reader) → reader.cursor)
      } catch {
        case e: Borer.Error[_] ⇒ Left(e.asInstanceOf[Borer.Error[Position[Input]]])
        case NonFatal(e)       ⇒ Left(Borer.Error.General(Position(input, reader.cursor), e))
      }
    }

    private def newReader(): Reader =
      new Reader(input, startIndex, parser, validationApplier, config, target)(InputAccess.asAny[Input])

    private def decodeFrom(reader: Reader): AnyRef = {
      reader.pull() // fetch first data item
      val value = decoder.read(reader)
      if (!prefixOnly) reader.readEndOfInput()
      value
    }
  }
}
