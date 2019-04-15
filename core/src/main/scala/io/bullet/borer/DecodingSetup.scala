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

  sealed trait Api[Input, Config <: Reader.Config] {

    /**
      * Indicates that this decoding run is not expected to consume the complete [[Input]].
      */
    def withStartIndex(index: Long): this.type

    /**
      * Indicates that this decoding run is not expected to consume the complete [[Input]].
      */
    def withPrefixOnly: this.type

    /**
      * Configures the [[Config]] for this decoding run.
      */
    def withConfig(config: Config): this.type

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
      * Allows for injecting custom logic into the decoding process.
      * Used, for example, for on-the-side [[Logging]].
      */
    def withWrapper(receiverWrapper: Receiver.Wrapper[Config]): this.type

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

  private[borer] final class Impl[Input, Config <: Reader.Config](input: Input,
                                                                  defaultConfig: Config,
                                                                  defaultWrapper: Receiver.Wrapper[Config],
                                                                  parserCreator: Receiver.ParserCreator[Input, Config],
                                                                  target: Target)(implicit ia: InputAccess[Input])
      extends Borer.AbstractSetup[Config](defaultConfig, defaultWrapper) with Api[Input, Config]
      with Sealed[Input, AnyRef] {

    private[this] var startIndex: Long         = _
    private[this] var prefixOnly: Boolean      = _
    private[this] var decoder: Decoder[AnyRef] = _

    def withStartIndex(index: Long): this.type = {
      this.startIndex = index
      this
    }

    def withPrefixOnly: this.type = {
      this.prefixOnly = true
      this
    }

    def to[T](implicit decoder: Decoder[T]): Sealed[Input, T] = {
      this.decoder = decoder.asInstanceOf[Decoder[AnyRef]]
      this.asInstanceOf[Sealed[Input, T]]
    }

    def value: AnyRef = {
      val reader = newReader()
      try {
        decodeFrom(reader)
      } catch {
        case e: Borer.Error[_] ⇒ throw e.withPosOf(reader)
        case NonFatal(e)       ⇒ throw new Borer.Error.General(reader.position, e)
      }
    }

    def valueTry: Try[AnyRef] = {
      val reader = newReader()
      try {
        Success(decodeFrom(reader))
      } catch {
        case e: Borer.Error[_] ⇒ Failure(e.withPosOf(reader))
        case NonFatal(e)       ⇒ Failure(new Borer.Error.General(reader.position, e))
      }
    }

    def valueEither: Either[Borer.Error[Position[Input]], AnyRef] = {
      val reader = newReader()
      try {
        Right(decodeFrom(reader))
      } catch {
        case e: Borer.Error[_] ⇒ Left(e.withPosOf(reader))
        case NonFatal(e)       ⇒ Left(new Borer.Error.General(reader.position, e))
      }
    }

    def valueAndIndex: (AnyRef, Long) = {
      val reader = newReader()
      try {
        decodeFrom(reader) → reader.cursor
      } catch {
        case e: Borer.Error[_] ⇒ throw e.withPosOf(reader)
        case NonFatal(e)       ⇒ throw new Borer.Error.General(reader.position, e)
      }
    }

    def valueAndIndexTry: Try[(AnyRef, Long)] = {
      val reader = newReader()
      try {
        Success(decodeFrom(reader) → reader.cursor)
      } catch {
        case e: Borer.Error[_] ⇒ Failure(e.withPosOf(reader))
        case NonFatal(e)       ⇒ Failure(new Borer.Error.General(reader.position, e))
      }
    }

    def valueAndIndexEither: Either[Borer.Error[Position[Input]], (AnyRef, Long)] = {
      val reader = newReader()
      try {
        Right(decodeFrom(reader) → reader.cursor)
      } catch {
        case e: Borer.Error[_] ⇒ Left(e.withPosOf(reader))
        case NonFatal(e)       ⇒ Left(new Borer.Error.General(reader.position, e))
      }
    }

    private def newReader(): InputReader[Input, Config] =
      new InputReader(startIndex, parserCreator(input, config, ia), receiverWrapper, config, target)

    private def decodeFrom(reader: Reader): AnyRef = {
      reader.pull() // fetch first data item
      val value = decoder.read(reader)
      if (!prefixOnly) reader.readEndOfInput()
      value
    }
  }
}
