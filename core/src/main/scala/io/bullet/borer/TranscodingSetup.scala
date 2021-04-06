/*
 * Copyright (c) 2019-2021 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import io.bullet.borer.internal.{ElementDeque, ElementDequeCache, Parser}

import java.lang.{StringBuilder => JStringBuilder}
import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal

object TranscodingSetup {

  sealed trait EncodingApi[EncodingConfig <: Borer.TransEncodingConfig, DecodingConfig <: Reader.Config] {

    /**
      * Configures the [[Config]] for this encoding run.
      */
    def withConfig(config: EncodingConfig): this.type

    /**
      * Enables logging of the encoding progress to the console.
      * Each data item that is written by the application is pretty printed to the console on its own line.
      */
    def withPrintLogging(
        maxShownByteArrayPrefixLen: Int = 20,
        maxShownStringPrefixLen: Int = 50,
        maxShownArrayElems: Int = 20,
        maxShownMapEntries: Int = 20): this.type

    /**
      * Enables logging of the encoding progress to the given [[JStringBuilder]].
      * Each data item that is written by the application is formatted and appended as its own line.
      */
    def withStringLogging(
        stringBuilder: JStringBuilder,
        maxShownByteArrayPrefixLen: Int = 20,
        maxShownStringPrefixLen: Int = 50,
        maxShownArrayElems: Int = 20,
        maxShownMapEntries: Int = 20,
        lineSeparator: String = System.lineSeparator()): this.type

    /**
      * Allows for injecting custom logic into the encoding process.
      * Used, for example, for on-the-side [[Logging]].
      */
    def withWrapper(receiverWrapper: Receiver.Wrapper[EncodingConfig]): this.type

    def transDecode: DecodingApi[DecodingConfig]
  }

  final private[borer] class EncodingApiImpl[A: Encoder, EC <: Borer.TransEncodingConfig, DC <: Reader.Config](
      value: A,
      target: Target,
      defaultEncConfig: EC,
      defaultEncWrapper: Receiver.Wrapper[EC],
      defaultDecConfig: DC,
      defaultDecWrapper: Receiver.Wrapper[DC])
      extends Borer.AbstractSetup(defaultEncConfig, defaultEncWrapper) with EncodingApi[EC, DC] {

    def transDecode: DecodingApi[DC] =
      new DecodingApiImpl(value, target, config, receiverWrapper, defaultDecConfig, defaultDecWrapper)
  }

  sealed trait DecodingApi[Config <: Reader.Config] {

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
    def withPrintLogging(
        maxShownByteArrayPrefixLen: Int = 20,
        maxShownStringPrefixLen: Int = 50,
        maxShownArrayElems: Int = 20,
        maxShownMapEntries: Int = 20): this.type

    /**
      * Enables logging of this decoding run to the given [[JStringBuilder]].
      * Each data item that is consumed from the underlying CBOR stream is formatted and appended as its own line.
      */
    def withStringLogging(
        stringBuilder: JStringBuilder,
        maxShownByteArrayPrefixLen: Int = 20,
        maxShownStringPrefixLen: Int = 50,
        maxShownArrayElems: Int = 20,
        maxShownMapEntries: Int = 20,
        lineSeparator: String = System.lineSeparator()): this.type

    /**
      * Allows for injecting custom logic into the decoding process.
      * Used, for example, for on-the-side [[Logging]].
      */
    def withWrapper(receiverWrapper: Receiver.Wrapper[Config]): this.type

    /**
      * Decodes an instance of [[T]] from the configured [[Input]] using the configured options.
      */
    def to[T: Decoder]: Sealed[T]
  }

  sealed trait Sealed[T] {

    def value: T

    def valueTry: Try[T]

    def valueEither: Either[Borer.Error[Unit], T]
  }

  final private[borer] class DecodingApiImpl[A: Encoder, EC <: Borer.TransEncodingConfig, DC <: Reader.Config](
      valueToEncode: A,
      target: Target,
      encConfig: EC,
      encWrapper: Receiver.Wrapper[EC],
      defaultDecConfig: DC,
      defaultDecWrapper: Receiver.Wrapper[DC])
      extends Borer.AbstractSetup(defaultDecConfig, defaultDecWrapper) with DecodingApi[DC] with Sealed[AnyRef] {

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

    def value: AnyRef =
      try {
        transcode()
      } catch {
        case e: Borer.Error[_] => throw e.withUnit
        case NonFatal(e)       => throw new Borer.Error.General((), e)
      }

    def valueTry: Try[AnyRef] =
      try {
        Success(transcode())
      } catch {
        case e: Borer.Error[_] => Failure(e.withUnit)
        case NonFatal(e)       => Failure(new Borer.Error.General((), e))
      }

    def valueEither: Either[Borer.Error[Unit], AnyRef] =
      try {
        Right(transcode())
      } catch {
        case e: Borer.Error[_] => Left(e.withUnit)
        case NonFatal(e)       => Left(new Borer.Error.General((), e))
      }

    private def transcode(): AnyRef = {
      val deque =
        if (encConfig.allowBufferCaching)
          ElementDequeCache.acquire(encConfig.maxBufferSize)
        else
          new ElementDeque(encConfig.maxBufferSize)
      try {
        val writer = new Writer(null, encWrapper(deque.appendReceiver, encConfig), target, encConfig)
        writer
          .write(valueToEncode)
          .writeEndOfInput() // doesn't actually write anything but triggers certain validation checks (if configured)

        val reader = new InputReader(new Parser.DequeParser(deque), null, receiverWrapper, config, target)
        val value  = decoder.read(reader)
        if (!prefixOnly) reader.readEndOfInput()
        value
      } finally {
        if (encConfig.allowBufferCaching) ElementDequeCache.release(deque)
      }
    }
  }
}
