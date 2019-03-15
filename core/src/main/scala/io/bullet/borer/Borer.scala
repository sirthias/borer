/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import io.bullet.borer.cbor.{CborParser, CborRenderer}
import java.nio.charset.StandardCharsets.UTF_8

import io.bullet.borer.json.{JsonParser, JsonRenderer}

import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal

case object Cbor extends Borer.Target {

  /**
    * Entry point into the CBOR encoding mini-DSL.
    */
  def encode[T: Encoder](value: T): Borer.EncodingSetup[T] =
    new Borer.EncodingSetup[T](value, this, CborRenderer)

  /**
    * Entry point into the CBOR decoding mini-DSL.
    */
  def decode(input: Input): Borer.DecodingSetup[input.Self] =
    new Borer.DecodingSetup(input, this, CborParser)

  /**
    * Constructs a new [[Writer]] that writes CBOR to the given [[Output]].
    */
  def writer(output: Output,
             config: Writer.Config = Writer.Config.default,
             validationApplier: Receiver.Applier[Output] = Receiver.defaultApplier): Writer = {
    val receiver = validationApplier(Validation.creator(this, config.validation), CborRenderer)
    new Writer(output, receiver, config, this)
  }

  /**
    * Constructs a new [[Reader]] that reads CBOR from the given [[Input]].
    */
  def reader(input: Input,
             config: Reader.Config = Reader.Config.default,
             validationApplier: Receiver.Applier[Input] = Receiver.defaultApplier): Reader =
    new Reader(input, CborParser, validationApplier, config, this)
}

case object Json extends Borer.Target {

  /**
    * Entry point into the JSON encoding mini-DSL.
    */
  def encode[T: Encoder](value: T): Borer.EncodingSetup[T] with JsonEncoding =
    new Borer.EncodingSetup[T](value, this, new JsonRenderer).asInstanceOf[Borer.EncodingSetup[T] with JsonEncoding]

  /**
    * Entry point into the JSON decoding mini-DSL.
    */
  def decode(input: Input): Borer.DecodingSetup[input.Self] =
    new Borer.DecodingSetup(input, this, new JsonParser)

  /**
    * Constructs a new [[Writer]] that writes JSON to the given [[Output]].
    */
  def writer(output: Output,
             config: Writer.Config = Writer.Config.default,
             validationApplier: Receiver.Applier[Output] = Receiver.defaultApplier): Writer = {
    val receiver = validationApplier(Validation.creator(this, config.validation), new JsonRenderer)
    new Writer(output, receiver, config, this)
  }

  /**
    * Constructs a new [[Reader]] that reads JSON from the given [[Input]].
    */
  def reader(input: Input,
             config: Reader.Config = Reader.Config.default,
             validationApplier: Receiver.Applier[Input] = Receiver.defaultApplier): Reader =
    new Reader(input, new JsonParser, validationApplier, config, this)

  sealed trait JsonEncoding

  implicit final class EncodingExtraOps[T](val underlying: Borer.EncodingSetup[T] with JsonEncoding) extends AnyVal {

    /**
      * Short-cut for encoding to a plain byte array, throwing an exception in case of any failures,
      * and then immediately UTF-8 decoding into a [[String]].
      */
    @inline def toUtf8String: String = new String(underlying.toByteArray, UTF_8)
  }
}

/**
  * Main entry point into the CBOR API.
  */
object Borer {

  /**
    * Super-type of the [[Cbor]] and [[Json]] objects.
    *
    * Used, for example, as the type of the `target` member of [[Reader]] and [[Writer]] instances,
    * which allows custom logic to pick different (de)serialization approaches
    * depending on whether the target is CBOR or JSON.
    */
  sealed abstract class Target {
    def encode[T: Encoder](value: T): Borer.EncodingSetup[T]
    def decode(input: Input): Borer.DecodingSetup[input.Self]

    def writer(output: Output,
               config: Writer.Config = Writer.Config.default,
               validationApplier: Receiver.Applier[Output] = Receiver.defaultApplier): Writer

    def reader(input: Input,
               config: Reader.Config = Reader.Config.default,
               validationApplier: Receiver.Applier[Input] = Receiver.defaultApplier): Reader
  }

  final class EncodingSetup[T: Encoder] private[borer] (value: T, target: Target, renderer: Receiver[Output]) {

    private[this] var config: Writer.Config                       = Writer.Config.default
    private[this] var validationApplier: Receiver.Applier[Output] = Receiver.defaultApplier

    /**
      * Configures the [[Writer.Config]] for this encoding run.
      */
    @inline def withConfig(config: Writer.Config): this.type = {
      this.config = config
      this
    }

    /**
      * Enables logging of the encoding progress to the console.
      * Each data item that is written by the application is pretty printed to the console on its own line.
      */
    def withPrintLogging(maxShownByteArrayPrefixLen: Int = 20, maxShownStringPrefixLen: Int = 50): this.type = {
      withValidationApplier(
        Logging.afterValidation(Logging.PrintLogger(maxShownByteArrayPrefixLen, maxShownStringPrefixLen)))
      this
    }

    /**
      * Enables logging of the encoding progress to the given [[java.lang.StringBuilder]].
      * Each data item that is written by the application is formatted and appended as its own line.
      */
    def withStringLogging(stringBuilder: java.lang.StringBuilder,
                          maxShownByteArrayPrefixLen: Int = 20,
                          maxShownStringPrefixLen: Int = 50,
                          lineSeparator: String = System.lineSeparator()): this.type = {
      withValidationApplier(
        Logging.afterValidation(
          Logging.ToStringLogger(stringBuilder, maxShownByteArrayPrefixLen, maxShownStringPrefixLen, lineSeparator)))
      this
    }

    /**
      * Allows for customizing the injection points around input validation.
      * Used, for example, for on-the-side [[Logging]] of the encoding process.
      */
    @inline def withValidationApplier(validationApplier: Receiver.Applier[Output]): this.type = {
      this.validationApplier = validationApplier
      this
    }

    /**
      * Short-cut for encoding to a plain byte array, throwing an exception in case of any failures.
      */
    @inline def toByteArray: Array[Byte] = to[Array[Byte]].bytes

    /**
      * Short-cut for encoding to a plain byte array, wrapped in a [[Try]] for error handling.
      */
    @inline def toByteArrayTry: Try[Array[Byte]] = to[Array[Byte]].bytesTry

    /**
      * Encodes an instance of [[T]] to the given `Bytes` type using the configured options.
      */
    def to[Bytes](implicit ba: ByteAccess[Bytes]): Either[Error[ba.Out], ba.Out] = {
      val receiver = validationApplier(Validation.creator(target, config.validation), renderer)
      val writer   = new Writer(ba.newOutput, receiver, config, target)
      def out      = writer.output.asInstanceOf[ba.Out]
      try {
        writer
          .write(value)
          .writeEndOfInput() // doesn't actually write anything but triggers certain validation checks (if configured)
        Right(out)
      } catch {
        case e: Error[_] ⇒ Left(e.asInstanceOf[Error[ba.Out]])
        case NonFatal(e) ⇒ Left(Error.General(out, e))
      }
    }
  }

  final class DecodingSetup[In <: Input] private[borer] (input: Input, target: Target, parser: Receiver.Parser) {

    private[this] var prefixOnly: Boolean                        = _
    private[this] var config: Reader.Config                      = Reader.Config.default
    private[this] var validationApplier: Receiver.Applier[Input] = Receiver.defaultApplier[Input]

    /**
      * Indicates that this decoding run is not expected to consume the complete [[Input]].
      */
    @inline def consumePrefix: this.type = {
      this.prefixOnly = true
      this
    }

    /**
      * Configures the [[Reader.Config]] for this decoding run.
      */
    @inline def withConfig(config: Reader.Config): this.type = {
      this.config = config
      this
    }

    /**
      * Enables logging of this decoding run to the console.
      * Each data item that is consumed from the underlying CBOR stream is pretty printed to the console
      * on its own line.
      */
    def withPrintLogging(maxShownByteArrayPrefixLen: Int = 20, maxShownStringPrefixLen: Int = 50): this.type = {
      withValidationApplier(
        Logging.afterValidation(Logging.PrintLogger(maxShownByteArrayPrefixLen, maxShownStringPrefixLen)))
      this
    }

    /**
      * Enables logging of this decoding run to the given [[java.lang.StringBuilder]].
      * Each data item that is consumed from the underlying CBOR stream is formatted and appended as its own line.
      */
    def withStringLogging(stringBuilder: java.lang.StringBuilder,
                          maxShownByteArrayPrefixLen: Int = 20,
                          maxShownStringPrefixLen: Int = 50,
                          lineSeparator: String = System.lineSeparator()): this.type = {
      withValidationApplier(
        Logging.afterValidation(
          Logging.ToStringLogger(stringBuilder, maxShownByteArrayPrefixLen, maxShownStringPrefixLen, lineSeparator)))
      this
    }

    /**
      * Allows for customizing the injection points around input validation.
      * Used, for example, for on-the-side [[Logging]] of the decoding process.
      */
    @inline def withValidationApplier(validationApplier: Receiver.Applier[Input]): this.type = {
      this.validationApplier = validationApplier
      this
    }

    /**
      * Decodes an instance of [[T]] from the configured [[Input]] using the configured options.
      */
    def to[T](implicit decoder: Decoder[T]): Either[Error[In], (T, In)] = {
      val reader = new Reader(input, parser, validationApplier, config, target)
      def in     = reader.input.asInstanceOf[In]
      try {
        reader.pull() // fetch first data item
        val value = decoder.read(reader)
        if (!prefixOnly) reader.readEndOfInput()
        Right(value → in)
      } catch {
        case e: Error[_] ⇒ Left(e.asInstanceOf[Error[In]])
        case NonFatal(e) ⇒ Left(Error.General(in, e))
      }
    }
  }

  sealed abstract class Error[IO](msg: String, cause: Throwable = null) extends RuntimeException(msg, cause) {
    def io: IO
  }

  object Error {
    final case class InvalidCborData[IO](io: IO, msg: String) extends Error[IO](msg)

    final case class InvalidJsonData[IO](io: IO, msg: String) extends Error[IO](msg)

    final case class ValidationFailure[IO](io: IO, msg: String) extends Error[IO](msg)

    final case class InsufficientInput[IO](io: IO, length: Long) extends Error[IO]("Insufficient Input")

    final case class UnexpectedDataItem[IO](io: IO, expected: String, actual: String)
        extends Error[IO](s"Unexpected data item: Expected [$expected] but got [$actual]")

    final case class Unsupported[IO](io: IO, msg: String) extends Error[IO](msg)

    final case class Overflow[IO](io: IO, msg: String) extends Error[IO](msg)

    final case class General[IO](io: IO, cause: Throwable) extends Error[IO](cause.toString, cause)

    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    implicit final class EncodingResultOps[Out <: Output](val underlying: Either[Error[Out], Out]) extends AnyVal {

      @inline def bytes: Out#Result = underlying match {
        case Right(out) ⇒ out.result()
        case Left(e)    ⇒ throw e
      }

      @inline def bytesTry: Try[Out#Result] = underlying match {
        case Right(out) ⇒ Success(out.result())
        case Left(e)    ⇒ Failure(e)
      }

      @inline def output: Out = underlying match {
        case Right(out) ⇒ out
        case Left(e)    ⇒ throw e
      }

      @inline def error: Error[Out] = underlying.left.get
    }

    implicit final class DecodingResultOps[In, T](val underlying: Either[Error[In], (T, In)]) extends AnyVal {

      @inline def value: T = underlying match {
        case Right((x, _)) ⇒ x
        case Left(e)       ⇒ throw e
      }

      @inline def valueTry: Try[T] = underlying match {
        case Right((x, _)) ⇒ Success(x)
        case Left(e)       ⇒ Failure(e)
      }

      @inline def error: Error[In] = underlying.left.get
    }
  }
}
