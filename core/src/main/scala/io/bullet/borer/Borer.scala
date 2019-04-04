/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import java.lang.{StringBuilder ⇒ JStringBuilder}

import io.bullet.borer.cbor._
import io.bullet.borer.json._

case object Cbor extends Borer.Target {

  /**
    * Entry point into the CBOR encoding mini-DSL.
    */
  def encode[T: Encoder](value: T): EncodingSetup.Api[T] =
    new EncodingSetup.Impl[T](value, this, CborRenderer)

  /**
    * Entry point into the CBOR decoding mini-DSL.
    */
  def decode[Input: InputAccess](input: Input): DecodingSetup.Api[Input] =
    new DecodingSetup.Impl(input, this, CborParser)

  /**
    * Constructs a new [[Writer]] that writes CBOR to the given [[Output]].
    */
  def writer(output: Output,
             config: Writer.Config = Writer.Config.default,
             validationApplier: Receiver.Applier = Receiver.defaultApplier): Writer = {
    val receiver = validationApplier(Validation.creator(this, config.validation), CborRenderer(output))
    new Writer(receiver, config, this)
  }

  /**
    * Constructs a new [[Reader]] that reads CBOR from the given [[Input]].
    */
  def reader[Input: InputAccess](input: Input,
                                 startIndex: Long = 0,
                                 config: Reader.Config = Reader.Config.default,
                                 validationApplier: Receiver.Applier = Receiver.defaultApplier): Reader =
    new Reader(input, startIndex, CborParser, validationApplier, config, this)(InputAccess.asAny[Input])
}

case object Json extends Borer.Target {

  /**
    * Entry point into the JSON encoding mini-DSL.
    */
  def encode[T: Encoder](value: T): EncodingSetup.JsonApi[T] =
    new EncodingSetup.Impl[T](value, this, JsonRenderer)

  /**
    * Entry point into the JSON decoding mini-DSL.
    */
  def decode[Input: InputAccess](input: Input): DecodingSetup.Api[Input] =
    new DecodingSetup.Impl(input, this, new JsonParser)

  /**
    * Constructs a new [[Writer]] that writes JSON to the given [[Output]].
    */
  def writer(output: Output,
             config: Writer.Config = Writer.Config.default,
             validationApplier: Receiver.Applier = Receiver.defaultApplier): Writer = {
    val receiver = validationApplier(Validation.creator(this, config.validation), JsonRenderer(output))
    new Writer(receiver, config, this)
  }

  /**
    * Constructs a new [[Reader]] that reads JSON from the given [[Input]].
    */
  def reader[Input: InputAccess](input: Input,
                                 startIndex: Long = 0,
                                 config: Reader.Config = Reader.Config.default,
                                 validationApplier: Receiver.Applier = Receiver.defaultApplier): Reader =
    new Reader(input, startIndex, new JsonParser, validationApplier, config, this)(InputAccess.asAny[Input])
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
    def encode[T: Encoder](value: T): EncodingSetup.Api[T]

    def decode[Input: InputAccess](input: Input): DecodingSetup.Api[Input]

    def writer(output: Output,
               config: Writer.Config = Writer.Config.default,
               validationApplier: Receiver.Applier = Receiver.defaultApplier): Writer

    def reader[Input: InputAccess](input: Input,
                                   startIndex: Long = 0,
                                   config: Reader.Config = Reader.Config.default,
                                   validationApplier: Receiver.Applier = Receiver.defaultApplier): Reader
  }

  private[borer] abstract class AbstractSetup {
    protected var validationApplier: Receiver.Applier = Receiver.defaultApplier

    def withPrintLogging(maxShownByteArrayPrefixLen: Int, maxShownStringPrefixLen: Int): this.type = {
      withValidationApplier(
        Logging.afterValidation(Logging.PrintLogger(maxShownByteArrayPrefixLen, maxShownStringPrefixLen)))
      this
    }

    def withStringLogging(stringBuilder: JStringBuilder,
                          maxShownByteArrayPrefixLen: Int,
                          maxShownStringPrefixLen: Int,
                          lineSeparator: String): this.type = {
      withValidationApplier(
        Logging.afterValidation(
          Logging.ToStringLogger(stringBuilder, maxShownByteArrayPrefixLen, maxShownStringPrefixLen, lineSeparator)))
      this
    }

    def withValidationApplier(validationApplier: Receiver.Applier): this.type = {
      this.validationApplier = validationApplier
      this
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
  }
}
