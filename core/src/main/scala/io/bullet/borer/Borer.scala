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

  sealed abstract class Error[IO <: AnyRef](private var _io: IO, msg: String, cause: Throwable = null)
      extends RuntimeException(msg, cause) {

    final def io: IO = _io

    private[borer] def withPosOf[Input](reader: Reader): Error[Position[Input]] = {
      val thiz = this.asInstanceOf[Error[Position[Input]]]
      if (thiz._io eq null) thiz._io = reader.position[Input]
      thiz
    }
  }

  object Error {
    final class InvalidCborData[IO <: AnyRef](io: IO, msg: String) extends Error[IO](io, msg)

    final class InvalidJsonData[IO <: AnyRef](io: IO, msg: String) extends Error[IO](io, msg)

    final class ValidationFailure[IO <: AnyRef](io: IO, msg: String) extends Error[IO](io, msg)

    final class InsufficientInput[IO <: AnyRef](io: IO) extends Error[IO](io, "Insufficient Input")

    final class UnexpectedDataItem[IO <: AnyRef](io: IO, val expected: String, val actual: String)
        extends Error[IO](io, s"Unexpected data item: Expected [$expected] but got [$actual]")

    final class Unsupported[IO <: AnyRef](io: IO, msg: String) extends Error[IO](io, msg)

    final class Overflow[IO <: AnyRef](io: IO, msg: String) extends Error[IO](io, msg)

    final class General[IO <: AnyRef](io: IO, cause: Throwable) extends Error[IO](io, cause.toString, cause)
  }
}
