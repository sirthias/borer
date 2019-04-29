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
import io.bullet.borer.internal.{CborValidation, Util}
import io.bullet.borer.json._

case object Cbor extends Target {

  /**
    * Entry point into the CBOR encoding mini-DSL.
    */
  def encode[T: Encoder](value: T): EncodingSetup.Api[T, EncodingConfig] =
    new EncodingSetup.Impl(value, this, EncodingConfig.default, CborValidation.wrapper, CborRenderer)

  /**
    * Entry point into the CBOR decoding mini-DSL.
    */
  def decode[Input: InputAccess](input: Input): DecodingSetup.Api[Input, DecodingConfig] =
    new DecodingSetup.Impl(input, DecodingConfig.default, CborValidation.wrapper, CborParser.creator, this)

  /**
    * Constructs a new [[Writer]] that writes CBOR to the given [[Output]].
    */
  def writer(output: Output,
             config: EncodingConfig = EncodingConfig.default,
             receiverWrapper: Receiver.Wrapper[EncodingConfig] = CborValidation.wrapper): Writer =
    new Writer(receiverWrapper(CborRenderer(output), config), this, config)

  /**
    * Constructs a new [[Reader]] that reads CBOR from the given [[Input]].
    */
  def reader[Input: InputAccess](input: Input,
                                 startIndex: Long = 0,
                                 config: DecodingConfig = DecodingConfig.default,
                                 receiverWrapper: Receiver.Wrapper[DecodingConfig] = CborValidation.wrapper): Reader =
    new InputReader(startIndex, new CborParser(input, config), receiverWrapper, config, this)

  /**
    * @param compressFloatingPointValues set to false in order to always write floats as 32-bit values and doubles
    *                                        as 64-bit values, even if they could safely be represented with fewer bits
    */
  final case class EncodingConfig(compressFloatingPointValues: Boolean = true,
                                  maxArrayLength: Long = Int.MaxValue,
                                  maxMapLength: Long = Int.MaxValue,
                                  maxNestingLevels: Int = 1000)
      extends Borer.EncodingConfig with CborValidation.Config

  object EncodingConfig {
    val default = EncodingConfig()
  }

  /**
    * @param readIntegersAlsoAsFloatingPoint set to false to disable automatic conversion of integer to floating point
    *                                        values
    */
  final case class DecodingConfig(readIntegersAlsoAsFloatingPoint: Boolean = true,
                                  readDoubleAlsoAsFloat: Boolean = false,
                                  maxTextStringLength: Int = 1024 * 1024,
                                  maxByteStringLength: Int = 10 * 1024 * 1024,
                                  maxArrayLength: Long = Int.MaxValue,
                                  maxMapLength: Long = Int.MaxValue,
                                  maxNestingLevels: Int = 1000)
      extends Borer.DecodingConfig with CborValidation.Config with CborParser.Config {

    Util.requireNonNegative(maxTextStringLength, "maxTextStringLength")
    Util.requireNonNegative(maxByteStringLength, "maxByteStringLength")
    Util.requireNonNegative(maxArrayLength, "maxArrayLength")
    Util.requireNonNegative(maxMapLength, "maxMapLength")
    Util.requireNonNegative(maxNestingLevels, "maxNestingLevels")

    if (maxMapLength > Long.MaxValue / 2)
      throw new IllegalArgumentException(s"maxMapLength must be <= ${Long.MaxValue / 2}, but was $maxMapLength")
  }

  object DecodingConfig {
    val default = DecodingConfig()
  }
}

case object Json extends Target {

  /**
    * Entry point into the JSON encoding mini-DSL.
    */
  def encode[T: Encoder](value: T): EncodingSetup.JsonApi[T, EncodingConfig] =
    new EncodingSetup.Impl(value, Json, EncodingConfig.default, Receiver.nopWrapper, JsonRenderer)

  /**
    * Entry point into the JSON decoding mini-DSL.
    */
  def decode[Input: InputAccess](input: Input): DecodingSetup.Api[Input, DecodingConfig] =
    new DecodingSetup.Impl[Input, DecodingConfig](
      input,
      DecodingConfig.default,
      Receiver.nopWrapper,
      JsonParser.creator,
      null)

  /**
    * Constructs a new [[Writer]] that writes JSON to the given [[Output]].
    */
  def writer(output: Output,
             config: EncodingConfig = EncodingConfig.default,
             receiverWrapper: Receiver.Wrapper[EncodingConfig] = Receiver.nopWrapper): Writer =
    new Writer(receiverWrapper(JsonRenderer(output), config), null, config)

  /**
    * Constructs a new [[Reader]] that reads JSON from the given [[Input]].
    */
  def reader[Input: InputAccess](input: Input,
                                 startIndex: Long = 0,
                                 config: DecodingConfig = DecodingConfig.default,
                                 receiverWrapper: Receiver.Wrapper[DecodingConfig] = Receiver.nopWrapper): Reader =
    new InputReader(startIndex, new JsonParser(input, config), receiverWrapper, config, Json)

  final case class EncodingConfig() extends Borer.EncodingConfig {
    def compressFloatingPointValues = false
  }

  object EncodingConfig {
    val default = EncodingConfig()
  }

  /**
    * @param readIntegersAlsoAsFloatingPoint set to false to disable automatic conversion of integer to floating point
    *                                        values
    */
  final case class DecodingConfig(readIntegersAlsoAsFloatingPoint: Boolean = true,
                                  maxNumberAbsExponent: Int = 64,
                                  maxStringLength: Int = 1024 * 1024,
                                  maxNumberMantissaDigits: Int = 32,
                                  maxNumberExponentDigits: Int = 3)
      extends Borer.DecodingConfig with JsonParser.Config {

    Util.requirePositive(maxStringLength, "maxStringLength")
    if (maxNumberExponentDigits < 0 || maxNumberExponentDigits > 9)
      throw new IllegalArgumentException(
        s"$maxNumberExponentDigits must be in the range [0..9], but was $maxNumberExponentDigits")

    // the JsonParser never produces Float values directly (only doubles), so this is necessary
    def readDoubleAlsoAsFloat = true
  }

  object DecodingConfig {
    val default = DecodingConfig()
  }
}

/**
  * Super-type of the [[Cbor]] and [[Json]] objects.
  *
  * Used, for example, as the type of the `target` member of [[Reader]] and [[Writer]] instances,
  * which allows custom logic to pick different (de)serialization approaches
  * depending on whether the target is CBOR or JSON.
  */
sealed abstract class Target {

  def encode[T: Encoder](value: T): EncodingSetup.Api[T, _]

  def decode[Input: InputAccess](input: Input): DecodingSetup.Api[Input, _]
}

/**
  * Main entry point into the CBOR API.
  */
object Borer {

  sealed abstract class EncodingConfig extends Writer.Config
  sealed abstract class DecodingConfig extends Reader.Config

  private[borer] abstract class AbstractSetup[Config](defaultConfig: Config, defaultWrapper: Receiver.Wrapper[Config]) {
    protected var config: Config                            = defaultConfig
    protected var receiverWrapper: Receiver.Wrapper[Config] = defaultWrapper

    final def withConfig(config: Config): this.type = {
      this.config = config
      this
    }

    final def withPrintLogging(maxShownByteArrayPrefixLen: Int, maxShownStringPrefixLen: Int): this.type =
      withWrapper(Logging(Logging.PrintLogger(maxShownByteArrayPrefixLen, maxShownStringPrefixLen)))

    final def withStringLogging(stringBuilder: JStringBuilder,
                                maxShownByteArrayPrefixLen: Int,
                                maxShownStringPrefixLen: Int,
                                lineSeparator: String): this.type =
      withWrapper {
        Logging {
          Logging.ToStringLogger(stringBuilder, maxShownByteArrayPrefixLen, maxShownStringPrefixLen, lineSeparator)
        }
      }

    final def withWrapper(receiverWrapper: Receiver.Wrapper[Config]): this.type = {
      this.receiverWrapper = receiverWrapper
      this
    }
  }

  sealed abstract class Error[IO <: AnyRef](private var _io: IO, msg: String, cause: Throwable = null)
      extends RuntimeException(msg, cause) {

    final override def getMessage = s"$msg [${_io}]"

    final def io: IO = _io

    private[borer] def withPosOf[Input](reader: InputReader[Input, _]): Error[Position[Input]] = {
      val thiz = this.asInstanceOf[Error[Position[Input]]]
      if (thiz._io eq null) thiz._io = reader.lastPosition
      thiz
    }

    private[borer] def withOut(out: Output): Error[Output] = {
      val thiz = this.asInstanceOf[Error[Output]]
      if (thiz._io eq null) thiz._io = out
      thiz
    }
  }

  object Error {
    final class UnexpectedEndOfInput[IO <: AnyRef](io: IO, expected: String)
        extends Error[IO](io, s"Expected $expected but got end of input")

    final class InvalidInputData[IO <: AnyRef](io: IO, msg: String) extends Error[IO](io, msg) {
      def this(io: IO, expected: String, actual: String) = this(io, s"Expected $expected but got $actual")
    }

    final class ValidationFailure[IO <: AnyRef](io: IO, msg: String) extends Error[IO](io, msg)

    final class Unsupported[IO <: AnyRef](io: IO, msg: String) extends Error[IO](io, msg)

    final class Overflow[IO <: AnyRef](io: IO, msg: String) extends Error[IO](io, msg)

    final class General[IO <: AnyRef](io: IO, cause: Throwable) extends Error[IO](io, cause.toString, cause)
  }
}
