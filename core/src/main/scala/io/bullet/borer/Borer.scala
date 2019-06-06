/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import java.lang.{StringBuilder => JStringBuilder}

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
  def decode[T](input: T)(implicit w: Input.Wrapper[T]): DecodingSetup.Api[w.In, DecodingConfig] =
    new DecodingSetup.Impl(w(input), DecodingConfig.default, CborValidation.wrapper, CborParser.creator, this)

  /**
    * Constructs a new [[Writer]] that writes CBOR to the given [[Output]].
    */
  def writer(
      output: Output,
      config: EncodingConfig = EncodingConfig.default,
      receiverWrapper: Receiver.Wrapper[EncodingConfig] = CborValidation.wrapper): Writer =
    new Writer(receiverWrapper(CborRenderer(output), config), this, config)

  /**
    * Constructs a new [[Reader]] that reads CBOR from the given [[Input]].
    */
  def reader[In <: Input](
      input: In,
      config: DecodingConfig = DecodingConfig.default,
      receiverWrapper: Receiver.Wrapper[DecodingConfig] = CborValidation.wrapper): Reader =
    new InputReader(new CborParser(input, config), receiverWrapper, config, this)

  /**
    * @param compressFloatingPointValues set to false in order to always write floats as 32-bit values and doubles
    *                                        as 64-bit values, even if they could safely be represented with fewer bits
    */
  final case class EncodingConfig(
      bufferSize: Int = 1024,
      compressFloatingPointValues: Boolean = true,
      maxArrayLength: Long = Int.MaxValue,
      maxMapLength: Long = Int.MaxValue,
      maxNestingLevels: Int = 1000)
      extends Borer.EncodingConfig with CborValidation.Config {

    if (bufferSize < 8) throw new IllegalArgumentException(s"bufferSize must be >= 8, but was $bufferSize")
  }

  object EncodingConfig {
    val default = EncodingConfig()
  }

  /**
    * @param readIntegersAlsoAsFloatingPoint set to false to disable automatic conversion of integer to floating point
    *                                        values
    */
  final case class DecodingConfig(
      readIntegersAlsoAsFloatingPoint: Boolean = true,
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
  def decode[T](input: T)(implicit w: Input.Wrapper[T]): DecodingSetup.Api[w.In, DecodingConfig] =
    new DecodingSetup.Impl[w.In, DecodingConfig](
      w(input),
      DecodingConfig.default,
      Receiver.nopWrapper,
      JsonParser.creator,
      null)

  /**
    * Constructs a new [[Writer]] that writes JSON to the given [[Output]].
    */
  def writer(
      output: Output,
      config: EncodingConfig = EncodingConfig.default,
      receiverWrapper: Receiver.Wrapper[EncodingConfig] = Receiver.nopWrapper): Writer =
    new Writer(receiverWrapper(JsonRenderer(output), config), null, config)

  /**
    * Constructs a new [[Reader]] that reads JSON from the given [[Input]].
    */
  def reader[In <: Input](
      input: In,
      config: DecodingConfig = DecodingConfig.default,
      receiverWrapper: Receiver.Wrapper[DecodingConfig] = Receiver.nopWrapper): Reader =
    new InputReader(new JsonParser(input, config), receiverWrapper, config, Json)

  final case class EncodingConfig(bufferSize: Int = 1024) extends Borer.EncodingConfig {
    def compressFloatingPointValues = false

    if (bufferSize < 8) throw new IllegalArgumentException(s"bufferSize must be >= 8, but was $bufferSize")
  }

  object EncodingConfig {
    val default = EncodingConfig()
  }

  /**
    * @param readIntegersAlsoAsFloatingPoint set to false to disable automatic conversion of integer to floating point values
    * @param readDecimalNumbersOnlyAsNumberStrings set to true to disable the fast, immediate conversion of
    *                                              JSON numbers to [[Double]] values where easily possible.
    *                                              In rare cases this might be necessary to allow for maximum
    *                                              possible precision when reading 32-bit [[Float]] values from JSON.
    *                                              (see https://github.com/sirthias/borer/issues/20 for more info on this)
    * @param maxNumberAbsExponent the maximum absolute exponent value to accept in JSON numbers
    * @param maxStringLength the maximum string length to accept
    *                        Note: For performance reasons this is a soft limit, that the parser will sometimes overstep.
    *                        The only guarantee is that it will never accept Strings that are more than twice as long as
    *                        the this limit.
    * @param maxNumberMantissaDigits the maximum number of digits to accept before the exponent in JSON numbers
    */
  final case class DecodingConfig(
      readIntegersAlsoAsFloatingPoint: Boolean = true,
      readDecimalNumbersOnlyAsNumberStrings: Boolean = false,
      maxNumberAbsExponent: Int = 64,
      maxStringLength: Int = 1024 * 1024,
      maxNumberMantissaDigits: Int = 34,
      initialCharbufferSize: Int = 256)
      extends Borer.DecodingConfig with JsonParser.Config {

    Util.requirePositive(maxNumberAbsExponent, "maxNumberAbsExponent")
    Util.requirePositive(maxStringLength, "maxStringLength")
    Util.requirePositive(maxNumberMantissaDigits, "maxNumberMantissaDigits")

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

  def decode[T](input: T)(implicit w: Input.Wrapper[T]): DecodingSetup.Api[w.In, _]
}

/**
  * Main entry point into the CBOR API.
  */
object Borer {

  sealed abstract class EncodingConfig extends Writer.Config {
    def bufferSize: Int
  }

  sealed abstract class DecodingConfig extends Reader.Config

  abstract private[borer] class AbstractSetup[Config](defaultConfig: Config, defaultWrapper: Receiver.Wrapper[Config]) {
    protected var config: Config                            = defaultConfig
    protected var receiverWrapper: Receiver.Wrapper[Config] = defaultWrapper

    final def withConfig(config: Config): this.type = {
      this.config = config
      this
    }

    final def withPrintLogging(maxShownByteArrayPrefixLen: Int, maxShownStringPrefixLen: Int): this.type =
      withWrapper(Logging(Logging.PrintLogger(maxShownByteArrayPrefixLen, maxShownStringPrefixLen)))

    final def withStringLogging(
        stringBuilder: JStringBuilder,
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

  sealed abstract class Error[IO](private var _io: IO, msg: String, cause: Throwable = null)
      extends RuntimeException(msg, cause) {

    final override def getMessage = s"$msg (${_io})"

    final def io: IO = _io

    private[borer] def withPosOf[In <: Input](reader: InputReader[In, _]): Error[In#Position] = {
      val thiz = this.asInstanceOf[Error[In#Position]]
      if (thiz._io.asInstanceOf[AnyRef] eq null) thiz._io = reader.position
      thiz
    }

    private[borer] def withOut(out: Output): Error[Output] = {
      val thiz = this.asInstanceOf[Error[Output]]
      if (thiz._io eq null) thiz._io = out
      thiz
    }
  }

  object Error {
    final class UnexpectedEndOfInput[IO](io: IO, expected: String)
        extends Error[IO](io, s"Expected $expected but got end of input")

    final class InvalidInputData[IO](io: IO, msg: String) extends Error[IO](io, msg) {
      def this(io: IO, expected: String, actual: String) = this(io, s"Expected $expected but got $actual")
    }

    final class ValidationFailure[IO](io: IO, msg: String) extends Error[IO](io, msg)

    final class Unsupported[IO](io: IO, msg: String) extends Error[IO](io, msg)

    final class Overflow[IO](io: IO, msg: String) extends Error[IO](io, msg)

    final class General[IO](io: IO, cause: Throwable) extends Error[IO](io, cause.toString, cause)
  }
}
