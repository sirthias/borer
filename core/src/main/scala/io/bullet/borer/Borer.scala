/*
 * Copyright (c) 2019-2023 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import io.bullet.borer.cbor._
import io.bullet.borer.internal.Util
import io.bullet.borer.json._

import scala.annotation.unchecked.uncheckedVariance

case object Cbor extends Target:

  /**
   * Entry point into the CBOR encoding mini-DSL.
   */
  def encode[T: Encoder](value: T): EncodingSetup.Api[EncodingConfig] =
    new EncodingSetup.Impl(value, this, EncodingConfig.default, CborValidation.wrapper, CborRenderer)

  /**
   * Entry point into the CBOR decoding mini-DSL.
   */
  def decode[T](value: T)(implicit p: Input.Provider[T]): DecodingSetup.Api[DecodingConfig] =
    new DecodingSetup.Impl[T, p.Bytes, DecodingConfig](
      value,
      DecodingConfig.default,
      CborValidation.wrapper,
      CborParser.creator[p.Bytes, DecodingConfig],
      this)

  /**
   * Entry point into the CBOR transcoding mini-DSL.
   */
  def transEncode[T: Encoder](value: T): TranscodingSetup.EncodingApi[TransEncodingConfig, TransDecodingConfig] =
    new TranscodingSetup.EncodingApiImpl(
      value,
      this,
      TransEncodingConfig.default,
      CborValidation.wrapper,
      TransDecodingConfig.default,
      CborValidation.wrapper)

  /**
   * Constructs a new [[Writer]] that writes CBOR to the given [[Output]].
   */
  def writer(
      output: Output,
      config: EncodingConfig = EncodingConfig.default,
      receiverWrapper: Receiver.Transformer[EncodingConfig] = CborValidation.wrapper): Writer =
    new Writer(output, receiverWrapper(CborRenderer(output), config), this, config)

  /**
   * Constructs a new [[Reader]] that reads CBOR from the given [[Input]].
   */
  def reader[T](
      value: T,
      config: DecodingConfig = DecodingConfig.default,
      receiverWrapper: Receiver.Transformer[DecodingConfig] = CborValidation.wrapper)(
      implicit p: Input.Provider[T]): Reader =
    new InputReader(new CborParser(p(value))(using p.byteAccess, config), null, receiverWrapper, config, this)

  /**
   * @param bufferSize                  the buffer size used for configuring the respective [[Output]]
   * @param compressFloatingPointValues set to false in order to always write floats as 32-bit values and doubles
   *                                    as 64-bit values, even if they could safely be represented with fewer bits
   * @param maxArrayLength the maximum array length to accept
   * @param maxMapLength the maximum map length to accept
   * @param maxNestingLevels the maximum number of nesting levels to accept
   */
  final case class EncodingConfig(
      bufferSize: Int = 1024,
      allowBufferCaching: Boolean = true,
      compressFloatingPointValues: Boolean = true,
      maxArrayLength: Long = Int.MaxValue,
      maxMapLength: Long = Int.MaxValue,
      maxNestingLevels: Int = 1000)
      extends Borer.EncodingConfig with CborValidation.Config:

    Util.requireNonNegative(maxArrayLength, "maxArrayLength")
    Util.requireNonNegative(maxMapLength, "maxMapLength")
    Util.requireNonNegative(maxNestingLevels, "maxNestingLevels")

    if (bufferSize < 8) throw new IllegalArgumentException(s"bufferSize must be >= 8, but was $bufferSize")

  object EncodingConfig:
    val default = EncodingConfig()

  /**
   * @param readIntegersAlsoAsFloatingPoint set to false to disable automatic conversion of integer to floating point values
   * @param readDoubleAlsoAsFloat set to false to disable automatic conversion of [[Double]] to [[Float]] values
   * @param maxTextStringLength the maximum text string length to accept
   * @param maxByteStringLength the maximum byte string length to accept
   * @param maxArrayLength the maximum array length to accept
   * @param maxMapLength the maximum map length to accept
   * @param maxNestingLevels the maximum number of nesting levels to accept
   */
  final case class DecodingConfig(
      readIntegersAlsoAsFloatingPoint: Boolean = true,
      readDoubleAlsoAsFloat: Boolean = false,
      maxTextStringLength: Int = 1024 * 1024,
      maxByteStringLength: Int = 10 * 1024 * 1024,
      maxArrayLength: Long = Int.MaxValue,
      maxMapLength: Long = Int.MaxValue,
      maxNestingLevels: Int = 1000)
      extends Borer.DecodingConfig with CborValidation.Config with CborParser.Config:

    Util.requireNonNegative(maxTextStringLength, "maxTextStringLength")
    Util.requireNonNegative(maxByteStringLength, "maxByteStringLength")
    Util.requireNonNegative(maxArrayLength, "maxArrayLength")
    Util.requireNonNegative(maxMapLength, "maxMapLength")
    Util.requireNonNegative(maxNestingLevels, "maxNestingLevels")

    if (maxMapLength > Long.MaxValue / 2)
      throw new IllegalArgumentException(s"maxMapLength must be <= ${Long.MaxValue / 2}, but was $maxMapLength")

  object DecodingConfig:
    val default = DecodingConfig()

  /**
   * @param maxBufferSize                  the max number of transcoding elements (not bytes!) that can be buffered
   * @param compressFloatingPointValues set to false in order to always write floats as 32-bit values and doubles
   *                                    as 64-bit values, even if they could safely be represented with fewer bits
   * @param maxArrayLength the maximum array length to accept
   * @param maxMapLength the maximum map length to accept
   * @param maxNestingLevels the maximum number of nesting levels to accept
   */
  final case class TransEncodingConfig(
      maxBufferSize: Int = 16384,
      allowBufferCaching: Boolean = true,
      compressFloatingPointValues: Boolean = true,
      maxArrayLength: Long = Int.MaxValue,
      maxMapLength: Long = Int.MaxValue,
      maxNestingLevels: Int = 1000)
      extends Borer.TransEncodingConfig with CborValidation.Config:

    Util.requireNonNegative(maxArrayLength, "maxArrayLength")
    Util.requireNonNegative(maxMapLength, "maxMapLength")
    Util.requireNonNegative(maxNestingLevels, "maxNestingLevels")

    if (!Util.isPowerOf2(maxBufferSize) || maxBufferSize < 16)
      throw new IllegalArgumentException(s"maxBufferSize must be a power of two >= 16, but was $maxBufferSize")

  object TransEncodingConfig:
    val default = TransEncodingConfig()

  /**
   * @param readIntegersAlsoAsFloatingPoint set to false to disable automatic conversion of integer to floating point values
   * @param readDoubleAlsoAsFloat set to false to disable automatic conversion of [[Double]] to [[Float]] values
   * @param maxArrayLength the maximum array length to accept
   * @param maxMapLength the maximum map length to accept
   * @param maxNestingLevels the maximum number of nesting levels to accept
   */
  final case class TransDecodingConfig(
      readIntegersAlsoAsFloatingPoint: Boolean = true,
      readDoubleAlsoAsFloat: Boolean = false,
      maxArrayLength: Long = Int.MaxValue,
      maxMapLength: Long = Int.MaxValue,
      maxNestingLevels: Int = 1000)
      extends Reader.Config with CborValidation.Config:

    Util.requireNonNegative(maxArrayLength, "maxArrayLength")
    Util.requireNonNegative(maxMapLength, "maxMapLength")
    Util.requireNonNegative(maxNestingLevels, "maxNestingLevels")

    if (maxMapLength > Long.MaxValue / 2)
      throw new IllegalArgumentException(s"maxMapLength must be <= ${Long.MaxValue / 2}, but was $maxMapLength")

  object TransDecodingConfig:
    val default = TransDecodingConfig()

case object Json extends Target:

  /**
   * Entry point into the JSON encoding mini-DSL.
   */
  def encode[T: Encoder](value: T): EncodingSetup.JsonApi[T, EncodingConfig] =
    new EncodingSetup.Impl(value, Json, EncodingConfig.default, Receiver.nopTransformer, JsonRenderer)

  /**
   * Entry point into the JSON decoding mini-DSL.
   */
  def decode[T](value: T)(implicit p: Input.Provider[T]): DecodingSetup.Api[DecodingConfig] =
    new DecodingSetup.Impl[T, p.Bytes, DecodingConfig](
      value,
      DecodingConfig.default,
      Receiver.nopTransformer,
      JsonParser.creator[p.Bytes, DecodingConfig],
      this)

  /**
   * Entry point into the JSON transcoding mini-DSL.
   */
  def transEncode[T: Encoder](value: T): TranscodingSetup.EncodingApi[TransEncodingConfig, TransDecodingConfig] =
    new TranscodingSetup.EncodingApiImpl(
      value,
      this,
      TransEncodingConfig.default,
      Receiver.nopTransformer,
      TransDecodingConfig.default,
      Receiver.nopTransformer)

  /**
   * Constructs a new [[Writer]] that writes JSON to the given [[Output]].
   */
  def writer(
      output: Output,
      config: EncodingConfig = EncodingConfig.default,
      receiverWrapper: Receiver.Transformer[EncodingConfig] = Receiver.nopTransformer): Writer =
    new Writer(output, receiverWrapper(JsonRenderer(output), config), null, config)

  /**
   * Constructs a new [[Reader]] that reads JSON from the given [[Input]].
   */
  def reader[T](
      value: T,
      config: DecodingConfig = DecodingConfig.default,
      receiverWrapper: Receiver.Transformer[DecodingConfig] = Receiver.nopTransformer)(
      implicit p: Input.Provider[T]): Reader =
    val directParser = io.bullet.borer.json.DirectParser(value, config)
    val parser       = if (directParser ne null) null else new JsonParser(p(value), config)(p.byteAccess)
    new InputReader(parser, directParser, receiverWrapper, config, Json)

  final case class EncodingConfig(
      bufferSize: Int = 1024,
      allowBufferCaching: Boolean = true
  ) extends Borer.EncodingConfig:

    def compressFloatingPointValues = false

    if (bufferSize < 8) throw new IllegalArgumentException(s"bufferSize must be >= 8, but was $bufferSize")

  object EncodingConfig:
    val default = EncodingConfig()

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
   * @param initialCharbufferSize the initial size of the parser's Char buffer. Will grow to the max string length in
   *                              the document rounded up to the next power of two
   */
  final case class DecodingConfig(
      readIntegersAlsoAsFloatingPoint: Boolean = true,
      readDecimalNumbersOnlyAsNumberStrings: Boolean = false,
      maxNumberAbsExponent: Int = 64,
      maxStringLength: Int = 1024 * 1024,
      maxNumberMantissaDigits: Int = 34,
      initialCharbufferSize: Int = 2048,
      allowBufferCaching: Boolean = true,
      allowDirectParsing: Boolean = true)
      extends Borer.DecodingConfig with JsonParser.Config:

    Util.requireRange(maxNumberAbsExponent, 1, 999, "maxNumberAbsExponent")
    Util.requirePositive(maxStringLength, "maxStringLength")
    Util.requireRange(maxNumberMantissaDigits, 1, 200, "maxNumberMantissaDigits")
    Util.requirePositive(initialCharbufferSize, "initialCharbufferSize")
    if (!Util.isPowerOf2(initialCharbufferSize))
      throw new IllegalArgumentException(
        s"initialCharbufferSize must be a power of two, but was $initialCharbufferSize")

    // the JsonParser never produces Float values directly (only doubles), so this is necessary
    def readDoubleAlsoAsFloat = true

  object DecodingConfig:
    val default = DecodingConfig()

  /**
   * @param maxBufferSize                  the max number of transcoding elements (not bytes!) that can be buffered
   * @param compressFloatingPointValues set to false in order to always write floats as 32-bit values and doubles
   *                                    as 64-bit values, even if they could safely be represented with fewer bits
   */
  final case class TransEncodingConfig(
      maxBufferSize: Int = 16384,
      allowBufferCaching: Boolean = true,
      compressFloatingPointValues: Boolean = true)
      extends Borer.TransEncodingConfig:

    if (!Util.isPowerOf2(maxBufferSize) || maxBufferSize < 16)
      throw new IllegalArgumentException(s"maxBufferSize must be a power of two >= 16, but was $maxBufferSize")

  object TransEncodingConfig:
    val default = TransEncodingConfig()

  /**
   * @param readIntegersAlsoAsFloatingPoint set to false to disable automatic conversion of integer to floating point values
   * @param readDoubleAlsoAsFloat set to false to disable automatic conversion of [[Double]] to [[Float]] values
   */
  final case class TransDecodingConfig(
      readIntegersAlsoAsFloatingPoint: Boolean = true,
      readDoubleAlsoAsFloat: Boolean = false)
      extends Reader.Config

  object TransDecodingConfig:
    val default = TransDecodingConfig()

/**
 * Super-type of the [[Cbor]] and [[Json]] objects.
 *
 * Used, for example, as the type of the `target` member of [[Reader]] and [[Writer]] instances,
 * which allows custom logic to pick different (de)serialization approaches
 * depending on whether the target is CBOR or JSON.
 */
sealed abstract class Target:

  def encode[T: Encoder](value: T): EncodingSetup.Api[_]

  def decode[T](input: T)(implicit w: Input.Provider[T]): DecodingSetup.Api[_]

  def transEncode[T: Encoder](value: T): TranscodingSetup.EncodingApi[_, _]

/**
 * Main entry point into the CBOR API.
 */
object Borer:

  sealed abstract class EncodingConfig extends Writer.Config:
    def bufferSize: Int
    def allowBufferCaching: Boolean

  sealed abstract class TransEncodingConfig extends Writer.Config:
    def maxBufferSize: Int
    def allowBufferCaching: Boolean

  sealed abstract class DecodingConfig extends Reader.Config

  sealed abstract class Error[+IO](private var _io: IO @uncheckedVariance, msg: String, cause: Throwable = null)
      extends RuntimeException(msg, cause):

    final override def getMessage = s"$msg (${_io})"

    final def io: IO = _io

    private[borer] def withPosOf(reader: Reader): Error[Input.Position] =
      val thiz = this.asInstanceOf[Error[Input.Position]]
      if (thiz._io.asInstanceOf[AnyRef] eq null) thiz._io = reader.position
      thiz

    private[borer] def withOut(out: Output): Error[Output] =
      val thiz = this.asInstanceOf[Error[Output]]
      if (thiz._io eq null) thiz._io = out
      thiz

    private[borer] def withUnit: Error[Unit] =
      val thiz = this.asInstanceOf[Error[Unit]]
      thiz._io = ()
      thiz

  object Error:

    final class UnexpectedEndOfInput[IO](io: IO, expected: String)
        extends Error[IO](io, s"Expected $expected but got end of input")

    final class InvalidInputData[IO](io: IO, msg: String) extends Error[IO](io, msg):
      def this(io: IO, expected: String, actual: String) = this(io, s"Expected $expected but got $actual")

    final class ValidationFailure[IO](io: IO, msg: String) extends Error[IO](io, msg)

    final class Unsupported[IO](io: IO, msg: String) extends Error[IO](io, msg)

    final class Overflow[IO](io: IO, msg: String) extends Error[IO](io, msg)

    final class General[IO](io: IO, cause: Throwable) extends Error[IO](io, cause.toString, cause)
