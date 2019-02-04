/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.core

import scala.util.control.NonFatal

/**
  * Main entry point into the API.
  */
object Cbor {

  /**
    * Encodes the given `value` into a byte array using the given `config`.
    *
    * @param value the value to encode
    * @param config the config to use
    * @param validationApplier injection point for custom logic around input validation,
    *                          can be used for [[Logging]], for example
    */
  def encode[T](value: T,
                config: Writer.Config = Writer.Config.default,
                validationApplier: Receiver.Applier[Output, Array[Byte]] = Receiver.defaultApplier)(
      implicit enc: Encoder[Array[Byte], T]): Either[Error[Output.ToByteArray], Array[Byte]] =
    generalEncode(value, new Output.ToByteArray, config, validationApplier).map(_.result())

  /**
    * Encodes the given `value` into the given output using the given `config`.
    *
    * @param value the value to encode
    * @param output the [[Output]] to write to
    * @param config the config to use
    * @param validationApplier injection point for custom logic around input validation,
    *                          can be used for [[Logging]], for example
    */
  def generalEncode[T, Bytes](value: T,
                              output: Output[Bytes],
                              config: Writer.Config = Writer.Config.default,
                              validationApplier: Receiver.Applier[Output, Bytes] =
                                Receiver.defaultApplier[Output, Bytes])(
      implicit ba: ByteAccess[Bytes],
      encoder: Encoder[Bytes, T]): Either[Error[output.Self], output.Self] = {

    val writer = new Writer(output, config, validationApplier)
    def out    = writer.output.asInstanceOf[output.Self]
    try {
      encoder.write(writer, value)
      writer.writeEndOfInput() // doesn't actually write anything but triggers certain validation checks (if configured)
      Right(out)
    } catch {
      case e: Error[_] ⇒ Left(e.asInstanceOf[Error[output.Self]])
      case NonFatal(e) ⇒ Left(new Error.General(out, e))
    }
  }

  /**
    * Decodes an instance of [[T]].
    * (This method is actually only the entrypoint to the `from` method of [[DecodingHelper]],
    * which is split off in order to allow for separation of type parameters.)
    */
  def decode[T]: DecodingHelper[T] = helperSingleton.asInstanceOf[DecodingHelper[T]]

  private[this] val helperSingleton = new DecodingHelper[Any]

  final class DecodingHelper[T] private[Cbor] {

    /**
      * Decodes an instance of [[T]] from the given `input` using the given `config`.
      *
      * @param input             the [[Input]] to read from
      * @param prefixOnly        set to true if the decoding process is not expected to consume the complete input
      * @param config            the config to use
      * @param validationApplier injection point for custom logic around input validation,
      *                          can be used for [[Logging]], for example
      */
    def from[Bytes](input: Input[Bytes],
                    prefixOnly: Boolean = false,
                    config: Reader.Config = Reader.Config.default,
                    validationApplier: Receiver.Applier[Input, Bytes] = Receiver.defaultApplier[Input, Bytes])(
        implicit ba: ByteAccess[Bytes],
        decoder: Decoder[Bytes, T]): Either[Error[input.Self], (T, input.Self)] = {

      val reader = new Reader(input, config, validationApplier)
      def in     = reader.input.asInstanceOf[input.Self]
      try {
        reader.pull() // fetch first data item
        val value = decoder.read(reader)
        if (!prefixOnly) reader.readEndOfInput()
        Right(value → in)
      } catch {
        case e: Error[_] ⇒ Left(e.asInstanceOf[Error[input.Self]])
        case NonFatal(e) ⇒ Left(new Error.General(in, e))
      }
    }
  }

  sealed abstract class Error[IO](val io: IO, msg: String, cause: Throwable = null) extends RuntimeException(msg, cause)

  object Error {
    final class InvalidCborData[IO](io: IO, msg: String) extends Error(io, msg)

    final class ValidationFailure[IO](io: IO, msg: String) extends Error(io, msg)

    final class InsufficientInput[IO](io: IO, val length: Long) extends Error(io, "Insufficient Input")

    final class UnexpectedDataItem[IO](io: IO, expected: String, actual: String)
        extends Error(io, s"Unexpected data item: Expected [$expected] but got [$actual]")

    final class Unsupported[IO](io: IO, msg: String) extends Error(io, msg)

    final class Overflow[IO](io: IO, msg: String) extends Error(io, msg)

    final class General[IO](io: IO, cause: Throwable) extends Error(io, cause.toString, cause)
  }
}
