/*
 * Copyright (c) 2019-2021 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import java.nio.charset.StandardCharsets.UTF_8
import java.lang.{StringBuilder => JStringBuilder}
import java.nio.ByteBuffer

import io.bullet.borer.internal.Renderer

import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal

object EncodingSetup {

  sealed trait Api[Config <: Borer.EncodingConfig] {

    /**
      * Configures the [[Config]] for this encoding run.
      */
    def withConfig(config: Config): this.type

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
    def withWrapper(receiverWrapper: Receiver.Wrapper[Config]): this.type

    /**
      * Short-cut for encoding to a plain byte array, throwing an exception in case of any failures.
      */
    def toByteArray: Array[Byte]

    /**
      * Short-cut for encoding to a plain byte array, wrapped in a [[Try]] for error handling.
      */
    def toByteArrayTry: Try[Array[Byte]]

    /**
      * Short-cut for encoding to a plain byte array, wrapped in an [[Either]] for error handling.
      *
      * NOTE: You can get a slightly more narrowly typed error by using `.to[Array[Byte]].resultEither` instead!
      */
    def toByteArrayEither: Either[Borer.Error[Output], Array[Byte]]

    /**
      * Short-cut for encoding to a [[ByteBuffer]], throwing an exception in case of any failures.
      */
    def toByteBuffer: ByteBuffer

    /**
      * Short-cut for encoding to a [[ByteBuffer]], wrapped in a [[Try]] for error handling.
      */
    def toByteBufferTry: Try[ByteBuffer]

    /**
      * Short-cut for encoding to a [[ByteBuffer]], wrapped in an [[Either]] for error handling.
      *
      * NOTE: You can get a slightly more narrowly typed error by using `.to[ByteBuffer].resultEither` instead!
      */
    def toByteBufferEither: Either[Borer.Error[Output], ByteBuffer]

    /**
      * Encodes an instance of T to the given type `R` type using the configured options.
      */
    def to[R](implicit op: Output.ToTypeProvider[R]): Sealed[op.Out, R]

    /**
      * Encodes an instance of T to the given `target` using the configured options.
      */
    def to[R](target: R)(implicit op: Output.ToValueProvider[R]): Sealed[op.Out, R]
  }

  sealed trait Sealed[Out <: Output, Result] {

    def result: Result

    def resultTry: Try[Result]

    def resultEither: Either[Borer.Error[Out], Result]

    def output: Out

    def outputTry: Try[Out]

    def outputEither: Either[Borer.Error[Out], Out]
  }

  sealed trait JsonApi[T, Config <: Borer.EncodingConfig] extends Api[Config] {

    /**
      * Short-cut for encoding to a plain byte array, throwing an exception in case of any failures,
      * and then immediately UTF-8 decoding into a [[String]].
      */
    @inline def toUtf8String: String
  }

  final private[borer] class Impl[T: Encoder, Config <: Borer.EncodingConfig](
      value: T,
      target: Target,
      defaultConfig: Config,
      defaultWrapper: Receiver.Wrapper[Config],
      rendererCreator: Output => Renderer)
      extends Borer.AbstractSetup[Config](defaultConfig, defaultWrapper) with JsonApi[T, Config]
      with Sealed[Output, AnyRef] {

    private[this] var _output: Output = _

    def toUtf8String: String = new String(toByteArray, UTF_8)

    def toByteArray: Array[Byte] = to[Array[Byte]].result

    def toByteArrayTry: Try[Array[Byte]] = to[Array[Byte]].resultTry

    def toByteArrayEither: Either[Borer.Error[Output], Array[Byte]] = to[Array[Byte]].resultEither

    def toByteBuffer: ByteBuffer = to[ByteBuffer].result

    def toByteBufferTry: Try[ByteBuffer] = to[ByteBuffer].resultTry

    def toByteBufferEither: Either[Borer.Error[Output], ByteBuffer] = to[ByteBuffer].resultEither

    def to[R](implicit op: Output.ToTypeProvider[R]): Sealed[op.Out, R] = {
      _output = op(config.bufferSize, config.allowBufferCaching)
      this.asInstanceOf[Sealed[op.Out, R]]
    }

    def to[R](target: R)(implicit op: Output.ToValueProvider[R]): Sealed[op.Out, R] = {
      _output = op(target, config.bufferSize, config.allowBufferCaching)
      this.asInstanceOf[Sealed[op.Out, R]]
    }

    def result: AnyRef = {
      val renderer = rendererCreator(_output)
      try {
        render(renderer).out.result()
      } catch {
        case e: Borer.Error[_] => throw e.withOut(renderer.out)
        case NonFatal(e)       => throw new Borer.Error.General(renderer.out, e)
      }
    }

    def resultTry: Try[AnyRef] = {
      val renderer = rendererCreator(_output)
      try {
        Success(render(renderer).out.result())
      } catch {
        case e: Borer.Error[_] => Failure(e.withOut(renderer.out))
        case NonFatal(e)       => Failure(new Borer.Error.General(renderer.out, e))
      }
    }

    def resultEither: Either[Borer.Error[Output], AnyRef] = {
      val renderer = rendererCreator(_output)
      try {
        Right(render(renderer).out.result())
      } catch {
        case e: Borer.Error[_] => Left(e.withOut(renderer.out))
        case NonFatal(e)       => Left(new Borer.Error.General(renderer.out, e))
      }
    }

    def output: Output = render(rendererCreator(_output)).out

    def outputTry: Try[Output] = {
      val renderer = rendererCreator(_output)
      try {
        Success(render(renderer).out)
      } catch {
        case e: Borer.Error[_] => Failure(e.withOut(renderer.out))
        case NonFatal(e)       => Failure(new Borer.Error.General(renderer.out, e))
      }
    }

    def outputEither: Either[Borer.Error[Output], Output] = {
      val renderer = rendererCreator(_output)
      try {
        Right(render(renderer).out)
      } catch {
        case e: Borer.Error[_] => Left(e.withOut(renderer.out))
        case NonFatal(e)       => Left(new Borer.Error.General(renderer.out, e))
      }
    }

    private def render(renderer: Renderer): Renderer = {
      val writer = new Writer(_output, receiverWrapper(renderer, config), target, config)
      writer
        .write(value)
        .writeEndOfInput() // doesn't actually write anything but triggers certain validation checks (if configured)
      renderer
    }
  }
}
