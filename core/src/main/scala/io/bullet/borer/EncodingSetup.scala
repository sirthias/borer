/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import java.nio.charset.StandardCharsets.UTF_8
import java.lang.{StringBuilder ⇒ JStringBuilder}

import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal

object EncodingSetup {

  sealed trait Api[T, Config <: Writer.Config] {

    /**
      * Configures the [[Config]] for this encoding run.
      */
    def withConfig(config: Config): this.type

    /**
      * Enables logging of the encoding progress to the console.
      * Each data item that is written by the application is pretty printed to the console on its own line.
      */
    def withPrintLogging(maxShownByteArrayPrefixLen: Int = 20, maxShownStringPrefixLen: Int = 50): this.type

    /**
      * Enables logging of the encoding progress to the given [[JStringBuilder]].
      * Each data item that is written by the application is formatted and appended as its own line.
      */
    def withStringLogging(stringBuilder: JStringBuilder,
                          maxShownByteArrayPrefixLen: Int = 20,
                          maxShownStringPrefixLen: Int = 50,
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
      * Encodes an instance of [[T]] to the given `Bytes` type using the configured options.
      */
    def to[Bytes](implicit ba: ByteAccess[Bytes]): Sealed[ba.Out, ba.Out#Result]
  }

  sealed trait Sealed[Out <: Output, Result] {

    def bytes: Result

    def bytesTry: Try[Result]

    def bytesEither: Either[Borer.Error[Out], Result]

    def output: Out

    def outputTry: Try[Out]

    def outputEither: Either[Borer.Error[Out], Out]
  }

  sealed trait JsonApi[T, Config <: Writer.Config] extends Api[T, Config] {

    /**
      * Short-cut for encoding to a plain byte array, throwing an exception in case of any failures,
      * and then immediately UTF-8 decoding into a [[String]].
      */
    @inline def toUtf8String: String
  }

  private[borer] final class Impl[T: Encoder, Config <: Writer.Config](value: T,
                                                                       target: Target,
                                                                       defaultConfig: Config,
                                                                       defaultWrapper: Receiver.Wrapper[Config],
                                                                       rendererCreator: Output ⇒ Receiver.Renderer)
      extends Borer.AbstractSetup[Config](defaultConfig, defaultWrapper) with JsonApi[T, Config]
      with Sealed[Output, AnyRef] {

    private[this] var byteAccess: ByteAccess[_] = _

    def toUtf8String: String = new String(toByteArray, UTF_8)

    def toByteArray: Array[Byte] = to[Array[Byte]].bytes

    def toByteArrayTry: Try[Array[Byte]] = to[Array[Byte]].bytesTry

    def to[Bytes](implicit ba: ByteAccess[Bytes]): Sealed[ba.Out, ba.Out#Result] = {
      byteAccess = ba
      this.asInstanceOf[Sealed[ba.Out, ba.Out#Result]]
    }

    def bytes: AnyRef = {
      val renderer = rendererCreator(byteAccess.newOutput)
      try {
        render(renderer).out.result()
      } catch {
        case e: Borer.Error[_] ⇒ throw e.withOut(renderer.out)
        case NonFatal(e)       ⇒ throw new Borer.Error.General(renderer.out, e)
      }
    }

    def bytesTry: Try[AnyRef] = {
      val renderer = rendererCreator(byteAccess.newOutput)
      try {
        Success(render(renderer).out.result())
      } catch {
        case e: Borer.Error[_] ⇒ Failure(e.withOut(renderer.out))
        case NonFatal(e)       ⇒ Failure(new Borer.Error.General(renderer.out, e))
      }
    }

    def bytesEither: Either[Borer.Error[Output], AnyRef] = {
      val renderer = rendererCreator(byteAccess.newOutput)
      try {
        Right(render(renderer).out.result())
      } catch {
        case e: Borer.Error[_] ⇒ Left(e.withOut(renderer.out))
        case NonFatal(e)       ⇒ Left(new Borer.Error.General(renderer.out, e))
      }
    }

    def output: Output = render(rendererCreator(byteAccess.newOutput)).out

    def outputTry: Try[Output] = {
      val renderer = rendererCreator(byteAccess.newOutput)
      try {
        Success(render(renderer).out)
      } catch {
        case e: Borer.Error[_] ⇒ Failure(e.withOut(renderer.out))
        case NonFatal(e)       ⇒ Failure(new Borer.Error.General(renderer.out, e))
      }
    }

    def outputEither: Either[Borer.Error[Output], Output] = {
      val renderer = rendererCreator(byteAccess.newOutput)
      try {
        Right(render(renderer).out)
      } catch {
        case e: Borer.Error[_] ⇒ Left(e.withOut(renderer.out))
        case NonFatal(e)       ⇒ Left(new Borer.Error.General(renderer.out, e))
      }
    }

    private def render(renderer: Receiver.Renderer): Receiver.Renderer = {
      val writer = new Writer(receiverWrapper(renderer, config), target, config)
      writer
        .write(value)
        .writeEndOfInput() // doesn't actually write anything but triggers certain validation checks (if configured)
      renderer
    }
  }
}
