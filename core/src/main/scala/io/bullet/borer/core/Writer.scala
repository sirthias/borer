/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.core

import java.nio.charset.StandardCharsets
import scala.annotation.unchecked.uncheckedVariance
import StandardCharsets.UTF_8

/**
  * Stateful, mutable abstraction for writing a stream of CBOR data to the given [[Output]].
  *
  * @tparam Bytes The abstraction for byte chunks that the wrapped [[Output]] consumes.
  */
final class Writer[-Bytes](startOutput: Output[Bytes],
                           config: Writer.Config,
                           validationApplier: Receiver.Applier[Output, Bytes])(implicit byteAccess: ByteAccess[Bytes]) {

  private[this] var _output: Output[Bytes] = startOutput
  private[this] val byteWriter             = new ByteWriter[Bytes](config)
  private[this] val receiver: Receiver[Output[Bytes], Bytes] =
    validationApplier(Validation.creator(config.validation), byteWriter)

  def output: Output[Bytes @uncheckedVariance] = _output

  def ~(value: Boolean): this.type = writeBool(value)
  def ~(value: Char): this.type    = writeChar(value)
  def ~(value: Byte): this.type    = writeByte(value)
  def ~(value: Short): this.type   = writeShort(value)
  def ~(value: Int): this.type     = writeInt(value)
  def ~(value: Long): this.type    = writeLong(value)
  def ~(value: Float): this.type   = writeFloat(value)
  def ~(value: Double): this.type  = writeDouble(value)
  def ~(value: String): this.type  = writeString(value)
  def ~(value: Bytes): this.type   = writeBytes(value)

  def ~[T](value: T)(implicit encoder: Encoder[Bytes, T]): this.type = write(value)

  def writeNull(): this.type      = ret(receiver.onNull(_output))
  def writeUndefined(): this.type = ret(receiver.onUndefined(_output))

  def writeBool(value: Boolean): this.type              = ret(receiver.onBool(_output, value))
  def writeChar(value: Char): this.type                 = writeInt(value.toInt)
  def writeByte(value: Byte): this.type                 = writeInt(value.toInt)
  def writeShort(value: Short): this.type               = writeInt(value.toInt)
  def writeInt(value: Int): this.type                   = ret(receiver.onInt(_output, value.toInt))
  def writeLong(value: Long): this.type                 = ret(receiver.onLong(_output, value))
  def writePosOverLong(value: Long): this.type          = ret(receiver.onPosOverLong(_output, value))
  def writeNegOverLong(value: Long): this.type          = ret(receiver.onNegOverLong(_output, value))
  def writeFloat16(value: Float): this.type             = ret(receiver.onFloat16(_output, value))
  def writeFloat(value: Float): this.type               = ret(receiver.onFloat(_output, value))
  def writeDouble(value: Double): this.type             = ret(receiver.onDouble(_output, value))
  def writeString(value: String): this.type             = writeTextByteArray(value getBytes UTF_8)
  def writeText(value: Bytes): this.type                = ret(receiver.onText(_output, value))
  def writeTextByteArray(value: Array[Byte]): this.type = ret(receiver.onTextByteArray(_output, value))
  def writeBytes(value: Bytes): this.type               = ret(receiver.onBytes(_output, value))
  def writeByteArray(value: Array[Byte]): this.type     = ret(receiver.onByteArray(_output, value))
  def writeTag(value: Tag): this.type                   = ret(receiver.onTag(_output, value))
  def writeSimpleValue(value: Int): this.type           = ret(receiver.onSimpleValue(_output, value))

  def writeBytesStart(): this.type = ret(receiver.onBytesStart(_output))
  def writeTextStart(): this.type  = ret(receiver.onTextStart(_output))

  def writeArrayHeader(length: Int): this.type  = writeArrayHeader(length.toLong)
  def writeArrayHeader(length: Long): this.type = ret(receiver.onArrayHeader(_output, length))
  def writeArrayStart(): this.type              = ret(receiver.onArrayStart(_output))

  def writeMapHeader(length: Int): this.type  = writeMapHeader(length.toLong)
  def writeMapHeader(length: Long): this.type = ret(receiver.onMapHeader(_output, length))
  def writeMapStart(): this.type              = ret(receiver.onMapStart(_output))

  def writeBreak(): this.type = ret(receiver.onBreak(_output))

  def writeEndOfInput(): this.type = ret(receiver.onEndOfInput(_output))

  def write[T](value: T)(implicit encoder: Encoder[Bytes, T]): this.type = encoder.write(this, value)

  private def ret(out: Output[Bytes @uncheckedVariance]): this.type = {
    _output = out
    this
  }
}

object Writer {

  type Universal = Writer[Nothing]

  /**
    * Serialization config settings
    *
    * @param validation the validation settings to use or `None` if no validation should be performed
    * @param dontCompressFloatingPointValues set to true in order to always write floats as 32-bit values and doubles
    *                                        as 64-bit values, even if they could safely be represented with fewer bits
    */
  final case class Config(
      validation: Option[Validation.Config] = Some(Validation.Config()),
      dontCompressFloatingPointValues: Boolean = false
  )

  object Config {
    val default = Config()
  }

  def script(encode: Writer.Universal ⇒ Any): Script.Universal = Script(encode)

  final case class Script[Bytes](encode: Writer[Bytes] ⇒ Any)

  object Script {
    type Universal = Script[Nothing]

    val Undefined  = script(_.writeUndefined())
    val BytesStart = script(_.writeBytesStart())
    val TextStart  = script(_.writeTextStart())
    val ArrayStart = script(_.writeArrayStart())
    val MapStart   = script(_.writeMapStart())
    val Break      = script(_.writeBreak())

    implicit def encoder[Bytes]: Encoder[Bytes, Script[Bytes]] = Encoder((w, x) ⇒ x.encode(w))
  }

  /**
    * Allows for concise [[Encoder]] definition for case classes, without any macro magic.
    * Can be used e.g. like this:
    *
    * {{{
    * case class Foo(int: Int, string: String, doubleOpt: Option[Double])
    *
    * val fooEncoder = Encoder(Foo.unapply) // if you only need an `Encoder` for `Foo`
    * val fooCodec = Codec.of[Foo](Foo.unapply, Foo.apply) // if you need a full `Codec` for `Foo`
    * }}}
    */
  implicit def encoderFuncFromUnapply[T, Tuple](unapply: T ⇒ Option[Tuple])(
      implicit tupleEncoder: Encoder[Nothing, Tuple]): (Writer.Universal, T) ⇒ Unit =
    (w, x) ⇒ tupleEncoder.write(w, unapply(x).get)

  implicit def encoderFuncFromUnapply0[T](unapply: T ⇒ Boolean): (Writer.Universal, T) ⇒ Unit =
    (w, x) ⇒ if (unapply(x)) w.writeArrayHeader(0) else sys.error("Unapply unexpectedly failed: " + unapply)
}
