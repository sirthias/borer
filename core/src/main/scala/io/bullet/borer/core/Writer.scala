/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.core

import java.nio.charset.StandardCharsets
import StandardCharsets.UTF_8

/**
  * Stateful, mutable abstraction for writing a stream of CBOR data to the given [[Output]].
  */
final class Writer(startOutput: Output,
                   config: Writer.Config = Writer.Config(),
                   validationApplier: Receiver.Applier[Output] = Receiver.defaultApplier) {

  private[this] var _output: Output            = startOutput
  private[this] val receiver: Receiver[Output] = validationApplier(Validation.creator(config.validation), ByteWriter)

  def output: Output = _output

  def ~(value: Boolean): this.type = writeBool(value)
  def ~(value: Char): this.type    = writeChar(value)
  def ~(value: Byte): this.type    = writeByte(value)
  def ~(value: Short): this.type   = writeShort(value)
  def ~(value: Int): this.type     = writeInt(value)
  def ~(value: Long): this.type    = writeLong(value)
  def ~(value: Float): this.type   = writeFloat(value)
  def ~(value: Double): this.type  = writeDouble(value)
  def ~(value: String): this.type  = writeString(value)

  def ~[T: Encoder](value: T): this.type = write(value)

  def writeNull(): this.type      = ret(receiver.onNull(_output))
  def writeUndefined(): this.type = ret(receiver.onUndefined(_output))

  def writeBool(value: Boolean): this.type     = ret(receiver.onBool(_output, value))
  def writeChar(value: Char): this.type        = writeInt(value.toInt)
  def writeByte(value: Byte): this.type        = writeInt(value.toInt)
  def writeShort(value: Short): this.type      = writeInt(value.toInt)
  def writeInt(value: Int): this.type          = ret(receiver.onInt(_output, value.toInt))
  def writeLong(value: Long): this.type        = ret(receiver.onLong(_output, value))
  def writePosOverLong(value: Long): this.type = ret(receiver.onPosOverLong(_output, value))
  def writeNegOverLong(value: Long): this.type = ret(receiver.onNegOverLong(_output, value))
  def writeFloat16(value: Float): this.type    = ret(receiver.onFloat16(_output, value))

  def writeFloat(value: Float): this.type = ret {
    if (config.dontCompressFloatingPointValues || !Util.canBeRepresentedAsFloat16(value)) {
      receiver.onFloat(_output, value)
    } else receiver.onFloat16(_output, value)
  }

  def writeDouble(value: Double): this.type =
    if (config.dontCompressFloatingPointValues || !Util.canBeRepresentedAsFloat(value)) {
      ret(receiver.onDouble(_output, value))
    } else writeFloat(value.toFloat)

  def writeString(value: String): this.type                  = writeText(value getBytes UTF_8)
  def writeBytes[Bytes: ByteAccess](value: Bytes): this.type = ret(receiver.onBytes(_output, value))
  def writeText[Bytes: ByteAccess](value: Bytes): this.type  = ret(receiver.onText(_output, value))
  def writeTag(value: Tag): this.type                        = ret(receiver.onTag(_output, value))
  def writeSimpleValue(value: Int): this.type                = ret(receiver.onSimpleValue(_output, value))

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

  def write[T](value: T)(implicit encoder: Encoder[T]): this.type = encoder.write(this, value)

  private def ret(out: Output): this.type = {
    _output = out
    this
  }
}

object Writer {

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

  /**
    * Simple encapsulation of encoding logic in a stand-alone object.
    */
  final case class Script(encode: Writer ⇒ Unit)

  object Script {
    val Undefined  = Script(_.writeUndefined())
    val BytesStart = Script(_.writeBytesStart())
    val TextStart  = Script(_.writeTextStart())
    val ArrayStart = Script(_.writeArrayStart())
    val MapStart   = Script(_.writeMapStart())
    val Break      = Script(_.writeBreak())

    implicit val encoder: Encoder[Script] = Encoder((w, x) ⇒ x.encode(w))
  }
}
