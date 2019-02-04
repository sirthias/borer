/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.core

import java.util

import scala.collection.immutable.{ListMap, VectorBuilder}

object Dom {

  sealed trait Element

  object Element {

    sealed trait Value extends Element

    object Value {
      final case object Null                extends Value
      final case object Undefined           extends Value
      final case class Bool(value: Boolean) extends Value

      object Bool {
        val True  = Bool(true)
        val False = Bool(false)
      }

      final case class Int(value: scala.Int)          extends Value
      final case class Long(value: scala.Long)        extends Value
      final case class PosOverLong(value: scala.Long) extends Value
      final case class NegOverLong(value: scala.Long) extends Value

      final case class Float16(value: scala.Float) extends Value
      final case class Float(value: scala.Float)   extends Value
      final case class Double(value: scala.Double) extends Value

      sealed trait Bytes extends Value

      final case class ByteArray(value: scala.Array[Byte]) extends Bytes {
        override def hashCode() = util.Arrays.hashCode(value)
        override def equals(obj: Any) = obj match {
          case ByteArray(x) ⇒ util.Arrays.equals(value, x)
          case _            ⇒ false
        }
      }

      final case class BytesStream(value: Vector[Bytes]) extends Bytes

      sealed trait Text                                extends Value
      final case class String(value: java.lang.String) extends Text
      final case class TextStream(value: Vector[Text]) extends Text

      final case class Simple(value: SimpleValue) extends Value
    }

    final case class Array(value: Vector[Element], indefiniteLength: Boolean = false) extends Element {
      override def toString = value.mkString("[", ", ", "]")
    }
    object Array {
      def apply(elements: Element*): Array = new Array(elements.toVector, indefiniteLength = false)
    }

    final case class Map(value: ListMap[Element, Element], indefiniteLength: Boolean = false) extends Element {
      override def toString = value.map(x ⇒ x._1 + ": " + x._2).mkString("{", ", ", "}")
    }
    object Map {
      def apply(entries: (String, Element)*): Map =
        new Map(
          value = entries.foldLeft(ListMap.empty[Element, Element])((m, x) ⇒ m.updated(Value.String(x._1), x._2)),
          indefiniteLength = false)
    }

    final case class Tagged(tag: Tag, value: Element) extends Element
  }

  implicit def encoder[T <: Element]: Encoder.Universal[T] = elementEncoder.asInstanceOf[Encoder.Universal[T]]

  val elementEncoder: Encoder.Universal[Element] = {
    val writeElement = (w: Writer.Universal, x: Element) ⇒ w.write(x)
    val writeEntry   = (w: Writer.Universal, x: (Element, Element)) ⇒ w.write(x._1).write(x._2)

    Encoder.of[Element].from {
      case (w, Element.Value.Null)      ⇒ w.writeNull()
      case (w, Element.Value.Undefined) ⇒ w.writeUndefined()
      case (w, Element.Value.Bool(x))   ⇒ w.writeBool(x)

      case (w, Element.Value.Int(x))         ⇒ w.writeInt(x)
      case (w, Element.Value.Long(x))        ⇒ w.writeLong(x)
      case (w, Element.Value.PosOverLong(x)) ⇒ w.writePosOverLong(x)
      case (w, Element.Value.NegOverLong(x)) ⇒ w.writeNegOverLong(x)
      case (w, Element.Value.Float16(x))     ⇒ w.writeFloat16(x)
      case (w, Element.Value.Float(x))       ⇒ w.writeFloat(x)
      case (w, Element.Value.Double(x))      ⇒ w.writeDouble(x)

      case (w, Element.Value.ByteArray(x))   ⇒ w.writeByteArray(x)
      case (w, Element.Value.BytesStream(x)) ⇒ x.foldLeft(w.writeBytesStart())(writeElement).writeBreak()

      case (w, Element.Value.String(x))     ⇒ w.writeString(x)
      case (w, Element.Value.TextStream(x)) ⇒ x.foldLeft(w.writeTextStart())(writeElement).writeBreak()

      case (w, Element.Value.Simple(x)) ⇒ w.writeSimpleValue(x.value)

      case (w, Element.Array(x, false)) ⇒ x.foldLeft(w.writeArrayHeader(x.size))(writeElement)
      case (w, Element.Array(x, true))  ⇒ x.foldLeft(w.writeArrayStart())(writeElement).writeBreak()

      case (w, Element.Map(x, false)) ⇒ x.foldLeft(w.writeMapHeader(x.size))(writeEntry)
      case (w, Element.Map(x, true))  ⇒ x.foldLeft(w.writeMapStart())(writeEntry).writeBreak()

      case (w, Element.Tagged(tag, x)) ⇒ w.writeTag(tag).write(x)
    }
  }

  implicit def decoder[T <: Element]: Decoder.Universal[T] = elementDecoder.asInstanceOf[Decoder.Universal[T]]

  val elementDecoder: Decoder.Universal[Element] = {
    val bytesDecoder = Decoder.of[Vector[Element.Value.Bytes]].from { r ⇒
      r.readBytesStart()
      val b = new VectorBuilder[Element.Value.Bytes]
      while (!r.tryReadBreak()) b += r.read[Element.Value.Bytes]()
      b.result()
    }
    val textDecoder = Decoder.of[Vector[Element.Value.Text]].from { r ⇒
      r.readTextStart()
      val b = new VectorBuilder[Element.Value.Text]
      while (!r.tryReadBreak()) b += r.read[Element.Value.Text]()
      b.result()
    }

    Decoder.of[Element].from { r ⇒
      r.dataItem match {
        case DataItem.Null      ⇒ r.readNull(); Element.Value.Null
        case DataItem.Undefined ⇒ r.readUndefined(); Element.Value.Undefined
        case DataItem.Bool      ⇒ if (r.readBoolean()) Element.Value.Bool.True else Element.Value.Bool.False

        case DataItem.Int         ⇒ Element.Value.Int(r.readInt())
        case DataItem.Long        ⇒ Element.Value.Long(r.readLong())
        case DataItem.PosOverLong ⇒ Element.Value.PosOverLong(r.readPosOverLong())
        case DataItem.NegOverLong ⇒ Element.Value.NegOverLong(r.readNegOverLong())
        case DataItem.Float16     ⇒ Element.Value.Float16(r.readFloat16())
        case DataItem.Float       ⇒ Element.Value.Float(r.readFloat())
        case DataItem.Double      ⇒ Element.Value.Double(r.readDouble())

        case DataItem.Bytes      ⇒ Element.Value.ByteArray(r.readByteArray())
        case DataItem.BytesStart ⇒ Element.Value.BytesStream(r.read()(bytesDecoder))

        case DataItem.Text      ⇒ Element.Value.String(r.readString())
        case DataItem.TextStart ⇒ Element.Value.TextStream(r.read()(textDecoder))

        case DataItem.SimpleValue ⇒ Element.Value.Simple(SimpleValue(r.readSimpleValue()))

        case DataItem.ArrayHeader ⇒ Element.Array(r.read[Vector[Element]]())
        case DataItem.ArrayStart  ⇒ Element.Array(r.read[Vector[Element]](), indefiniteLength = true)

        case DataItem.MapHeader ⇒ Element.Map(r.read[ListMap[Element, Element]]())
        case DataItem.MapStart  ⇒ Element.Map(r.read[ListMap[Element, Element]](), indefiniteLength = true)

        case DataItem.Tag ⇒ Element.Tagged(r.readTag(), r.read[Element]())
      }
    }
  }
}
