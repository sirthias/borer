/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import java.util
import java.math.{BigDecimal ⇒ JBigDecimal, BigInteger ⇒ JBigInteger}
import scala.collection.immutable.{ListMap, VectorBuilder}

/**
  * Simple Document Object Model (DOM) for CBOR.
  *
  * Practically all valid CBOR encodings can be decoded into this structure and vice versa.
  * Provided as an alternative to plain [[Writer]]-based encoding and [[Reader]]-based decoding.
  */
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

      final case class Int(value: scala.Int)                          extends Value
      final case class Long(value: scala.Long)                        extends Value
      final case class OverLong(negative: Boolean, value: scala.Long) extends Value

      final case class Float16(value: scala.Float) extends Value
      final case class Float(value: scala.Float)   extends Value
      final case class Double(value: scala.Double) extends Value

      final case class BigInteger(value: JBigInteger) extends Value
      final case class BigDecimal(value: JBigDecimal) extends Value

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
      override def toString = value.mkString(if (indefiniteLength) "*[" else "[", ", ", "]")
    }
    object Array {
      def apply(elements: Element*): Array      = new Array(elements.toVector, indefiniteLength = false)
      def indefinite(elements: Element*): Array = new Array(elements.toVector, indefiniteLength = true)
    }

    final case class Map(value: ListMap[Element, Element], indefiniteLength: Boolean = false) extends Element {
      override def toString = value.map(x ⇒ x._1 + ": " + x._2).mkString(if (indefiniteLength) "*{" else "{", ", ", "}")
    }
    object Map {
      def apply(entries: (String, Element)*): Map      = new Map(asMap(entries), indefiniteLength = false)
      def indefinite(entries: (String, Element)*): Map = new Map(asMap(entries), indefiniteLength = true)
      private def asMap(entries: Seq[(String, Element)]): ListMap[Element, Element] =
        entries.foldLeft(ListMap.empty[Element, Element])((m, x) ⇒ m.updated(Value.String(x._1), x._2))
    }

    final case class Tagged(tag: Tag, value: Element) extends Element
  }

  implicit def encoder[T <: Element]: Encoder[T] = elementEncoder.asInstanceOf[Encoder[T]]

  val elementEncoder: Encoder[Element] = {
    val writeElement = (w: Writer, x: Element) ⇒ w.write(x)
    val writeEntry   = (w: Writer, x: (Element, Element)) ⇒ w.write(x._1).write(x._2)

    Encoder {
      case (w, Element.Value.Null)      ⇒ w.writeNull()
      case (w, Element.Value.Undefined) ⇒ w.writeUndefined()
      case (w, Element.Value.Bool(x))   ⇒ w.writeBool(x)

      case (w, Element.Value.Int(x))         ⇒ w.writeInt(x)
      case (w, Element.Value.Long(x))        ⇒ w.writeLong(x)
      case (w, Element.Value.OverLong(n, x)) ⇒ w.writeOverLong(n, x)
      case (w, Element.Value.Float16(x))     ⇒ w.writeFloat16(x)
      case (w, Element.Value.Float(x))       ⇒ w.writeFloat(x)
      case (w, Element.Value.Double(x))      ⇒ w.writeDouble(x)
      case (w, Element.Value.BigInteger(x))  ⇒ w.writeBigInteger(x)
      case (w, Element.Value.BigDecimal(x))  ⇒ w.writeBigDecimal(x)

      case (w, Element.Value.ByteArray(x))   ⇒ w.writeBytes(x)
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

  implicit def decoder[T <: Element]: Decoder[T] = elementDecoder.asInstanceOf[Decoder[T]]

  val elementDecoder: Decoder[Element] = {
    val bytesDecoder: Decoder[Vector[Element.Value.Bytes]] = Decoder { r ⇒
      r.readBytesStart()
      val b = new VectorBuilder[Element.Value.Bytes]
      while (!r.tryReadBreak()) b += r.read[Element.Value.Bytes]()
      b.result()
    }
    val textDecoder: Decoder[Vector[Element.Value.Text]] = Decoder { r ⇒
      r.readTextStart()
      val b = new VectorBuilder[Element.Value.Text]
      while (!r.tryReadBreak()) b += r.read[Element.Value.Text]()
      b.result()
    }

    Decoder { r ⇒
      r.dataItem match {
        case DataItem.Null      ⇒ r.readNull(); Element.Value.Null
        case DataItem.Undefined ⇒ r.readUndefined(); Element.Value.Undefined
        case DataItem.Bool      ⇒ if (r.readBoolean()) Element.Value.Bool.True else Element.Value.Bool.False

        case DataItem.Int      ⇒ Element.Value.Int(r.readInt())
        case DataItem.Long     ⇒ Element.Value.Long(r.readLong())
        case DataItem.OverLong ⇒ Element.Value.OverLong(r.overLongNegative, r.readOverLong())
        case DataItem.Float16  ⇒ Element.Value.Float16(r.readFloat16())
        case DataItem.Float    ⇒ Element.Value.Float(r.readFloat())
        case DataItem.Double   ⇒ Element.Value.Double(r.readDouble())

        case DataItem.BigInteger ⇒ Element.Value.BigInteger(r.readBigInteger())
        case DataItem.BigDecimal ⇒ Element.Value.BigDecimal(r.readBigDecimal())

        case DataItem.Bytes      ⇒ Element.Value.ByteArray(r.readByteArray())
        case DataItem.BytesStart ⇒ Element.Value.BytesStream(r.read()(bytesDecoder))

        case DataItem.Text | DataItem.String ⇒ Element.Value.String(r.readString())
        case DataItem.TextStart              ⇒ Element.Value.TextStream(r.read()(textDecoder))

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
