/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import java.util

import scala.collection.immutable

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

      final case class Float16(value: scala.Float)           extends Value
      final case class Float(value: scala.Float)             extends Value
      final case class Double(value: scala.Double)           extends Value
      final case class NumberString(value: java.lang.String) extends Value

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

    sealed trait Array extends Element {
      def elements: Vector[Element]
    }

    object Array {
      final case class Sized(elements: Vector[Element]) extends Array {
        override def toString = elements.mkString("[", ", ", "]")
      }
      object Sized {
        def apply(elements: Element*) = new Sized(elements.toVector)
      }

      final case class Unsized(elements: Vector[Element]) extends Array {
        override def toString = elements.mkString("*[", ", ", "]")
      }
      object Unsized {
        def apply(elements: Element*) = new Unsized(elements.toVector)
      }
    }

    sealed trait Map extends Element {
      def entries: immutable.Map[Element, Element]
    }

    object Map {
      final case class Sized(entries: immutable.Map[Element, Element]) extends Map {
        override def toString = entries.map(x ⇒ x._1 + ": " + x._2).mkString("{", ", ", "}")
      }
      object Sized {
        def apply(entries: (String, Element)*) = new Sized(asListMap(entries))
      }

      final case class Unsized(entries: immutable.Map[Element, Element]) extends Map {
        override def toString = entries.map(x ⇒ x._1 + ": " + x._2).mkString("*{", ", ", "}")
      }
      object Unsized {
        def apply(entries: (String, Element)*) = new Unsized(asListMap(entries))
      }

      private def asListMap(entries: Seq[(String, Element)]): immutable.ListMap[Element, Element] =
        entries.foldLeft(immutable.ListMap.empty[Element, Element])((m, x) ⇒ m.updated(Value.String(x._1), x._2))
    }

    final case class Tagged(tag: Tag, value: Element) extends Element
  }

  implicit def encoder[T <: Element]: Encoder[T] = elementEncoder.asInstanceOf[Encoder[T]]

  val elementEncoder: Encoder[Element] = {
    val writeElement = (w: Writer, x: Element) ⇒ w.write(x)
    val writeEntry   = (w: Writer, x: (Element, Element)) ⇒ w.write(x._1).write(x._2)

    Encoder {
      case (w, Element.Value.String(x))  ⇒ w.writeString(x)
      case (w, Element.Array.Unsized(x)) ⇒ x.foldLeft(w.writeArrayStart())(writeElement).writeBreak()
      case (w, Element.Array.Sized(x))   ⇒ x.foldLeft(w.writeArrayHeader(x.size))(writeElement)
      case (w, Element.Map.Unsized(x))   ⇒ x.foldLeft(w.writeMapStart())(writeEntry).writeBreak()
      case (w, Element.Map.Sized(x))     ⇒ x.foldLeft(w.writeMapHeader(x.size))(writeEntry)

      case (w, Element.Value.Int(x))          ⇒ w.writeInt(x)
      case (w, Element.Value.Long(x))         ⇒ w.writeLong(x)
      case (w, Element.Value.NumberString(x)) ⇒ w.writeNumberString(x)
      case (w, Element.Value.Bool(x))         ⇒ w.writeBool(x)
      case (w, Element.Value.Double(x))       ⇒ w.writeDouble(x)

      case (w, Element.Value.Null)      ⇒ w.writeNull()
      case (w, Element.Value.Undefined) ⇒ w.writeUndefined()

      case (w, Element.Value.OverLong(n, x)) ⇒ w.writeOverLong(n, x)
      case (w, Element.Value.Float16(x))     ⇒ w.writeFloat16(x)
      case (w, Element.Value.Float(x))       ⇒ w.writeFloat(x)

      case (w, Element.Value.ByteArray(x))   ⇒ w.writeBytes(x)
      case (w, Element.Value.BytesStream(x)) ⇒ x.foldLeft(w.writeBytesStart())(writeElement).writeBreak()

      case (w, Element.Value.TextStream(x)) ⇒ x.foldLeft(w.writeTextStart())(writeElement).writeBreak()

      case (w, Element.Value.Simple(x)) ⇒ w.writeSimpleValue(x.value)

      case (w, Element.Tagged(tag, x)) ⇒ w.writeTag(tag).write(x)
    }
  }

  implicit def decoder[T <: Element]: Decoder[T] = elementDecoder.asInstanceOf[Decoder[T]]

  val elementDecoder: Decoder[Element] = {
    val bytesDecoder: Decoder[Vector[Element.Value.Bytes]] = Decoder { r ⇒
      r.readBytesStart()
      val b = new immutable.VectorBuilder[Element.Value.Bytes]
      while (!r.tryReadBreak()) b += r.read[Element.Value.Bytes]()
      b.result()
    }
    val textDecoder: Decoder[Vector[Element.Value.Text]] = Decoder { r ⇒
      r.readTextStart()
      val b = new immutable.VectorBuilder[Element.Value.Text]
      while (!r.tryReadBreak()) b += r.read[Element.Value.Text]()
      b.result()
    }

    Decoder { r ⇒
      import io.bullet.borer.{DataItem ⇒ DI}
      r.dataItem match {
        case DI.Null      ⇒ r.readNull(); Element.Value.Null
        case DI.Undefined ⇒ r.readUndefined(); Element.Value.Undefined
        case DI.Bool      ⇒ if (r.readBoolean()) Element.Value.Bool.True else Element.Value.Bool.False

        case DI.Int          ⇒ Element.Value.Int(r.readInt())
        case DI.Long         ⇒ Element.Value.Long(r.readLong())
        case DI.OverLong     ⇒ Element.Value.OverLong(r.overLongNegative, r.readOverLong())
        case DI.Float16      ⇒ Element.Value.Float16(r.readFloat16())
        case DI.Float        ⇒ Element.Value.Float(r.readFloat())
        case DI.Double       ⇒ Element.Value.Double(r.readDouble())
        case DI.NumberString ⇒ Element.Value.NumberString(r.readNumberString())

        case DI.Bytes      ⇒ Element.Value.ByteArray(r.readByteArray())
        case DI.BytesStart ⇒ Element.Value.BytesStream(r.read()(bytesDecoder))

        case DI.Chars | DI.String | DI.Text ⇒ Element.Value.String(r.readString())
        case DI.TextStart                   ⇒ Element.Value.TextStream(r.read()(textDecoder))

        case DI.SimpleValue ⇒ Element.Value.Simple(SimpleValue(r.readSimpleValue()))

        case DI.ArrayHeader ⇒ Element.Array.Sized(r.read[Vector[Element]]())
        case DI.ArrayStart  ⇒ Element.Array.Unsized(r.read[Vector[Element]]())

        case DI.MapHeader ⇒ Element.Map.Sized(r.read[immutable.ListMap[Element, Element]]())
        case DI.MapStart  ⇒ Element.Map.Unsized(r.read[immutable.ListMap[Element, Element]]())

        case DI.Tag ⇒ Element.Tagged(r.readTag(), r.read[Element]())
      }
    }
  }
}
