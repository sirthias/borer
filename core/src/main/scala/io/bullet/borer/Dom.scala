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
  import DataItem.{Shifts ⇒ DIS}

  sealed abstract class Element(val dataItemShift: Int)

  final case object NullElem                extends Element(DIS.Null)
  final case object UndefinedElem           extends Element(DIS.Undefined)
  final case class BoolElem(value: Boolean) extends Element(DIS.Bool)

  object BoolElem {
    val True  = BoolElem(true)
    val False = BoolElem(false)
  }

  final case class IntElem(value: Int)                          extends Element(DIS.Int)
  final case class LongElem(value: Long)                        extends Element(DIS.Long)
  final case class OverLongElem(negative: Boolean, value: Long) extends Element(DIS.OverLong)
  final case class Float16Elem(value: Float)                    extends Element(DIS.Float16)
  final case class FloatElem(value: Float)                      extends Element(DIS.Float)
  final case class DoubleElem(value: Double)                    extends Element(DIS.Double)
  final case class DecimalElem(integer: Long, fraction: Int)    extends Element(DIS.Decimal)
  final case class NumberStringElem(value: String)              extends Element(DIS.NumberString)

  sealed abstract class AbstractBytesElem(dataItem: Int) extends Element(dataItem)

  final case class ByteArrayElem(value: Array[Byte]) extends AbstractBytesElem(DIS.Bytes) {
    override def hashCode() = util.Arrays.hashCode(value)
    override def equals(obj: Any) = obj match {
      case ByteArrayElem(x) ⇒ util.Arrays.equals(value, x)
      case _                ⇒ false
    }
  }

  final case class BytesStreamElem(value: Vector[AbstractBytesElem]) extends AbstractBytesElem(DIS.BytesStart)

  sealed abstract class AbstractTextElem(dataItem: Int) extends Element(dataItem)

  final case class StringElem(value: String)                       extends AbstractTextElem(DIS.String)
  final case class TextStreamElem(value: Vector[AbstractTextElem]) extends AbstractTextElem(DIS.TextStart)

  final case class SimpleValueElem(value: SimpleValue) extends Element(DIS.SimpleValue)

  sealed abstract class ArrayElem(dataItem: Int) extends Element(dataItem) {
    def elements: Vector[Element]
  }

  object ArrayElem {
    final case class Sized(elements: Vector[Element]) extends ArrayElem(DIS.ArrayHeader) {
      override def toString = elements.mkString("[", ", ", "]")
    }
    object Sized {
      def apply(elements: Element*) = new Sized(elements.toVector)
    }

    final case class Unsized(elements: Vector[Element]) extends ArrayElem(DIS.ArrayStart) {
      override def toString = elements.mkString("*[", ", ", "]")
    }
    object Unsized {
      def apply(elements: Element*) = new Unsized(elements.toVector)
    }
  }

  sealed abstract class MapElem(dataItem: Int) extends Element(dataItem) {
    def entries: immutable.Map[Element, Element]
  }

  object MapElem {
    final case class Sized(entries: immutable.Map[Element, Element]) extends MapElem(DIS.MapHeader) {
      override def toString = entries.map(x ⇒ x._1 + ": " + x._2).mkString("{", ", ", "}")
    }
    object Sized {
      def apply(entries: (String, Element)*) = new Sized(asListMap(entries))
    }

    final case class Unsized(entries: immutable.Map[Element, Element]) extends MapElem(DIS.MapStart) {
      override def toString = entries.map(x ⇒ x._1 + ": " + x._2).mkString("*{", ", ", "}")
    }
    object Unsized {
      def apply(entries: (String, Element)*) = new Unsized(asListMap(entries))
    }

    private def asListMap(entries: Seq[(String, Element)]): immutable.ListMap[Element, Element] =
      entries.foldLeft(immutable.ListMap.empty[Element, Element])((m, x) ⇒ m.updated(StringElem(x._1), x._2))
  }

  final case class TaggedElem(tag: Tag, value: Element) extends Element(DIS.Tag)

  implicit def encoder[T <: Element]: Encoder[T] = elementEncoder.asInstanceOf[Encoder[T]]

  val elementEncoder: Encoder[Element] = {
    val writeElement = (w: Writer, x: Element) ⇒ w.write(x)
    val writeEntry   = (w: Writer, x: (Element, Element)) ⇒ w.write(x._1).write(x._2)

    Encoder { (w, x) ⇒
      x.dataItemShift match {
        case DIS.Null      ⇒ w.writeNull()
        case DIS.Undefined ⇒ w.writeUndefined()
        case DIS.Bool      ⇒ w.writeBool(x.asInstanceOf[BoolElem].value)

        case DIS.Int          ⇒ w.writeInt(x.asInstanceOf[IntElem].value)
        case DIS.Long         ⇒ w.writeLong(x.asInstanceOf[LongElem].value)
        case DIS.OverLong     ⇒ val n = x.asInstanceOf[OverLongElem]; w.writeOverLong(n.negative, n.value)
        case DIS.Float16      ⇒ w.writeFloat16(x.asInstanceOf[Float16Elem].value)
        case DIS.Float        ⇒ w.writeFloat(x.asInstanceOf[FloatElem].value)
        case DIS.Double       ⇒ w.writeDouble(x.asInstanceOf[DoubleElem].value)
        case DIS.Decimal      ⇒ val n = x.asInstanceOf[DecimalElem]; w.writeDecimal(n.integer, n.fraction)
        case DIS.NumberString ⇒ w.writeNumberString(x.asInstanceOf[NumberStringElem].value)

        case DIS.String ⇒ w.writeString(x.asInstanceOf[StringElem].value)
        case DIS.TextStart ⇒
          x.asInstanceOf[TextStreamElem].value.foldLeft(w.writeTextStart())(writeElement).writeBreak()

        case DIS.Bytes ⇒ w.writeBytes(x.asInstanceOf[ByteArrayElem].value)
        case DIS.BytesStart ⇒
          x.asInstanceOf[BytesStreamElem].value.foldLeft(w.writeBytesStart())(writeElement).writeBreak()

        case DIS.SimpleValue ⇒ w.write(x.asInstanceOf[SimpleValueElem].value)

        case DIS.ArrayHeader ⇒
          val a = x.asInstanceOf[ArrayElem.Sized]
          a.elements.foldLeft(w.writeArrayHeader(a.elements.size))(writeElement)
        case DIS.ArrayStart ⇒
          x.asInstanceOf[ArrayElem.Unsized].elements.foldLeft(w.writeArrayStart())(writeElement).writeBreak()

        case DIS.MapHeader ⇒
          val m = x.asInstanceOf[MapElem.Sized]
          m.entries.foldLeft(w.writeMapHeader(m.entries.size))(writeEntry)
        case DIS.MapStart ⇒
          x.asInstanceOf[MapElem.Unsized].entries.foldLeft(w.writeMapStart())(writeEntry).writeBreak()

        case DIS.Tag ⇒ val n = x.asInstanceOf[TaggedElem]; w.writeTag(n.tag).write(n.value)
      }
    }
  }

  implicit def decoder[T <: Element]: Decoder[T] = elementDecoder.asInstanceOf[Decoder[T]]

  val elementDecoder: Decoder[Element] = {
    val bytesDecoder: Decoder[Vector[AbstractBytesElem]] = Decoder { r ⇒
      r.readBytesStart()
      val b = new immutable.VectorBuilder[AbstractBytesElem]
      while (!r.tryReadBreak()) b += r.read[AbstractBytesElem]()
      b.result()
    }
    val textDecoder: Decoder[Vector[AbstractTextElem]] = Decoder { r ⇒
      r.readTextStart()
      val b = new immutable.VectorBuilder[AbstractTextElem]
      while (!r.tryReadBreak()) b += r.read[AbstractTextElem]()
      b.result()
    }

    Decoder { r ⇒
      31 - Integer.numberOfLeadingZeros(r.dataItem) match {
        case DIS.Null      ⇒ r.readNull(); NullElem
        case DIS.Undefined ⇒ r.readUndefined(); UndefinedElem
        case DIS.Bool      ⇒ if (r.readBoolean()) BoolElem.True else BoolElem.False

        case DIS.Int          ⇒ IntElem(r.readInt())
        case DIS.Long         ⇒ LongElem(r.readLong())
        case DIS.OverLong     ⇒ OverLongElem(r.overLongNegative, r.readOverLong())
        case DIS.Float16      ⇒ Float16Elem(r.readFloat16())
        case DIS.Float        ⇒ FloatElem(r.readFloat())
        case DIS.Double       ⇒ DoubleElem(r.readDouble())
        case DIS.Decimal      ⇒ DecimalElem(r.decimalInteger, r.readDecimalFraction())
        case DIS.NumberString ⇒ NumberStringElem(r.readNumberString())

        case DIS.Bytes      ⇒ ByteArrayElem(r.readByteArray())
        case DIS.BytesStart ⇒ BytesStreamElem(r.read()(bytesDecoder))

        case DIS.Chars | DIS.String | DIS.Text ⇒ StringElem(r.readString())
        case DIS.TextStart                     ⇒ TextStreamElem(r.read()(textDecoder))

        case DIS.SimpleValue ⇒ SimpleValueElem(SimpleValue(r.readSimpleValue()))

        case DIS.ArrayHeader ⇒ ArrayElem.Sized(r.read[Vector[Element]]())
        case DIS.ArrayStart  ⇒ ArrayElem.Unsized(r.read[Vector[Element]]())

        case DIS.MapHeader ⇒ MapElem.Sized(r.read[immutable.ListMap[Element, Element]]())
        case DIS.MapStart  ⇒ MapElem.Unsized(r.read[immutable.ListMap[Element, Element]]())

        case DIS.Tag ⇒ TaggedElem(r.readTag(), r.read[Element]())
      }
    }
  }
}
