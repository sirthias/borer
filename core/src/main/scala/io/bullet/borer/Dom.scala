/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import java.util

import scala.annotation.{switch, tailrec}
import scala.collection.{immutable, mutable}
import scala.collection.compat.immutable.ArraySeq
import scala.collection.immutable.HashMap
import scala.util.hashing.MurmurHash3

/**
  * Simple Document Object Model (DOM) for CBOR.
  *
  * Practically all valid CBOR encodings can be decoded into this structure and vice versa.
  * Provided as an alternative to plain [[Writer]]-based encoding and [[Reader]]-based decoding.
  */
object Dom {
  import DataItem.{Shifts => DIS}

  sealed abstract class Element(val dataItemShift: Int)

  final case object NullElem                   extends Element(DIS.Null)
  final case object UndefinedElem              extends Element(DIS.Undefined)
  final case class BooleanElem(value: Boolean) extends Element(DIS.Boolean)

  object BooleanElem {
    val True  = BooleanElem(true)
    val False = BooleanElem(false)
  }

  final case class IntElem(value: Int)                          extends Element(DIS.Int)
  final case class LongElem(value: Long)                        extends Element(DIS.Long)
  final case class OverLongElem(negative: Boolean, value: Long) extends Element(DIS.OverLong)
  final case class Float16Elem(value: Float)                    extends Element(DIS.Float16)
  final case class FloatElem(value: Float)                      extends Element(DIS.Float)
  final case class DoubleElem(value: Double)                    extends Element(DIS.Double)
  final case class NumberStringElem(value: String)              extends Element(DIS.NumberString)

  sealed abstract class AbstractBytesElem(dataItem: Int) extends Element(dataItem)

  final case class ByteArrayElem(value: Array[Byte]) extends AbstractBytesElem(DIS.Bytes) {
    override def hashCode() = util.Arrays.hashCode(value)
    override def equals(obj: Any) = obj match {
      case ByteArrayElem(x) => util.Arrays.equals(value, x)
      case _                => false
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
      val empty                     = new Sized(Vector.empty)
      def apply(elements: Element*) = new Sized(elements.toVector)
    }

    final case class Unsized(elements: Vector[Element]) extends ArrayElem(DIS.ArrayStart) {
      override def toString = elements.mkString("*[", ", ", "]")
    }

    object Unsized {
      val empty                     = new Unsized(Vector.empty)
      def apply(elements: Element*) = new Unsized(elements.toVector)
    }
  }

  sealed abstract class MapElem(val size: Int, private[Dom] val elements: Array[Element], dataItem: Int)
      extends Element(dataItem) {
    if (size != (elements.length >> 1)) throw new IllegalArgumentException

    final def elementsInterleaved: IndexedSeq[Element] = ArraySeq.unsafeWrapArray(elements)

    final def isEmpty                                          = false
    final def get: (Int, Iterator[Element], Iterator[Element]) = (size, keys, values)

    final def keys: Iterator[Element]   = new MapElem.KVIterator(elements, 0)
    final def values: Iterator[Element] = new MapElem.KVIterator(elements, 1)

    def apply(key: String): Option[Element] = {
      @tailrec def rec(ix: Int): Option[Element] =
        if (ix < elements.length) {
          elements(ix) match {
            case StringElem(`key`) => Some(elements(ix + 1))
            case _                 => rec(ix + 2)
          }
        } else None
      rec(0)
    }

    def apply(key: Element): Option[Element] = {
      @tailrec def rec(ix: Int): Option[Element] =
        if (ix < elements.length) if (elements(ix) == key) Some(elements(ix + 1)) else rec(ix + 2) else None
      rec(0)
    }

    final def toMap: HashMap[Element, Element] = {
      val k = keys
      val v = values
      @tailrec def rec(m: HashMap[Element, Element]): HashMap[Element, Element] =
        if (k.hasNext) rec(m.updated(k.next(), v.next())) else m
      rec(HashMap.empty)
    }

    final override def toString =
      keys
        .zip(values)
        .map(x => x._1.toString + ": " + x._2)
        .mkString(if (dataItem == DIS.MapStart) "*{" else "{", ", ", "}")

    final override def hashCode() = {
      import scala.runtime.Statics.{finalizeHash, mix}
      finalizeHash(mix(mix(mix(-889275714, size), MurmurHash3.arrayHash(elements)), dataItem), 3)
    }

    final override def equals(obj: Any) =
      obj match {
        case that: MapElem =>
          this.size == that.size && this.dataItemShift == that.dataItemShift && util.Arrays
            .equals(this.elements.asInstanceOf[Array[Object]], that.elements.asInstanceOf[Array[Object]])
        case _ => false
      }
  }

  object MapElem {

    final class Sized private[Dom] (size: Int, elements: Array[Element]) extends MapElem(size, elements, DIS.MapHeader)

    object Sized {
      private[this] val create                                             = new Sized(_, _)
      val empty                                                            = new Sized(0, Array.empty)
      def apply(first: (String, Element), more: (String, Element)*): Sized = construct(first +: more, create)
      def apply(entries: (Element, Element)*): Sized                       = construct(entries, create)
      def apply(entries: collection.Map[Element, Element]): Sized          = construct(entries, create)
      def unapply(value: Sized): Sized                                     = value
    }

    final class Unsized private[Dom] (size: Int, elements: Array[Element]) extends MapElem(size, elements, DIS.MapStart)

    object Unsized {
      private[this] val create                                               = new Unsized(_, _)
      val empty                                                              = new Unsized(0, Array.empty)
      def apply(first: (String, Element), more: (String, Element)*): Unsized = construct(first +: more, create)
      def apply(entries: (Element, Element)*): Unsized                       = construct(entries, create)
      def apply(entries: collection.Map[Element, Element]): Unsized          = construct(entries, create)
      def unapply(value: Unsized): Unsized                                   = value
    }

    private def construct[T](entries: Iterable[(AnyRef, Element)], f: (Int, Array[Element]) => T): T = {
      val elements = new mutable.ArrayBuilder.ofRef[Dom.Element]
      elements.sizeHint(entries.size << 1)
      entries.foreach {
        case (key, value) =>
          var keyElem = key match {
            case x: String  => StringElem(x)
            case x: Element => x
          }
          elements += keyElem += value
      }
      f(entries.size, elements.result())
    }

    final private class KVIterator(elements: Array[Element], startIndex: Int) extends Iterator[Element] {
      private[this] var ix = startIndex
      def hasNext          = ix < elements.length

      def next() =
        if (hasNext) {
          val elem = elements(ix)
          ix += 2
          elem
        } else Iterator.empty.next()
    }
  }

  final case class TaggedElem(tag: Tag, value: Element) extends Element(DIS.Tag)

  implicit def encoder[T <: Element]: Encoder[T] = elementEncoder.asInstanceOf[Encoder[T]]

  val elementEncoder: Encoder[Element] = {
    val writeElement = (w: Writer, x: Element) => w.write(x)

    Encoder { (w, x) =>
      (x.dataItemShift: @switch) match {
        case DIS.Null      => w.writeNull()
        case DIS.Undefined => w.writeUndefined()
        case DIS.Boolean   => w.writeBoolean(x.asInstanceOf[BooleanElem].value)

        case DIS.Int          => w.writeInt(x.asInstanceOf[IntElem].value)
        case DIS.Long         => w.writeLong(x.asInstanceOf[LongElem].value)
        case DIS.OverLong     => val n = x.asInstanceOf[OverLongElem]; w.writeOverLong(n.negative, n.value)
        case DIS.Float16      => w.writeFloat16(x.asInstanceOf[Float16Elem].value)
        case DIS.Float        => w.writeFloat(x.asInstanceOf[FloatElem].value)
        case DIS.Double       => w.writeDouble(x.asInstanceOf[DoubleElem].value)
        case DIS.NumberString => w.writeNumberString(x.asInstanceOf[NumberStringElem].value)

        case DIS.String => w.writeString(x.asInstanceOf[StringElem].value)
        case DIS.TextStart =>
          x.asInstanceOf[TextStreamElem].value.foldLeft(w.writeTextStart())(writeElement).writeBreak()

        case DIS.Bytes => w.writeBytes(x.asInstanceOf[ByteArrayElem].value)
        case DIS.BytesStart =>
          x.asInstanceOf[BytesStreamElem].value.foldLeft(w.writeBytesStart())(writeElement).writeBreak()

        case DIS.SimpleValue => w.write(x.asInstanceOf[SimpleValueElem].value)

        case DIS.ArrayHeader =>
          val a = x.asInstanceOf[ArrayElem.Sized]
          a.elements.foldLeft(w.writeArrayHeader(a.elements.size))(writeElement)
        case DIS.ArrayStart =>
          x.asInstanceOf[ArrayElem.Unsized].elements.foldLeft(w.writeArrayStart())(writeElement).writeBreak()

        case DIS.MapHeader =>
          val m     = x.asInstanceOf[MapElem.Sized]
          val array = m.elements
          @tailrec def rec(w: Writer, ix: Int): w.type =
            if (ix < array.length) rec(w.write(array(ix)).write(array(ix + 1)), ix + 2) else w
          rec(w.writeMapHeader(m.size), 0)

        case DIS.MapStart =>
          val m     = x.asInstanceOf[MapElem.Unsized]
          val array = m.elements
          @tailrec def rec(w: Writer, ix: Int): w.type =
            if (ix < array.length) rec(w.write(array(ix)).write(array(ix + 1)), ix + 2) else w
          rec(w.writeMapStart(), 0).writeBreak()

        case DIS.Tag => val n = x.asInstanceOf[TaggedElem]; w.writeTag(n.tag).write(n.value)
      }
    }
  }

  implicit def decoder[T <: Element]: Decoder[T] = elementDecoder.asInstanceOf[Decoder[T]]

  val elementDecoder: Decoder[Element] = {
    val bytesDecoder: Decoder[Vector[AbstractBytesElem]] = Decoder { r =>
      r.readBytesStart()
      if (!r.tryReadBreak()) {
        val b = new immutable.VectorBuilder[AbstractBytesElem]
        while (!r.tryReadBreak()) b += r.read[AbstractBytesElem]()
        b.result()
      } else Vector.empty
    }
    val textDecoder: Decoder[Vector[AbstractTextElem]] = Decoder { r =>
      r.readTextStart()
      if (!r.tryReadBreak()) {
        val b = new immutable.VectorBuilder[AbstractTextElem]
        while (!r.tryReadBreak()) b += r.read[AbstractTextElem]()
        b.result()
      } else Vector.empty
    }

    Decoder { r =>
      (Integer.numberOfTrailingZeros(r.dataItem): @switch) match {
        case DIS.Null      => r.readNull(); NullElem
        case DIS.Undefined => r.readUndefined(); UndefinedElem
        case DIS.Boolean   => if (r.readBoolean()) BooleanElem.True else BooleanElem.False

        case DIS.Int          => IntElem(r.readInt())
        case DIS.Long         => LongElem(r.readLong())
        case DIS.OverLong     => OverLongElem(r.overLongNegative, r.readOverLong())
        case DIS.Float16      => Float16Elem(r.readFloat16())
        case DIS.Float        => FloatElem(r.readFloat())
        case DIS.Double       => DoubleElem(r.readDouble())
        case DIS.NumberString => NumberStringElem(r.readNumberString())

        case DIS.Bytes      => ByteArrayElem(r.readByteArray())
        case DIS.BytesStart => BytesStreamElem(r.read()(bytesDecoder))

        case DIS.Chars | DIS.String | DIS.Text => StringElem(r.readString())
        case DIS.TextStart                     => TextStreamElem(r.read()(textDecoder))

        case DIS.SimpleValue => SimpleValueElem(SimpleValue(r.readSimpleValue()))

        case DIS.ArrayHeader => ArrayElem.Sized(r.read[Vector[Element]]())
        case DIS.ArrayStart  => ArrayElem.Unsized(r.read[Vector[Element]]())

        case DIS.MapHeader =>
          if (!r.tryReadMapHeader(0)) {
            val elements = new mutable.ArrayBuilder.ofRef[Dom.Element]
            val size     = r.readMapHeader()
            val count    = size.toInt
            if (size > Int.MaxValue) r.overflow("Dom.MapElem does not support more than 2^30 elements")
            elements.sizeHint(count)
            @tailrec def rec(remaining: Int): MapElem.Sized =
              if (remaining > 0) {
                elements += r.read[Element] += r.read[Element]
                rec(remaining - 1)
              } else new MapElem.Sized(count, elements.result())
            rec(count)
          } else MapElem.Sized.empty

        case DIS.MapStart =>
          r.skipDataItem()
          if (!r.tryReadBreak) {
            @tailrec def rec(elements: mutable.ArrayBuilder.ofRef[Dom.Element]): MapElem.Unsized =
              if (r.tryReadBreak()) {
                val array = elements.result()
                new MapElem.Unsized(array.length >> 1, array)
              } else rec(elements += r.read[Element]() += r.read[Element]())
            rec(new mutable.ArrayBuilder.ofRef[Dom.Element])
          } else MapElem.Unsized.empty

        case DIS.Tag => TaggedElem(r.readTag(), r.read[Element]())
      }
    }
  }
}
