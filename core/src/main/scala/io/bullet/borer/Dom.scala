/*
 * Copyright (c) 2019-2021 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import io.bullet.borer.encodings.BaseEncoding

import java.util
import scala.annotation.{implicitNotFound, switch, tailrec}
import scala.collection.{immutable, mutable, MapFactory}
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

  sealed abstract class AbstractBytesElem(dataItem: Int) extends Element(dataItem) {
    def byteCount: Long
    def bytesIterator: Iterator[Array[Byte]]
    def compact: Array[Byte]
  }

  final case class ByteArrayElem(value: Array[Byte]) extends AbstractBytesElem(DIS.Bytes) {
    def byteCount                            = value.length.toLong
    def bytesIterator: Iterator[Array[Byte]] = Iterator.single(value)
    def compact                              = value

    override def hashCode() = util.Arrays.hashCode(value)

    override def equals(obj: Any) =
      obj match {
        case ByteArrayElem(x) => util.Arrays.equals(value, x)
        case _                => false
      }
  }

  final case class BytesStreamElem(value: Vector[AbstractBytesElem]) extends AbstractBytesElem(DIS.BytesStart) {
    def byteCount = value.foldLeft(0L)((acc, x) => acc + x.byteCount)

    def bytesIterator: Iterator[Array[Byte]] =
      value.foldLeft[Iterator[Array[Byte]]](Iterator.empty)((acc, x) => acc ++ x.bytesIterator)

    def compact: Array[Byte] = {
      val longSize = byteCount
      val len      = longSize.toInt
      if (len.toLong != longSize) sys.error("byte stream with total size > Int.MaxValue cannot be compacted")
      val result = new Array[Byte](len)
      val iter   = bytesIterator
      @tailrec def rec(ix: Int): Array[Byte] =
        if (ix < len) {
          val chunk = iter.next()
          System.arraycopy(chunk, 0, result, ix, chunk.length)
          rec(ix + chunk.length)
        } else result
      rec(0)
    }
  }

  sealed abstract class AbstractTextElem(dataItem: Int) extends Element(dataItem) {
    def charCount: Long
    def stringIterator: Iterator[String]
    def compact: String
  }

  final case class StringElem(value: String) extends AbstractTextElem(DIS.String) {
    def charCount                        = value.length.toLong
    def stringIterator: Iterator[String] = Iterator.single(value)
    def compact                          = value
  }

  final case class TextStreamElem(value: Vector[AbstractTextElem]) extends AbstractTextElem(DIS.TextStart) {
    def charCount = value.foldLeft(0L)((acc, x) => acc + x.charCount)

    def stringIterator: Iterator[String] =
      value.foldLeft[Iterator[String]](Iterator.empty)((acc, x) => acc ++ x.stringIterator)

    def compact: String = {
      val longSize = charCount
      val len      = longSize.toInt
      if (len.toLong != longSize) sys.error("text stream with total size > Int.MaxValue cannot be compacted")
      val iter = stringIterator
      val sb   = new java.lang.StringBuilder(len)
      while (iter.hasNext) sb.append(iter.next())
      sb.toString
    }
  }

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

  sealed abstract class MapElem(private[Dom] val elements: Array[Element], dataItem: Int) extends Element(dataItem) {
    if ((elements.length & 1) != 0) throw new IllegalArgumentException

    @inline final def size: Int                                        = elements.length >> 1
    @inline final def elementsInterleaved: IndexedSeq[Element]         = ArraySeq.unsafeWrapArray(elements)
    @inline final def isEmpty                                          = false
    @inline final def get: (Int, Iterator[Element], Iterator[Element]) = (size, keys, values)
    @inline final def keys: Iterator[Element]                          = new MapElem.KVIterator(elements, 0)
    @inline final def values: Iterator[Element]                        = new MapElem.KVIterator(elements, 1)

    final def members: Iterator[(Element, Element)] =
      keys.zip(values)

    final def stringKeyedMembers: Iterator[(String, Element)] =
      members.collect { case (StringElem(s), v) => s -> v }

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

    /**
      * Attempts to transform this map element into a `HashMap[String, Element]` and either returns the
      * result, if all keys are indeed `StringElem`s, or the first offending key element that is not a `StringElem`.
      */
    final def toStringKeyedMap: Either[Element, HashMap[String, Element]] = {
      val k = keys
      val v = values
      @tailrec def rec(m: HashMap[String, Element]): Either[Element, HashMap[String, Element]] =
        if (k.hasNext) {
          k.next() match {
            case StringElem(x) => rec(m.updated(x, v.next()))
            case x             => Left(x)
          }
        } else Right(m)
      rec(HashMap.empty)
    }

    final def to[M[A, B] <: Map[A, B]](implicit mf: MapFactory[M]): M[Element, Element] = {
      val b = mf.newBuilder[Element, Element]
      b.addAll(keys zip values)
      b.result()
    }

    final def toStringKeyed[M[A, B] <: Map[A, B]](implicit mf: MapFactory[M]): Either[Element, M[String, Element]] = {
      val k = keys
      val v = values
      @tailrec def rec(b: mutable.Builder[(String, Element), M[String, Element]]): Either[Element, M[String, Element]] =
        if (k.hasNext) {
          k.next() match {
            case StringElem(x) => rec(b.addOne(x -> v.next()))
            case x             => Left(x)
          }
        } else Right(b.result())
      rec(mf.newBuilder[String, Element])
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
          this.dataItemShift == that.dataItemShift && util.Arrays
            .equals(this.elements.asInstanceOf[Array[Object]], that.elements.asInstanceOf[Array[Object]])
        case _ => false
      }
  }

  object MapElem {

    final class Sized private[Dom] (elements: Array[Element]) extends MapElem(elements, DIS.MapHeader)

    object Sized {
      val empty = new Sized(Array.empty)

      def apply[T <: AnyRef: StringOrElem](entries: (T, Element)*): Sized =
        new Sized(construct(entries.iterator, entries.size))

      def apply[T <: AnyRef: StringOrElem](entries: collection.Map[T, Element]): Sized = new Sized(
        construct(entries.iterator, entries.size))

      def apply[T <: AnyRef: StringOrElem](entries: Iterator[(T, Element)]): Sized =
        new Sized(construct(entries))

      def unapply(value: Sized): Sized = value
    }

    final class Unsized private[Dom] (elements: Array[Element]) extends MapElem(elements, DIS.MapStart)

    object Unsized {
      val empty = new Unsized(Array.empty)

      def apply[T <: AnyRef: StringOrElem](entries: (T, Element)*): Unsized =
        new Unsized(construct(entries.iterator, entries.size))

      def apply[T <: AnyRef: StringOrElem](entries: collection.Map[T, Element]): Unsized =
        new Unsized(construct(entries.iterator, entries.size))

      def apply[T <: AnyRef: StringOrElem](entries: Iterator[(T, Element)]): Unsized =
        new Unsized(construct(entries))

      def unapply(value: Unsized): Unsized = value
    }

    @implicitNotFound("Key type must be either `String` or a subtype of `Dom.Element`, not `${T}`")
    sealed trait StringOrElem[T] // phantom type proving that `T` is either `String` or `Dom.Element`
    implicit def stringStringOrElem: StringOrElem[String]           = null
    implicit def elementStringOrElem[T <: Element]: StringOrElem[T] = null

    private def construct(entries: Iterator[(AnyRef, Element)], sizeHint: Int = -1): Array[Element] = {
      val elements = new mutable.ArrayBuilder.ofRef[Element]
      if (sizeHint >= 0) elements.sizeHint(sizeHint << 1)
      entries.foreach {
        case (key: String, value)  => elements += StringElem(key) += value
        case (key: Element, value) => elements += key += value
        case _                     => throw new IllegalStateException
      }
      elements.result()
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
      (Integer.numberOfTrailingZeros(r.dataItem()): @switch) match {
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
                elements += r.read[Element]() += r.read[Element]()
                rec(remaining - 1)
              } else new MapElem.Sized(elements.result())
            rec(count)
          } else MapElem.Sized.empty

        case DIS.MapStart =>
          r.skipDataItem()
          if (!r.tryReadBreak()) {
            @tailrec def rec(elements: mutable.ArrayBuilder.ofRef[Dom.Element]): MapElem.Unsized =
              if (r.tryReadBreak()) new MapElem.Unsized(elements.result())
              else rec(elements += r.read[Element]() += r.read[Element]())
            rec(new mutable.ArrayBuilder.ofRef[Dom.Element])
          } else MapElem.Unsized.empty

        case DIS.Tag => TaggedElem(r.readTag(), r.read[Element]())
      }
    }
  }

  /**
    * A [[Dom.Transformer]] encapsulates the ability for arbitrary DOM transformations.
    * The default implementation applies a NOP transformation, i.e. returns an identical DOM structure.
    *
    * Override some or all methods to customize the transformation logic.
    */
  trait Transformer extends (Element => Element) {

    def apply(elem: Element): Element =
      (elem.dataItemShift: @switch) match {
        case DIS.Null         => transformNull()
        case DIS.Undefined    => transformUndefined()
        case DIS.Boolean      => transformBoolean(elem.asInstanceOf[BooleanElem])
        case DIS.Int          => transformInt(elem.asInstanceOf[IntElem])
        case DIS.Long         => transformLong(elem.asInstanceOf[LongElem])
        case DIS.OverLong     => transformOverLong(elem.asInstanceOf[OverLongElem])
        case DIS.Float16      => transformFloat16(elem.asInstanceOf[Float16Elem])
        case DIS.Float        => transformFloat(elem.asInstanceOf[FloatElem])
        case DIS.Double       => transformDouble(elem.asInstanceOf[DoubleElem])
        case DIS.NumberString => transformNumberString(elem.asInstanceOf[NumberStringElem])
        case DIS.String       => transformString(elem.asInstanceOf[StringElem])
        case DIS.TextStart    => transformTextStart(elem.asInstanceOf[TextStreamElem])
        case DIS.Bytes        => transformBytes(elem.asInstanceOf[ByteArrayElem])
        case DIS.BytesStart   => transformBytesStart(elem.asInstanceOf[BytesStreamElem])
        case DIS.SimpleValue  => transformSimpleValue(elem.asInstanceOf[SimpleValueElem])
        case DIS.ArrayHeader  => transformSizedArray(elem.asInstanceOf[ArrayElem.Sized])
        case DIS.ArrayStart   => transformUnsizedArray(elem.asInstanceOf[ArrayElem.Unsized])
        case DIS.MapHeader    => transformSizedMap(elem.asInstanceOf[MapElem.Sized])
        case DIS.MapStart     => transformUnsizedMap(elem.asInstanceOf[MapElem.Unsized])
        case DIS.Tag          => transformTag(elem.asInstanceOf[TaggedElem])
      }

    def transformNull(): Element                                   = NullElem
    def transformUndefined(): Element                              = UndefinedElem
    def transformBoolean(elem: BooleanElem): Element               = elem
    def transformInt(elem: IntElem): Element                       = elem
    def transformLong(elem: LongElem): Element                     = elem
    def transformOverLong(elem: OverLongElem): Element             = elem
    def transformFloat16(elem: Float16Elem): Element               = elem
    def transformFloat(elem: FloatElem): Element                   = elem
    def transformDouble(elem: DoubleElem): Element                 = elem
    def transformNumberString(elem: NumberStringElem): Element     = elem
    def transformString(elem: StringElem): Element                 = transformTextElem(elem)
    def transformTextStart(elem: TextStreamElem): Element          = transformTextElem(elem)
    def transformTextElem(elem: AbstractTextElem): Element         = elem
    def transformBytes(elem: ByteArrayElem): Element               = transformBytesElem(elem)
    def transformBytesStart(elem: BytesStreamElem): Element        = transformBytesElem(elem)
    def transformBytesElem(elem: AbstractBytesElem): Element       = elem
    def transformSimpleValue(elem: SimpleValueElem): Element       = elem
    def transformTag(elem: TaggedElem): Element                    = elem
    def transformSizedArray(elem: ArrayElem.Sized): Element        = ArrayElem.Sized(transformArray(elem.elements))
    def transformUnsizedArray(elem: ArrayElem.Unsized): Element    = ArrayElem.Unsized(transformArray(elem.elements))
    def transformArray(elements: Vector[Element]): Vector[Element] = elements.map(this)
    def transformSizedMap(elem: MapElem.Sized): Element            = MapElem.Sized(transformMapMembers(elem.members))
    def transformUnsizedMap(elem: MapElem.Unsized): Element        = MapElem.Unsized(transformMapMembers(elem.members))

    def transformMapMembers(members: Iterator[(Element, Element)]): Iterator[(Element, Element)] =
      members.map(transformMapMember)
    def transformMapMember(member: (Element, Element)): (Element, Element) = this(member._1) -> this(member._2)
  }

  object Transformer {

    /**
      * A [[Transformer]] that converts certain DOM elements of a given DOM structure that are not supported by the
      * JSON renderer into alternative elements that can be represented in JSON.
      *
      * The elements that are not supported (and as such will still trigger errors during an attempted encoding to
      * JSON) are:
      *
      * - Non-String Map Keys
      * - Float.NaN, Float.PositiveInfinity, Float.NegativeInfinity
      * - Double.NaN, Double.PositiveInfinity, Double.NegativeInfinity
      * - Tags
      *
      * This transformer can be used, for example, for converting a CBOR document to JSON by first parsing the CBOR
      * encoding into a [[Dom.Element]], then applying this transformer and then encoding the transformation result to
      * JSON.
      *
      * NOTE: Since there is no single, standard way of representing the CBOR-only DOM elements in JSON this transformer
      * provides merely one of many possible such implementations.
      * Override the respective methods in order to customize the logic!
      */
    trait ToJsonSubset extends Transformer {
      val bytesEncoding: BaseEncoding = BaseEncoding.base64

      override def transformUndefined(): Element                = NullElem
      override def transformFloat16(elem: Float16Elem): Element = FloatElem(elem.value)

      override def transformBytesElem(elem: AbstractBytesElem): Element =
        StringElem(new String(bytesEncoding.encode(elem.compact)))

      override def transformTextStart(elem: TextStreamElem) =
        StringElem(elem.compact)

      override def transformSimpleValue(elem: SimpleValueElem): Element =
        IntElem(elem.value.value)

      override def transformSizedArray(elem: ArrayElem.Sized): Element =
        ArrayElem.Unsized(elem.elements.map(this))

      override def transformSizedMap(elem: MapElem.Sized): Element =
        new MapElem.Unsized(elem.elements.map(this))
    }
  }
}
