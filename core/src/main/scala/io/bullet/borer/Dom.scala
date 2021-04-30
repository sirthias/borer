/*
 * Copyright (c) 2019-2021 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import io.bullet.borer.encodings.BaseEncoding
import io.bullet.borer.internal.Util

import scala.annotation.{implicitNotFound, switch, tailrec}
import scala.collection.{immutable, mutable}
import scala.collection.compat.immutable.ArraySeq
import scala.collection.compat._
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

  sealed abstract class Element(val dataItemShift: Int) {
    def render: String = Renderer.default.render(this)
  }

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

  final case class ByteArrayElem(bytes: Array[Byte]) extends AbstractBytesElem(DIS.Bytes) {
    def byteCount                            = bytes.length.toLong
    def bytesIterator: Iterator[Array[Byte]] = Iterator.single(bytes)
    def compact                              = bytes

    override def toString = s"$productPrefix($byteCount bytes)"

    override def hashCode() = java.util.Arrays.hashCode(bytes)

    override def equals(obj: Any) =
      obj match {
        case ByteArrayElem(x) => java.util.Arrays.equals(bytes, x)
        case _                => false
      }
  }

  final case class BytesStreamElem(elems: Vector[AbstractBytesElem]) extends AbstractBytesElem(DIS.BytesStart) {
    def byteCount = elems.foldLeft(0L)((acc, x) => acc + x.byteCount)

    def bytesIterator: Iterator[Array[Byte]] =
      elems.foldLeft[Iterator[Array[Byte]]](Iterator.empty)((acc, x) => acc ++ x.bytesIterator)

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

  final case class TextStreamElem(elems: Vector[AbstractTextElem]) extends AbstractTextElem(DIS.TextStart) {
    def charCount = elems.foldLeft(0L)((acc, x) => acc + x.charCount)

    def stringIterator: Iterator[String] =
      elems.foldLeft[Iterator[String]](Iterator.empty)((acc, x) => acc ++ x.stringIterator)

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
    def elems: Vector[Element]
  }

  object ArrayElem {

    final case class Sized(elems: Vector[Element]) extends ArrayElem(DIS.ArrayHeader) {
      override def toString = elems.mkString("[", ", ", "]")
    }

    object Sized {
      val empty                     = new Sized(Vector.empty)
      def apply(elements: Element*) = new Sized(elements.toVector)
    }

    final case class Unsized(elems: Vector[Element]) extends ArrayElem(DIS.ArrayStart) {
      override def toString = elems.mkString("*[", ", ", "]")
    }

    object Unsized {
      val empty                     = new Unsized(Vector.empty)
      def apply(elements: Element*) = new Unsized(elements.toVector)
    }
  }

  sealed abstract class MapElem(private[Dom] val elems: Array[Element], dataItem: Int) extends Element(dataItem) {
    if ((elems.length & 1) != 0) throw new IllegalArgumentException

    @inline final def size: Int                                        = elems.length >> 1
    @inline final def elementsInterleaved: IndexedSeq[Element]         = ArraySeq.unsafeWrapArray(elems)
    @inline final def isEmpty                                          = false
    @inline final def get: (Int, Iterator[Element], Iterator[Element]) = (size, keys, values)
    @inline final def keys: Iterator[Element]                          = new MapElem.KVIterator(elems, 0)
    @inline final def values: Iterator[Element]                        = new MapElem.KVIterator(elems, 1)

    final def members: Iterator[(Element, Element)] =
      keys.zip(values)

    final def stringKeyedMembers: Iterator[(String, Element)] =
      members.collect { case (StringElem(s), v) => s -> v }

    def apply(key: String): Option[Element] = {
      @tailrec def rec(ix: Int): Option[Element] =
        if (ix < elems.length) {
          elems(ix) match {
            case StringElem(`key`) => Some(elems(ix + 1))
            case _                 => rec(ix + 2)
          }
        } else None
      rec(0)
    }

    def apply(key: Element): Option[Element] = {
      @tailrec def rec(ix: Int): Option[Element] =
        if (ix < elems.length) if (elems(ix) == key) Some(elems(ix + 1)) else rec(ix + 2) else None
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

    final def to[M[A, B] <: Map[A, B]](
        implicit fac: Factory[(Element, Element), M[Element, Element]]): M[Element, Element] = {
      val b = fac.newBuilder
      b ++= (keys zip values)
      b.result()
    }

    final def toStringKeyed[M[A, B] <: Map[A, B]](
        implicit fac: Factory[(String, Element), M[String, Element]]): Either[Element, M[String, Element]] = {
      val k = keys
      val v = values
      @tailrec def rec(b: mutable.Builder[(String, Element), M[String, Element]]): Either[Element, M[String, Element]] =
        if (k.hasNext) {
          k.next() match {
            case StringElem(x) => rec(b += (x -> v.next()))
            case x             => Left(x)
          }
        } else Right(b.result())
      rec(fac.newBuilder)
    }

    final override def toString =
      keys
        .zip(values)
        .map(x => x._1.toString + ": " + x._2)
        .mkString(if (dataItem == DIS.MapStart) "*{" else "{", ", ", "}")

    final override def hashCode() = {
      import scala.runtime.Statics.{finalizeHash, mix}
      finalizeHash(mix(mix(mix(-889275714, size), MurmurHash3.arrayHash(elems)), dataItem), 3)
    }

    final override def equals(obj: Any) =
      obj match {
        case that: MapElem =>
          this.dataItemShift == that.dataItemShift && java.util.Arrays
            .equals(this.elems.asInstanceOf[Array[Object]], that.elems.asInstanceOf[Array[Object]])
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
          x.asInstanceOf[TextStreamElem].elems.foldLeft(w.writeTextStart())(writeElement).writeBreak()

        case DIS.Bytes => w.writeBytes(x.asInstanceOf[ByteArrayElem].bytes)
        case DIS.BytesStart =>
          x.asInstanceOf[BytesStreamElem].elems.foldLeft(w.writeBytesStart())(writeElement).writeBreak()

        case DIS.SimpleValue => w.write(x.asInstanceOf[SimpleValueElem].value)

        case DIS.ArrayHeader =>
          val a = x.asInstanceOf[ArrayElem.Sized]
          a.elems.foldLeft(w.writeArrayHeader(a.elems.size))(writeElement)
        case DIS.ArrayStart =>
          x.asInstanceOf[ArrayElem.Unsized].elems.foldLeft(w.writeArrayStart())(writeElement).writeBreak()

        case DIS.MapHeader =>
          val m     = x.asInstanceOf[MapElem.Sized]
          val array = m.elems
          @tailrec def rec(w: Writer, ix: Int): w.type =
            if (ix < array.length) rec(w.write(array(ix)).write(array(ix + 1)), ix + 2) else w
          rec(w.writeMapHeader(m.size), 0)

        case DIS.MapStart =>
          val m     = x.asInstanceOf[MapElem.Unsized]
          val array = m.elems
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
        case DIS.TextStart    => transformTextStream(elem.asInstanceOf[TextStreamElem])
        case DIS.Bytes        => transformBytes(elem.asInstanceOf[ByteArrayElem])
        case DIS.BytesStart   => transformBytesStream(elem.asInstanceOf[BytesStreamElem])
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
    def transformTextStream(elem: TextStreamElem): Element         = transformTextElem(elem)
    def transformTextElem(elem: AbstractTextElem): Element         = elem
    def transformBytes(elem: ByteArrayElem): Element               = transformBytesElem(elem)
    def transformBytesStream(elem: BytesStreamElem): Element       = transformBytesElem(elem)
    def transformBytesElem(elem: AbstractBytesElem): Element       = elem
    def transformSimpleValue(elem: SimpleValueElem): Element       = elem
    def transformTag(elem: TaggedElem): Element                    = elem
    def transformSizedArray(elem: ArrayElem.Sized): Element        = ArrayElem.Sized(transformArray(elem.elems))
    def transformUnsizedArray(elem: ArrayElem.Unsized): Element    = ArrayElem.Unsized(transformArray(elem.elems))
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

      override def transformTextStream(elem: TextStreamElem) =
        StringElem(elem.compact)

      override def transformSimpleValue(elem: SimpleValueElem): Element =
        IntElem(elem.value.value)

      override def transformSizedArray(elem: ArrayElem.Sized): Element =
        ArrayElem.Unsized(elem.elems.map(this))

      override def transformSizedMap(elem: MapElem.Sized): Element =
        new MapElem.Unsized(elem.elems.map(this))
    }
  }

  class Renderer {
    import Renderer.Context

    def stringCutoffLength: Int     = 100
    def textStreamCutoffLength: Int = 100
    def bytesCutoffLength: Int      = 100
    def byteStreamCutoffLength: Int = 100
    def arrayCutoffLength: Int      = 20
    def mapCutoffLength: Int        = 20

    def render(elem: Element): String = render(new Context(new java.lang.StringBuilder, 0), elem).sb.toString

    def render(c: Context, elem: Element): Context =
      (elem.dataItemShift: @switch) match {
        case DIS.Null         => renderNull(c)
        case DIS.Undefined    => renderUndefined(c)
        case DIS.Boolean      => renderBoolean(c, elem.asInstanceOf[BooleanElem].value)
        case DIS.Int          => renderInt(c, elem.asInstanceOf[IntElem].value)
        case DIS.Long         => renderLong(c, elem.asInstanceOf[LongElem].value)
        case DIS.OverLong     => renderOverLong(c, elem.asInstanceOf[OverLongElem])
        case DIS.Float16      => renderFloat16(c, elem.asInstanceOf[Float16Elem].value)
        case DIS.Float        => renderFloat(c, elem.asInstanceOf[FloatElem].value)
        case DIS.Double       => renderDouble(c, elem.asInstanceOf[DoubleElem].value)
        case DIS.NumberString => renderNumberString(c, elem.asInstanceOf[NumberStringElem].value)
        case DIS.String       => renderString(c, elem.asInstanceOf[StringElem].value)
        case DIS.TextStart    => renderTextStream(c, elem.asInstanceOf[TextStreamElem].elems)
        case DIS.Bytes        => renderBytes(c, elem.asInstanceOf[ByteArrayElem].bytes)
        case DIS.BytesStart   => renderBytesStream(c, elem.asInstanceOf[BytesStreamElem].elems)
        case DIS.SimpleValue  => renderSimpleValue(c, elem.asInstanceOf[SimpleValueElem].value)
        case DIS.ArrayHeader  => renderSizedArray(c, elem.asInstanceOf[ArrayElem.Sized])
        case DIS.ArrayStart   => renderUnsizedArray(c, elem.asInstanceOf[ArrayElem.Unsized])
        case DIS.MapHeader    => renderSizedMap(c, elem.asInstanceOf[MapElem.Sized])
        case DIS.MapStart     => renderUnsizedMap(c, elem.asInstanceOf[MapElem.Unsized])
        case DIS.Tag          => renderTag(c, elem.asInstanceOf[TaggedElem])
      }

    def renderNull(c: Context): Context                        = c.append("null")
    def renderUndefined(c: Context): Context                   = c.append("undefined")
    def renderBoolean(c: Context, value: Boolean): Context     = c.append(value)
    def renderInt(c: Context, value: Int): Context             = c.append(value)
    def renderLong(c: Context, value: Long): Context           = c.append(value).append('l')
    def renderFloat16(c: Context, value: Float): Context       = c.append(value).append("f16")
    def renderFloat(c: Context, value: Float): Context         = c.append(value).append('f')
    def renderDouble(c: Context, value: Double): Context       = c.append(value).append('d')
    def renderNumberString(c: Context, value: String): Context = c.append(value).append('n')

    def renderOverLong(c: Context, elem: OverLongElem): Context = {
      val value = new java.math.BigInteger(1, Util.toBigEndianBytes(elem.value))
      val big   = if (elem.negative) value.not else value
      c.append(big.toString).append("ol")
    }

    def renderString(c: Context, s: String): Context =
      if (s.length <= stringCutoffLength) s.foldLeft(c.append('"'))(renderChar).append('"')
      else s.substring(0, stringCutoffLength).foldLeft(c.append('"'))(renderChar).append("...\"")

    def renderChar(c: Context, char: Char): Context =
      char match {
        case '"'                            => c.append("\\\"")
        case '\t'                           => c.append("\\t")
        case '\r'                           => c.append("\\r")
        case '\n'                           => c.append("\\n")
        case x if Character.isISOControl(x) => c.append("\\u%04x".format(x.toInt))
        case x                              => c.append(x)
      }

    def renderTextStream(c: Context, values: Vector[AbstractTextElem]): Context =
      if (values.nonEmpty) {
        values.iterator
          .slice(1, textStreamCutoffLength)
          .foldLeft(renderTextElem(c.append("TEXT["), values.head))((c, x) => renderTextElem(c.append(", "), x))
          .append(if (values.size <= textStreamCutoffLength) "]" else ", ...]")
      } else c.append("TEXT[]")

    def renderTextElem(c: Context, elem: AbstractTextElem): Context =
      elem match {
        case StringElem(x)     => renderString(c, x)
        case TextStreamElem(x) => renderTextStream(c, x)
      }

    def renderBytes(c: Context, bytes: Array[Byte]): Context =
      if (bytes.nonEmpty) {
        val res = bytes.iterator
          .slice(1, bytesCutoffLength)
          .foldLeft(renderByte(c, bytes.head))((c, x) => renderByte(renderByteSep(c), x))
        (if (bytes.length <= bytesCutoffLength) res else renderByteSep(res).append("...")).append(']')
      } else c.append("BYTES[]")

    def renderByte(c: Context, byte: Byte): Context = c.appendUpperHexString(byte.toInt)
    def renderByteSep(c: Context): Context          = c

    def renderBytesStream(c: Context, values: Vector[AbstractBytesElem]): Context =
      if (values.nonEmpty) {
        values.iterator
          .slice(1, byteStreamCutoffLength)
          .foldLeft(renderBytesElem(c.append("BYTES["), values.head))((c, x) => renderBytesElem(c.append(", "), x))
          .append(if (values.size <= byteStreamCutoffLength) "]" else ", ...]")
      } else c.append("BYTES[]")

    def renderBytesElem(c: Context, elem: AbstractBytesElem): Context =
      elem match {
        case ByteArrayElem(x)   => renderBytes(c, x)
        case BytesStreamElem(x) => renderBytesStream(c, x)
      }

    def renderSimpleValue(c: Context, value: SimpleValue): Context = c.append(value.toString)

    def renderTag(c: Context, elem: TaggedElem): Context =
      render(c.append('@').append(elem.tag.toString).append(' '), elem.value)

    def renderSizedArray(c: Context, elem: ArrayElem.Sized): Context     = renderArray(c, elem.elems)
    def renderUnsizedArray(c: Context, elem: ArrayElem.Unsized): Context = renderArray(c.append('*'), elem.elems)

    def renderArray(c: Context, elems: Vector[Element]): Context =
      if (elems.nonEmpty) {
        val cc = elems.iterator
          .slice(1, arrayCutoffLength)
          .foldLeft(render(renderIndentedNewLine(c.append('[').indented), elems.head)) { (c, x) =>
            render(renderArraySep(c), x)
          }
        val ccc = if (elems.size <= arrayCutoffLength) c else renderArraySep(cc).append("...").dedented
        renderIndentedNewLine(ccc).append(']')
      } else c.append("[]")

    def renderArraySep(c: Context): Context = renderIndentedNewLine(c.append(','))

    def renderSizedMap(c: Context, elem: MapElem.Sized): Context     = renderMap(c, elem.elems)
    def renderUnsizedMap(c: Context, elem: MapElem.Unsized): Context = renderMap(c.append('*'), elem.elems)

    def renderMap(c: Context, elems: Array[Element]): Context =
      if (elems.nonEmpty) {
        @tailrec def rec(remaining: Iterator[Element], c: Context): Context =
          if (remaining.hasNext)
            rec(remaining, renderMapMember(renderMapSep(c), remaining.next(), remaining.next()))
          else c

        val cc = rec(
          elems.iterator.slice(2, mapCutoffLength * 2),
          renderMapMember(renderIndentedNewLine(c.append('{').indented), elems.head, elems(1))
        )
        val ccc = if (elems.size <= mapCutoffLength * 2) c else renderMapSep(cc).append("...").dedented
        renderIndentedNewLine(ccc).append('}')
      } else c.append("{}")

    def renderMapMember(c: Context, key: Element, value: Element): Context =
      render(render(c, key).append(" = "), value)

    def renderMapSep(c: Context): Context = renderArraySep(c)

    def renderIndentedNewLine(c: Context): Context = {
      @tailrec def rec(remaining: Int, c: Context): Context =
        if (remaining > 0) rec(remaining - 1, renderIndentStep(c)) else c
      rec(c.indentLevel, renderNewLine(c))
    }

    def renderNewLine(c: Context): Context    = c.append('\n')
    def renderIndentStep(c: Context): Context = c.append(' ').append(' ')
  }

  object Renderer {

    val default: Renderer = new Renderer

    final class Context(val sb: java.lang.StringBuilder, val indentLevel: Int) {

      def append(s: String): this.type = { sb.append(s); this }
      def append(i: Int): this.type = { sb.append(i); this }
      def append(l: Long): this.type = { sb.append(l); this }
      def append(b: Boolean): this.type = { sb.append(b); this }
      def append(c: Char): this.type = { sb.append(c); this }
      def append(f: Float): this.type = { sb.append(f); this }
      def append(d: Double): this.type = { sb.append(d); this }

      def appendUpperHexString(byte: Int): this.type =
        append(upperHexDigit(byte >> 4) & 0xF).append(upperHexDigit(byte & 0xF))

      def appendLowerHexString(byte: Int): this.type =
        append(lowerHexDigit(byte >> 4) & 0xF).append(lowerHexDigit(byte & 0xF))

      def indented: Context =
        if (indentLevel < 1000000) new Context(sb, indentLevel + 1)
        else throw new IllegalStateException("INDENT overflow")

      def dedented: Context =
        if (indentLevel > 0) new Context(sb, indentLevel - 1)
        else throw new IllegalStateException("Mismatched DEDENT")
    }

    /**
      * Returns the upper-case hex digit corresponding to the last 4 bits of the given Int.
      * (fast branchless implementation)
      * CAUTION: The high bits 28 bits must be zero for this to produce the correct result!
      */
    def upperHexDigit(i: Int) = (48 + i + (7 & ((9 - i) >> 31))).toChar

    /**
      * Returns the lower-case hex digit corresponding to the last 4 bits of the given Int.
      * (fast branchless implementation)
      * CAUTION: The high bits 28 bits must be zero for this to produce the correct result!
      */
    def lowerHexDigit(i: Int) = (48 + i + (39 & ((9 - i) >> 31))).toChar
  }
}
