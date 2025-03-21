/*
 * Copyright (c) 2019-2024 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import io.bullet.borer.encodings.BaseEncoding
import io.bullet.borer.internal.{ElementDeque, Util}

import java.lang.{
  Boolean as JBoolean,
  Byte as JByte,
  Double as JDouble,
  Float as JFloat,
  Long as JLong,
  Short as JShort
}
import java.math.{BigDecimal as JBigDecimal, BigInteger as JBigInteger}
import scala.annotation.{tailrec, threadUnsafe}
import scala.collection.LinearSeq
import scala.deriving.Mirror

/**
 * Type class responsible for writing an instance of type [[T]] to a [[Writer]].
 */
trait Encoder[T]:
  def write(w: Writer, value: T): Writer

object Encoder extends LowPrioEncoders:

  /**
   * An [[Encoder]] that might change its encoding strategy if [[T]] has a default value.
   */
  trait DefaultValueAware[T] extends Encoder[T]:
    def withDefaultValue(defaultValue: T): Encoder[T]

  /**
   * An [[Encoder]] that might not actually produce any output for certain values of [[T]]
   * (e.g. because "not-present" already carries sufficient information).
   */
  trait PossiblyWithoutOutput[T] extends Encoder[T]:
    def producesOutputFor(value: T): Boolean

  /**
   * An [[Encoder]] that lazily wraps another [[Encoder]].
   * Useful, for example, for recursive definitions.
   */
  trait Lazy[T] extends Encoder[T]:
    def delegate: Encoder[T]

  /**
   * Creates an [[Encoder]] from the given function.
   */
  inline def apply[T](encoder: Encoder[T]): Encoder[T] = encoder

  /**
   * Gets a given [[Encoder]] for [[T]].
   */
  inline def of[T: Encoder]: Encoder[T] = summon

  /**
   * Creates an [[Encoder]] that encodes a product instance as a simple array of values.
   * Used, for example, as the default 'given' encoder for tuples.
   */
  inline def forProduct[T <: Product: Mirror.ProductOf]: Encoder[T] =
    io.bullet.borer.internal.BasicProductCodec.encoder[T]

  /**
   * Creates a "unified" [[Encoder]] from two encoders that each target only a single data format.
   */
  def targetSpecific[T](cbor: Encoder[T], json: Encoder[T]): Encoder[T] = { (w, x) =>
    if (w.target == Cbor) cbor.write(w, x)
    else json.write(w, x)
  }

  extension [A](underlying: Encoder[A])
    def contramap[B](f: B => A): Encoder[B]                     = Encoder((w, b) => underlying.write(w, f(b)))
    def contramapWithWriter[B](f: (Writer, B) => A): Encoder[B] = Encoder((w, b) => underlying.write(w, f(w, b)))

    def withDefaultValue(defaultValue: A): Encoder[A] =
      underlying.unwrap match
        case x: DefaultValueAware[A] => x.withDefaultValue(defaultValue)
        case x                       => x

    def unwrap: Encoder[A] =
      underlying match
        case x: Lazy[A] => x.delegate.unwrap
        case x          => x

  extension [T](underlying: => Encoder[T])
    /**
     * Wraps an [[Encoder]] definition with lazy initialization.
     */
    def recursive: Encoder[T] =
      new Lazy[T] {
        @threadUnsafe lazy val delegate: Encoder[T] = underlying
        def write(w: Writer, value: T): Writer      = delegate.write(w, value)
      }

    /**
     * Creates a new [[Encoder]] which emits the flat, concatenated encoding of the underlying encoder and the given
     * other one. Only works with encoders that encode to arrays or maps and both encoders must be of the same type,
     * i.e. both encode to an array or both encode to a map.
     * If the encoders are incompatible or produce elements that are not wrapped in an array or map each encoding
     * attempt will fail with a [[Borer.Error.Unsupported]] exception.
     *
     * @param maxBufferSize the maximum size of the buffer for the encoding of the first encoder
     */
    def concat(other: Encoder[T], maxBufferSize: Int = 16384): Encoder[T] =
      new Encoder.ConcatEncoder(underlying, other, maxBufferSize)

  given [T](using codec: Codec[T]): Encoder[T]  = codec.encoder
  given [T](using ev: Codec.All[T]): Encoder[T] = ev.delegate.encoder

  /**
   * Helper type serving only as the target of a `derives Encoder.All` clause.
   * The `borer-derivation` module can then provide the respective `derived` method on the companion object.
   */
  final class All[A](delegate: Encoder[A]) extends Encoder[A]:
    inline def write(w: Writer, value: A): Writer = delegate.write(w, value)

  object All

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  given forNull: Encoder[Null]       = Encoder((w, _) => w.writeNull())
  given forBoolean: Encoder[Boolean] = Encoder(_ writeBoolean _)
  given forChar: Encoder[Char]       = Encoder(_ writeChar _)
  given forByte: Encoder[Byte]       = Encoder(_ writeByte _)
  given forShort: Encoder[Short]     = Encoder(_ writeShort _)
  given forInt: Encoder[Int]         = Encoder(_ writeInt _)
  given forLong: Encoder[Long]       = Encoder(_ writeLong _)
  given forFloat: Encoder[Float]     = Encoder(_ writeFloat _)
  given forDouble: Encoder[Double]   = Encoder(_ writeDouble _)
  given forString: Encoder[String]   = Encoder(_ writeString _)

  given forBoxedBoolean: Encoder[JBoolean] = forBoolean.asInstanceOf[Encoder[JBoolean]]
  given forBoxedChar: Encoder[Character]   = forChar.asInstanceOf[Encoder[Character]]
  given forBoxedByte: Encoder[JByte]       = forByte.asInstanceOf[Encoder[JByte]]
  given forBoxedShort: Encoder[JShort]     = forShort.asInstanceOf[Encoder[JShort]]
  given forBoxedInt: Encoder[Integer]      = forInt.asInstanceOf[Encoder[Integer]]
  given forBoxedLong: Encoder[JLong]       = forLong.asInstanceOf[Encoder[JLong]]
  given forBoxedFloat: Encoder[JFloat]     = forFloat.asInstanceOf[Encoder[JFloat]]
  given forBoxedDouble: Encoder[JDouble]   = forDouble.asInstanceOf[Encoder[JDouble]]

  given forUnit: Encoder[Unit] = Encoder((w, _) => w.writeInt(0))

  given forByteArrayDefault: Encoder[Array[Byte]] = forByteArray(BaseEncoding.base64)

  def forByteArray(jsonBaseEncoding: BaseEncoding): Encoder[Array[Byte]] =
    Encoder { (w, x) =>
      if (w.writingJson) w.writeChars(jsonBaseEncoding.encode(x))
      else w.writeBytes(x)
    }

  given forJBigInteger: Encoder[JBigInteger] =
    Encoder { (w, x) =>
      x.bitLength match
        case n if n < 32        => w.writeInt(x.intValue)
        case n if n < 64        => w.writeLong(x.longValue)
        case 64 if x.signum > 0 => w.writeOverLong(negative = false, x.longValue)
        case 64                 => w.writeOverLong(negative = true, ~x.longValue)
        case _ if w.writingCbor =>
          val bytes = x.toByteArray
          w.writeTag(if (x.signum < 0) {
            Util.inPlaceNegate(bytes); Tag.NegativeBigNum
          } else Tag.PositiveBigNum)
          w.writeBytes(bytes)
        case _ => w.writeNumberString(x.toString(10))
    }

  given forBigInt: Encoder[BigInt] = forJBigInteger.contramap(_.bigInteger)

  given forJBigDecimal: Encoder[JBigDecimal] =
    Encoder { (w, x) =>
      if (w.writingCbor)
        if (x.scale != 0) w.writeTag(Tag.DecimalFraction).writeArrayHeader(2).writeInt(x.scale)
        w.write(x.unscaledValue)
      else if (x.scale != 0) w.writeNumberString(x.toString)
      else w.write(x.unscaledValue)
    }

  given forBigDecimal: Encoder[BigDecimal] = forJBigDecimal.contramap(_.bigDecimal)

  given forByteArrayIterator: Encoder[Iterator[Array[Byte]]] =
    Encoder { (w, x) =>
      w.writeBytesStart()
      while (x.hasNext) w.writeBytes(x.next())
      w.writeBreak()
    }

  given forBytesIterator[Bytes: ByteAccess]: Encoder[Iterator[Bytes]] =
    Encoder { (w, x) =>
      w.writeBytesStart()
      while (x.hasNext) w.writeBytes(x.next())
      w.writeBreak()
    }

  given forStringIterator: Encoder[Iterator[String]] =
    Encoder { (w, x) =>
      w.writeTextStart()
      while (x.hasNext) w.writeString(x.next())
      w.writeBreak()
    }

  // #option-encoder
  given forOption[T: Encoder]: Encoder.DefaultValueAware[Option[T]] =
    new Encoder.DefaultValueAware[Option[T]] {

      def write(w: Writer, value: Option[T]) =
        value match
          case Some(x) => w.writeToArray(x)
          case None    => w.writeEmptyArray()

      def withDefaultValue(defaultValue: Option[T]): Encoder[Option[T]] =
        if (defaultValue eq None)
          new Encoder.PossiblyWithoutOutput[Option[T]] {
            def producesOutputFor(value: Option[T]) = value ne None
            def write(w: Writer, value: Option[T]) =
              value match
                case Some(x) => w.write(x)
                case None    => w
          }
        else this
    }

  // #option-encoder

  given forIndexedSeq[T: Encoder, M[X] <: IndexedSeq[X]]: DefaultValueAware[M[T]] =
    new DefaultValueAware[M[T]] {
      def write(w: Writer, value: M[T]) = w.writeIndexedSeq(value)

      def withDefaultValue(defaultValue: M[T]): Encoder[M[T]] =
        if (defaultValue.isEmpty)
          new PossiblyWithoutOutput[M[T]] {
            def producesOutputFor(value: M[T]) = value.nonEmpty
            def write(w: Writer, value: M[T])  = if (value.nonEmpty) w.writeIndexedSeq(value) else w
          }
        else this
    }

  given forLinearSeq[T: Encoder, M[X] <: LinearSeq[X]]: DefaultValueAware[M[T]] =
    new DefaultValueAware[M[T]] {
      def write(w: Writer, value: M[T]) = w.writeLinearSeq(value)

      def withDefaultValue(defaultValue: M[T]): Encoder[M[T]] =
        if (defaultValue.isEmpty)
          new PossiblyWithoutOutput[M[T]] {
            def producesOutputFor(value: M[T]) = value.nonEmpty
            def write(w: Writer, value: M[T])  = if (value.nonEmpty) w.writeLinearSeq(value) else w
          }
        else this
    }

  given forMap[A: Encoder, B: Encoder, M[X, Y] <: Map[X, Y]]: DefaultValueAware[M[A, B]] =
    new DefaultValueAware[M[A, B]] {
      def write(w: Writer, value: M[A, B]) = w.writeMap(value)

      def withDefaultValue(defaultValue: M[A, B]): Encoder[M[A, B]] =
        if (defaultValue.isEmpty)
          new PossiblyWithoutOutput[M[A, B]] {
            def producesOutputFor(value: M[A, B]) = value.nonEmpty
            def write(w: Writer, value: M[A, B])  = if (value.nonEmpty) w.writeMap(value) else w
          }
        else this
    }

  given forArray[T: Encoder]: Encoder[Array[T]] =
    Encoder { (w, x) =>
      @tailrec def rec(w: Writer, ix: Int): w.type = if (ix < x.length) rec(w.write(x(ix)), ix + 1) else w
      if (w.writingJson) rec(w.writeArrayStart(), 0).writeBreak()
      else rec(w.writeArrayHeader(x.length), 0)
    }

  inline given forTuple[T <: Tuple: Mirror.ProductOf]: Encoder[T] = Encoder.forProduct[T]

  /**
   * The default [[Encoder]] for [[Either]] is not automatically in scope,
   * because there is no clear "standard" way of encoding instances of [[Either]].
   */
  object ForEither:

    given default[A: Encoder, B: Encoder]: Encoder[Either[A, B]] =
      Encoder { (w, x) =>
        if (w.writingJson) w.writeArrayStart() else w.writeMapHeader(1)
        x match
          case Left(a)  => w.writeInt(0).write(a)
          case Right(b) => w.writeInt(1).write(b)
        if (w.writingJson) w.writeBreak() else w
      }

    /**
     * An [[Encoder]] that unpacks the either and writes its content without any wrapping or type information.
     */
    given raw[A: Encoder, B: Encoder]: Encoder[Either[A, B]] =
      Encoder {
        case (w, Left(x))  => w ~ x
        case (w, Right(x)) => w ~ x
      }

  private val _toStringEncoder: Encoder[Any] = Encoder((w, x) => w.writeString(x.toString))
  def toStringEncoder[T]: Encoder[T]         = _toStringEncoder.asInstanceOf[Encoder[T]]

  object StringNumbers:
    given charEncoder: Encoder[Char]     = Encoder.toStringEncoder[Char]
    given byteEncoder: Encoder[Byte]     = Encoder.toStringEncoder[Byte]
    given shortEncoder: Encoder[Short]   = Encoder.toStringEncoder[Short]
    given intEncoder: Encoder[Int]       = Encoder.toStringEncoder[Int]
    given longEncoder: Encoder[Long]     = Encoder.toStringEncoder[Long]
    given floatEncoder: Encoder[Float]   = Encoder.toStringEncoder[Float]
    given doubleEncoder: Encoder[Double] = Encoder.toStringEncoder[Double]

    given boxedCharEncoder: Encoder[Character] = forChar.asInstanceOf[Encoder[Character]]
    given boxedByteEncoder: Encoder[JByte]     = forByte.asInstanceOf[Encoder[JByte]]
    given boxedShortEncoder: Encoder[JShort]   = forShort.asInstanceOf[Encoder[JShort]]
    given boxedIntEncoder: Encoder[Integer]    = forInt.asInstanceOf[Encoder[Integer]]
    given boxedLongEncoder: Encoder[JLong]     = forLong.asInstanceOf[Encoder[JLong]]
    given boxedFloatEncoder: Encoder[JFloat]   = forFloat.asInstanceOf[Encoder[JFloat]]
    given boxedDoubleEncoder: Encoder[JDouble] = forDouble.asInstanceOf[Encoder[JDouble]]

  object StringBooleans:
    given booleanEncoder: Encoder[Boolean]       = Encoder((w, x) => w.writeString(if (x) "true" else "false"))
    given boxedBooleanEncoder: Encoder[JBoolean] = forBoolean.asInstanceOf[Encoder[JBoolean]]

  object StringNulls:
    given nullEncoder: Encoder[Null] = Encoder((w, _) => w.writeString("null"))

  /**
   * Creates a new [[Encoder]] which emits the flat, concatenated encoding of two other encoders.
   * Only works with encoders that encode to arrays or maps and both encoders must be of the same type,
   * i.e. both encode to an array or both encode to a map.
   * If the encoders are incompatible or produce elements that are not wrapped in an array or map each encoding
   * attempt will fail with a [[Borer.Error.Unsupported]] exception.
   *
   * @param maxBufferSize the maximum size of the buffer for the encoding of the first encoder
   */
  final class ConcatEncoder[T](encoder0: Encoder[T], encoder1: Encoder[T], maxBufferSize: Int = 16384)
      extends Encoder[T]:
    if (maxBufferSize <= 0 || !Util.isPowerOf2(maxBufferSize))
      throw new IllegalArgumentException(s"maxBufferSize must be a positive power of two, but was $maxBufferSize")

    def write(w: Writer, value: T): Writer =
      val stash                    = new ElementDeque(maxBufferSize)
      val originalReceiver         = w.receiver
      var arrayOrMap               = 0             // 1 => array, 2 => map
      var len0                     = Long.MinValue // -1 => unbounded
      var len1                     = Long.MinValue // -1 => unbounded
      def unsupported(msg: String) = throw new Borer.Error.Unsupported(w.output, msg)

      w.receiver = new Receiver.WithDefault {

        override def onArrayHeader(length: Long): Unit =
          arrayOrMap = 1
          len0 = length
          w.receiver = stash.appendReceiver

        override def onArrayStart(): Unit =
          arrayOrMap = 1
          len0 = -1
          w.receiver = stash.appendReceiver

        override def onMapHeader(length: Long): Unit =
          arrayOrMap = 2
          len0 = length
          w.receiver = stash.appendReceiver

        override def onMapStart(): Unit =
          arrayOrMap = 2
          len0 = -1
          w.receiver = stash.appendReceiver

        protected def default(t: String): Unit =
          unsupported(s"First Encoder produced $t but Encoder merging only supports 'to-Array' and 'to-Map' Encoders")
      }

      encoder0.write(w, value)

      // stash now contains the complete first encoding, except for the Header/Start element
      if (len0 < 0) stash.dropLastBreakDataItem()

      w.receiver = new Receiver.WithDefault {

        override def onArrayHeader(length: Long): Unit =
          if (arrayOrMap == 1)
            w.receiver = originalReceiver
            len1 = length
            if (len0 >= 0 && len1 >= 0 && len0 + len1 >= 0) w.writeArrayHeader(len0 + len1) else w.writeArrayStart()
            stash.pullAll(originalReceiver)
          else unsupported("Cannot merge a 'to-Map' Encoder with a 'to-Array' Encoder")

        override def onArrayStart(): Unit =
          if (arrayOrMap == 1)
            w.receiver = originalReceiver
            len1 = -1
            w.writeArrayStart()
            stash.pullAll(originalReceiver)
          else unsupported("Cannot merge a 'to-Map' Encoder with a 'to-Array' Encoder")

        override def onMapHeader(length: Long): Unit =
          if (arrayOrMap == 2)
            w.receiver = originalReceiver
            len1 = length
            if (len0 >= 0 && len1 >= 0 && len0 + len1 >= 0) w.writeMapHeader(len0 + len1) else w.writeMapStart()
            stash.pullAll(originalReceiver)
          else unsupported("Cannot merge a 'to-Array' Encoder with a 'to-Map' Encoder")

        override def onMapStart(): Unit =
          if (arrayOrMap == 2)
            w.receiver = originalReceiver
            len1 = -1
            w.writeMapStart()
            stash.pullAll(originalReceiver)
          else unsupported("Cannot merge a 'to-Array' Encoder with a 'to-Map' Encoder")

        protected def default(t: String): Unit =
          unsupported(s"Second Encoder produced $t but Encoder merging only supports 'to-Array' and 'to-Map' Encoders")
      }

      encoder1.write(w, value)

      // if we dropped the terminating break before and encoder1 did not emit one, we need to manually append it
      if (len0 < 0 && len1 >= 0) w.writeBreak()
      w

sealed abstract class LowPrioEncoders:

  given forIterableOnce[T: Encoder, M[X] <: IterableOnce[X]]: Encoder[M[T]] =
    new Encoder.DefaultValueAware[M[T]] { self =>
      def write(w: Writer, value: M[T]) =
        value match
          case x: IndexedSeq[T] => w.writeIndexedSeq(x)
          case x: LinearSeq[T]  => w.writeLinearSeq(x)
          case x                => w.writeIterableOnce(x)

      def withDefaultValue(defaultValue: M[T]): Encoder[M[T]] =
        if (isEmpty(defaultValue))
          new Encoder.PossiblyWithoutOutput[M[T]] {
            def producesOutputFor(value: M[T]) = !isEmpty(value)
            def write(w: Writer, value: M[T])  = if (isEmpty(value)) w else self.write(w, value)
          }
        else this

      private def isEmpty(value: M[T]): Boolean =
        val ks = (value: IterableOnce[T]).knownSize
        ks == 0 || (ks < 0) && value.iterator.isEmpty
    }

  given forIterator[T: Encoder]: Encoder[Iterator[T]] = Encoder(_ writeIterator _)

/**
 * An [[AdtEncoder]] is an [[Encoder]] which encodes its values with an envelope holding the value's type id.
 *
 * It doesn't change or add to the outside interface of [[Encoder]] but merely serves as a marker
 * signaling that it takes on the responsibility of encoding the type id in addition to the value itself.
 * This allows outside encoders calling an [[AdtEncoder]] to delegate this responsibility rather than performing
 * the task themselves.
 */
trait AdtEncoder[T] extends Encoder[T]
