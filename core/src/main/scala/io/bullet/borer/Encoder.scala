/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import java.lang.{
  Boolean => JBoolean,
  Byte => JByte,
  Double => JDouble,
  Float => JFloat,
  Long => JLong,
  Short => JShort
}
import java.math.{BigDecimal => JBigDecimal, BigInteger => JBigInteger}

import io.bullet.borer.internal.Util

import scala.annotation.tailrec
import scala.collection.LinearSeq

/**
  * Type class responsible for writing an instance of type [[T]] to a [[Writer]].
  */
trait Encoder[T] {
  def write(w: Writer, value: T): Writer
}

object Encoder extends LowPrioEncoders {

  /**
    * An [[Encoder]] that might change its encoding strategy if [[T]] has a default value.
    */
  trait DefaultValueAware[T] extends Encoder[T] {
    def withDefaultValue(defaultValue: T): Encoder[T]
  }

  /**
    * An [[Encoder]] that might not actually produce any output for certain values of [[T]]
    * (e.g. because "not-present" already carries sufficient information).
    */
  trait PossiblyWithoutOutput[T] extends Encoder[T] {
    def producesOutputFor(value: T): Boolean
  }

  /**
    * Creates an [[Encoder]] from the given function.
    */
  def apply[T](encoder: Encoder[T]): Encoder[T] = encoder

  /**
    * Allows for somewhat concise [[Encoder]] definition for case classes, without any macro magic.
    * Can be used e.g. like this:
    *
    * {{{
    * case class Foo(int: Int, string: String, doubleOpt: Option[Double])
    *
    * val fooEncoder = Encoder.from(Foo.unapply _)
    * }}}
    *
    * Encodes an instance as a simple array of values.
    */
  def from[T, Unapplied](unapply: T => Option[Unapplied])(implicit tupleEnc: Encoder[Unapplied]): Encoder[T] =
    Encoder((w, x) => tupleEnc.write(w, unapply(x).get))

  /**
    * Same as the other `from` overload above, but for nullary case classes (i.e. with an empty parameter list).
    */
  def from[T](unapply: T => Boolean): Encoder[T] =
    Encoder((w, x) => if (unapply(x)) w.writeEmptyArray() else sys.error("Unapply unexpectedly failed: " + unapply))

  /**
    * Creates a "unified" [[Encoder]] from two encoders that each target only a single data format.
    */
  def targetSpecific[T](cbor: Encoder[T], json: Encoder[T]): Encoder[T] = { (w, x) =>
    if (w.target == Cbor) cbor.write(w, x)
    else json.write(w, x)
  }

  implicit final class EncoderOps[A](val underlying: Encoder[A]) extends AnyVal {
    def contramap[B](f: B => A): Encoder[B]                     = Encoder((w, b) => underlying.write(w, f(b)))
    def contramapWithWriter[B](f: (Writer, B) => A): Encoder[B] = Encoder((w, b) => underlying.write(w, f(w, b)))

    def withDefaultValue(defaultValue: A): Encoder[A] =
      underlying match {
        case x: Encoder.DefaultValueAware[A] => x withDefaultValue defaultValue
        case x                               => x
      }

    def producesOutputFor(value: A): Boolean =
      underlying match {
        case x: Encoder.PossiblyWithoutOutput[A] => x producesOutputFor value
        case _                                   => true
      }
  }

  implicit def fromCodec[T](implicit codec: Codec[T]): Encoder[T] = codec.encoder

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  implicit val forNull: Encoder[Null]             = Encoder((w, _) => w.writeNull())
  implicit val forBoolean: Encoder[Boolean]       = Encoder(_ writeBoolean _)
  implicit val forChar: Encoder[Char]             = Encoder(_ writeChar _)
  implicit val forByte: Encoder[Byte]             = Encoder(_ writeByte _)
  implicit val forShort: Encoder[Short]           = Encoder(_ writeShort _)
  implicit val forInt: Encoder[Int]               = Encoder(_ writeInt _)
  implicit val forLong: Encoder[Long]             = Encoder(_ writeLong _)
  implicit val forFloat: Encoder[Float]           = Encoder(_ writeFloat _)
  implicit val forDouble: Encoder[Double]         = Encoder(_ writeDouble _)
  implicit val forString: Encoder[String]         = Encoder(_ writeString _)
  implicit val forByteArray: Encoder[Array[Byte]] = Encoder(_ writeBytes _)

  implicit def forBoxedBoolean: Encoder[JBoolean] = forBoolean.asInstanceOf[Encoder[JBoolean]]
  implicit def forBoxedChar: Encoder[Character]   = forChar.asInstanceOf[Encoder[Character]]
  implicit def forBoxedByte: Encoder[JByte]       = forByte.asInstanceOf[Encoder[JByte]]
  implicit def forBoxedShort: Encoder[JShort]     = forShort.asInstanceOf[Encoder[JShort]]
  implicit def forBoxedInt: Encoder[Integer]      = forInt.asInstanceOf[Encoder[Integer]]
  implicit def forBoxedLong: Encoder[JLong]       = forLong.asInstanceOf[Encoder[JLong]]
  implicit def forBoxedFloat: Encoder[JFloat]     = forFloat.asInstanceOf[Encoder[JFloat]]
  implicit def forBoxedDouble: Encoder[JDouble]   = forDouble.asInstanceOf[Encoder[JDouble]]

  implicit val forJBigInteger: Encoder[JBigInteger] =
    Encoder { (w, x) =>
      x.bitLength match {
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
    }

  implicit val forBigInt: Encoder[BigInt] = forJBigInteger.contramap(_.bigInteger)

  implicit val forJBigDecimal: Encoder[JBigDecimal] =
    Encoder { (w, x) =>
      if (w.writingCbor) {
        if (x.scale != 0) w.writeTag(Tag.DecimalFraction).writeArrayHeader(2).writeInt(x.scale)
        w.write(x.unscaledValue)
      } else {
        if (x.scale != 0) w.writeNumberString(x.toString)
        else w.write(x.unscaledValue)
      }
    }

  implicit val forBigDecimal: Encoder[BigDecimal] = forJBigDecimal.contramap(_.bigDecimal)

  implicit val forByteArrayIterator: Encoder[Iterator[Array[Byte]]] =
    Encoder { (w, x) =>
      w.writeBytesStart()
      while (x.hasNext) w.writeBytes(x.next())
      w.writeBreak()
    }

  implicit def forBytesIterator[Bytes: ByteAccess]: Encoder[Iterator[Bytes]] =
    Encoder { (w, x) =>
      w.writeBytesStart()
      while (x.hasNext) w.writeBytes(x.next())
      w.writeBreak()
    }

  implicit val forStringIterator: Encoder[Iterator[String]] =
    Encoder { (w, x) =>
      w.writeTextStart()
      while (x.hasNext) w.writeString(x.next())
      w.writeBreak()
    }

  //#option-encoder
  implicit def forOption[T: Encoder]: Encoder.DefaultValueAware[Option[T]] =
    new Encoder.DefaultValueAware[Option[T]] {

      def write(w: Writer, value: Option[T]) =
        value match {
          case Some(x) => w.writeToArray(x)
          case None    => w.writeEmptyArray()
        }

      def withDefaultValue(defaultValue: Option[T]): Encoder[Option[T]] =
        if (defaultValue eq None) {
          new Encoder.PossiblyWithoutOutput[Option[T]] {
            def producesOutputFor(value: Option[T]) = value ne None
            def write(w: Writer, value: Option[T]) =
              value match {
                case Some(x) => w.write(x)
                case None    => w
              }
          }
        } else this
    }
  //#option-encoder

  implicit def forIndexedSeq[T: Encoder, M[X] <: IndexedSeq[X]]: DefaultValueAware[M[T]] =
    new DefaultValueAware[M[T]] {
      def write(w: Writer, value: M[T]) = w.writeIndexedSeq(value)

      def withDefaultValue(defaultValue: M[T]): Encoder[M[T]] =
        if (defaultValue.isEmpty) {
          new PossiblyWithoutOutput[M[T]] {
            def producesOutputFor(value: M[T]) = value.nonEmpty
            def write(w: Writer, value: M[T])  = if (value.nonEmpty) w.writeIndexedSeq(value) else w
          }
        } else this
    }

  implicit def forLinearSeq[T: Encoder, M[X] <: LinearSeq[X]]: DefaultValueAware[M[T]] =
    new DefaultValueAware[M[T]] {
      def write(w: Writer, value: M[T]) = w.writeLinearSeq(value)

      def withDefaultValue(defaultValue: M[T]): Encoder[M[T]] =
        if (defaultValue.isEmpty) {
          new PossiblyWithoutOutput[M[T]] {
            def producesOutputFor(value: M[T]) = value.nonEmpty
            def write(w: Writer, value: M[T])  = if (value.nonEmpty) w.writeLinearSeq(value) else w
          }
        } else this
    }

  implicit def forMap[A: Encoder, B: Encoder, M[X, Y] <: Map[X, Y]]: DefaultValueAware[M[A, B]] =
    new DefaultValueAware[M[A, B]] {
      def write(w: Writer, value: M[A, B]) = w.writeMap(value)

      def withDefaultValue(defaultValue: M[A, B]): Encoder[M[A, B]] =
        if (defaultValue.isEmpty) {
          new PossiblyWithoutOutput[M[A, B]] {
            def producesOutputFor(value: M[A, B]) = value.nonEmpty
            def write(w: Writer, value: M[A, B])  = if (value.nonEmpty) w.writeMap(value) else w
          }
        } else this
    }

  implicit def forArray[T: Encoder]: Encoder[Array[T]] =
    Encoder { (w, x) =>
      @tailrec def rec(w: Writer, ix: Int): w.type = if (ix < x.length) rec(w.write(x(ix)), ix + 1) else w
      if (w.writingJson) rec(w.writeArrayStart(), 0).writeBreak()
      else rec(w.writeArrayHeader(x.length), 0)
    }

  implicit def forEither[A: Encoder, B: Encoder]: Encoder[Either[A, B]] =
    Encoder { (w, x) =>
      if (w.writingJson) {
        w.writeArrayStart()
        x match {
          case Left(a)  => w.writeToArray(a).writeEmptyArray()
          case Right(b) => w.writeEmptyArray().writeToArray(b)
        }
        w.writeBreak()
      } else
        x match {
          case Left(a)  => w.writeMapHeader(1).writeInt(0).write(a)
          case Right(b) => w.writeMapHeader(1).writeInt(1).write(b)
        }
    }
}

sealed abstract class LowPrioEncoders extends TupleEncoders {

  implicit final def forIterable[T: Encoder, M[X] <: Iterable[X]]: Encoder[M[T]] =
    Encoder {
      case (w, x: IndexedSeq[T]) => w writeIndexedSeq x
      case (w, x: LinearSeq[T])  => w writeLinearSeq x
      case (w, x)                => w writeIterator x.iterator
    }

  implicit final def forIterator[T: Encoder]: Encoder[Iterator[T]] = Encoder(_ writeIterator _)
}
