/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.core

import java.lang.{Boolean ⇒ JBoolean, Byte ⇒ JByte, Double ⇒ JDouble, Float ⇒ JFloat, Long ⇒ JLong, Short ⇒ JShort}
import java.math.{BigDecimal ⇒ JBigDecimal, BigInteger ⇒ JBigInteger}
import io.bullet.borer.core.LeastUpperBounds.Lub2
import scala.annotation.tailrec

/**
  * Type class containing the logic for writing an instance of type [[T]] to a [[Writer]].
  *
  * @tparam Bytes The abstraction for byte chunks that this [[Encoder]] relies on.
  *               [[Nothing]] if it has no specific requirements (i.e. only works with plain byte arrays).
  * @tparam T The type to serialize
  */
trait Encoder[+Bytes, T] {
  def write[B >: Bytes](w: Writer[B], value: T): w.type
}

object Encoder extends LowPrioEncoders {

  /**
    * Type alias used for _providing_ a [[Encoder]] for type [[T]], that works with any `Bytes` abstraction
    * (because it either doesn't produce chunks of bytes at all or relies only on plain byte arrays).
    *
    * Should not be used for implicitly _consuming_ an [[Encoder]]!
    * If you need to implicitly _consume_ an [[Encoder]] (like the `Encoder.forOption` method for example)
    * this should rather be done with a free type parameter.
    */
  type Universal[T] = Encoder[Nothing, T]

  /**
    * Type alias used for _consuming_ a concrete [[Encoder]] for type [[T]],
    * when there are no specific requirements as to the `Bytes` abstraction.
    *
    * Should not be used for implicitly _providing_ an [[Encoder]]!
    * If you write an [[Encoder]] that can work with any `Bytes` abstraction
    * (because it either doesn't produce chunks of bytes at all or relies only on plain byte arrays)
    * then give it type `Encoder.Universal[T]` instead.
    */
  type Default[T] = Encoder[Array[Byte], T]

  /**
    * Creates an [[Encoder]] from the given function.
    */
  def apply[Bytes, T, U](f: (Writer[Bytes], T) ⇒ U): Encoder[Bytes, T] =
    new Encoder[Bytes, T] {
      def write[B >: Bytes](w: Writer[B], value: T): w.type = {
        f(w, value)
        w
      }
    }

  /**
    * Allows for concise [[Encoder]] definition for case classes, without any macro magic.
    * Can be used e.g. like this:
    *
    * {{{
    * case class Foo(int: Int, string: String, doubleOpt: Option[Double])
    *
    * val fooEncoder = Encoder(Foo.unapply) // if you only need an `Encoder` for `Foo`
    * val fooCodec = Codec.of[Foo](Foo.unapply, Foo.apply) // if you need a full `Codec` for `Foo`
    * }}}
    */
  implicit def from[T, Tuple, Bytes](unapply: T ⇒ Option[Tuple])(
      implicit tupleEncoder: Encoder[Bytes, Tuple]): Encoder[Bytes, T] =
    Encoder((w, x) ⇒ tupleEncoder.write(w, unapply(x).get))

  implicit def from[T, Bytes](unapply: T ⇒ Boolean): Encoder[Bytes, T] =
    Encoder((w, x) ⇒ if (unapply(x)) w.writeArrayHeader(0) else sys.error("Unapply unexpectedly failed: " + unapply))

  implicit final class EncoderOps[Bytes, A](val underlying: Encoder[Bytes, A]) extends AnyVal {
    def compose[B](f: B ⇒ A): Encoder[Bytes, B] = Encoder((w, b) ⇒ underlying.write(w, f(b)))
  }

  implicit def fromCodec[Bytes, T](implicit codec: Codec[Bytes, _, T]): Encoder[Bytes, T] = codec.encoder

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  implicit val forNull: Encoder.Universal[Null]             = Encoder((w, _) ⇒ w.writeNull())
  implicit val forBoolean: Encoder.Universal[Boolean]       = Encoder(_ writeBool _)
  implicit val forChar: Encoder.Universal[Char]             = Encoder(_ writeChar _)
  implicit val forByte: Encoder.Universal[Byte]             = Encoder(_ writeByte _)
  implicit val forShort: Encoder.Universal[Short]           = Encoder(_ writeShort _)
  implicit val forInt: Encoder.Universal[Int]               = Encoder(_ writeInt _)
  implicit val forLong: Encoder.Universal[Long]             = Encoder(_ writeLong _)
  implicit val forFloat: Encoder.Universal[Float]           = Encoder(_ writeFloat _)
  implicit val forDouble: Encoder.Universal[Double]         = Encoder(_ writeDouble _)
  implicit val forString: Encoder.Universal[String]         = Encoder(_ writeString _)
  implicit val forByteArray: Encoder.Universal[Array[Byte]] = Encoder(_ writeByteArray _)

  implicit def forBoxedBoolean: Encoder.Universal[JBoolean] = forBoolean.asInstanceOf[Encoder.Universal[JBoolean]]
  implicit def forBoxedChar: Encoder.Universal[Character]   = forChar.asInstanceOf[Encoder.Universal[Character]]
  implicit def forBoxedByte: Encoder.Universal[JByte]       = forByte.asInstanceOf[Encoder.Universal[JByte]]
  implicit def forBoxedShort: Encoder.Universal[JShort]     = forShort.asInstanceOf[Encoder.Universal[JShort]]
  implicit def forBoxedInt: Encoder.Universal[Integer]      = forInt.asInstanceOf[Encoder.Universal[Integer]]
  implicit def forBoxedLong: Encoder.Universal[JLong]       = forLong.asInstanceOf[Encoder.Universal[JLong]]
  implicit def forBoxedFloat: Encoder.Universal[JFloat]     = forFloat.asInstanceOf[Encoder.Universal[JFloat]]
  implicit def forBoxedDouble: Encoder.Universal[JDouble]   = forDouble.asInstanceOf[Encoder.Universal[JDouble]]

  implicit val forJBigInteger: Encoder.Universal[JBigInteger] =
    Encoder { (w, x) ⇒
      x.bitLength match {
        case n if n < 32 ⇒ w.writeInt(x.intValue)
        case n if n < 64 ⇒ w.writeLong(x.longValue)
        case 64          ⇒ if (x.signum > 0) w.writePosOverLong(x.longValue) else w.writeNegOverLong(~x.longValue)
        case _ ⇒
          if (x.signum > 0) w.writeTag(Tag.PositiveBigNum).writeByteArray(x.toByteArray)
          else w.writeTag(Tag.NegativeBigNum).writeByteArray(x.not.toByteArray)
      }
    }

  implicit val forBigInt: Encoder.Universal[BigInt] = forJBigInteger.compose(_.bigInteger)

  implicit val forJBigDecimal: Encoder.Universal[JBigDecimal] =
    Encoder { (w, x) ⇒
      if (x.scale != 0) w.writeTag(Tag.DecimalFraction).writeArrayHeader(2).writeInt(x.scale)
      w.write(x.unscaledValue)
    }

  implicit val forBigDecimal: Encoder.Universal[BigDecimal] = forJBigDecimal.compose(_.bigDecimal)

  implicit val forByteArrayIterator: Encoder.Universal[Iterator[Array[Byte]]] =
    Encoder { (w, x) ⇒
      w.writeBytesStart()
      while (x.hasNext) w.writeByteArray(x.next())
      w.writeBreak()
    }

  implicit def forStringIterator[Bytes]: Encoder[Bytes, Iterator[String]] =
    Encoder { (w, x) ⇒
      w.writeTextStart()
      while (x.hasNext) w.writeString(x.next())
      w.writeBreak()
    }

  implicit def forOption[Bytes, T](implicit e: Encoder[Bytes, T]): Encoder[Bytes, Option[T]] =
    Encoder {
      case (w, Some(x)) ⇒ w.writeArrayHeader(1).write(x)
      case (w, None)    ⇒ w.writeArrayHeader(0)
    }

  implicit def forIterable[Bytes, T, M[X] <: Iterable[X]](implicit e: Encoder[Bytes, T]): Encoder[Bytes, M[T]] =
    Encoder { (w, x) ⇒
      x.foldLeft(w.writeArrayHeader(x.size))(_ write _)
    }

  implicit def forArray[Bytes, T <: AnyRef](implicit e: Encoder[Bytes, T]): Encoder[Bytes, Array[T]] =
    Encoder { (w, x) ⇒
      @tailrec def rec(w: Writer[Bytes], ix: Int): w.type = if (ix < x.length) rec(w.write(x(ix)), ix + 1) else w
      rec(w.writeArrayHeader(x.length), 0)
    }

  implicit def forMap[A, B, M[X, Y] <: Map[X, Y], Bytes, Bytes1 <: Bytes, Bytes2 <: Bytes](
      implicit ea: Encoder[Bytes1, A],
      eb: Encoder[Bytes2, B],
      lub: Lub2[Bytes1, Bytes2, Bytes]): Encoder[Bytes, M[A, B]] =
    Encoder { (w, x) ⇒
      w.writeMapHeader(x.size)
      val iterator = x.iterator
      while (iterator.hasNext) {
        val (k, v) = iterator.next()
        w.write(k).write(v)
      }
    }

  implicit def forEither[A, B, Bytes, Bytes1 <: Bytes, Bytes2 <: Bytes](
      implicit ea: Encoder[Bytes1, A],
      eb: Encoder[Bytes2, B],
      lub: Lub2[Bytes1, Bytes2, Bytes]): Encoder[Bytes, Either[A, B]] =
    Encoder {
      case (w, Left(a))  ⇒ w.writeMapHeader(1).writeInt(0).write(a)
      case (w, Right(b)) ⇒ w.writeMapHeader(1).writeInt(1).write(b)
    }
}

sealed abstract class LowPrioEncoders extends TupleEncoders {

  implicit def forIterator[Bytes, T](implicit e: Encoder[Bytes, T]): Encoder[Bytes, Iterator[T]] =
    Encoder { (w, x) ⇒
      w.writeArrayStart()
      while (x.hasNext) w.write(x.next())
      w.writeBreak()
    }
}
