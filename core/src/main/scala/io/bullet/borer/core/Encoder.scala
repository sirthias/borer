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
import scala.annotation.tailrec

/**
  * Type class containing the logic for writing an instance of type [[T]] to a [[Writer]].
  *
  * @tparam T The type to serialize
  */
trait Encoder[T] {
  def write(w: Writer, value: T): w.type
}

object Encoder extends LowPrioEncoders {

  /**
    * Creates an [[Encoder]] from the given function.
    */
  def apply[T, U](f: (Writer, T) ⇒ U): Encoder[T] =
    new Encoder[T] {
      def write(w: Writer, value: T): w.type = {
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
    * val fooEncoder = Encoder.from(Foo.unapply _) // if you only need an `Encoder` for `Foo`
    * }}}
    */
  def from[T, Unapplied](unapply: T ⇒ Option[Unapplied])(implicit tupleEnc: Encoder[Unapplied]): Encoder[T] =
    Encoder((w, x) ⇒ tupleEnc.write(w, unapply(x).get))

  /**
    * Same as the other `from` overload above, but for nullary case classes (i.e. with an empty parameter list).
    */
  def from[T](unapply: T ⇒ Boolean): Encoder[T] =
    Encoder((w, x) ⇒ if (unapply(x)) w.writeArrayHeader(0) else sys.error("Unapply unexpectedly failed: " + unapply))

  /**
    * Simple macro shortening `Encoder.from(Foo.unapply _)` to `Encoder.forCaseClass[Foo]`
    */
  def forCaseClass[T]: Encoder[T] = macro Macros.encoderForCaseClass[T]

  implicit final class EncoderOps[A](val underlying: Encoder[A]) extends AnyVal {
    def compose[B](f: B ⇒ A): Encoder[B] = Encoder((w, b) ⇒ underlying.write(w, f(b)))
  }

  implicit def fromCodec[T](implicit codec: Codec[T]): Encoder[T] = codec.encoder

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  implicit val forNull: Encoder[Null]             = Encoder((w, _) ⇒ w.writeNull())
  implicit val forBoolean: Encoder[Boolean]       = Encoder(_ writeBool _)
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
    Encoder { (w, x) ⇒
      x.bitLength match {
        case n if n < 32 ⇒ w.writeInt(x.intValue)
        case n if n < 64 ⇒ w.writeLong(x.longValue)
        case 64          ⇒ if (x.signum > 0) w.writePosOverLong(x.longValue) else w.writeNegOverLong(~x.longValue)
        case _ ⇒
          if (x.signum > 0) w.writeTag(Tag.PositiveBigNum).writeBytes(x.toByteArray)
          else w.writeTag(Tag.NegativeBigNum).writeBytes(x.not.toByteArray)
      }
    }

  implicit val forBigInt: Encoder[BigInt] = forJBigInteger.compose(_.bigInteger)

  implicit val forJBigDecimal: Encoder[JBigDecimal] =
    Encoder { (w, x) ⇒
      if (x.scale != 0) w.writeTag(Tag.DecimalFraction).writeArrayHeader(2).writeInt(x.scale)
      w.write(x.unscaledValue)
    }

  implicit val forBigDecimal: Encoder[BigDecimal] = forJBigDecimal.compose(_.bigDecimal)

  implicit val forByteArrayIterator: Encoder[Iterator[Array[Byte]]] =
    Encoder { (w, x) ⇒
      w.writeBytesStart()
      while (x.hasNext) w.writeBytes(x.next())
      w.writeBreak()
    }

  implicit val forStringIterator: Encoder[Iterator[String]] =
    Encoder { (w, x) ⇒
      w.writeTextStart()
      while (x.hasNext) w.writeString(x.next())
      w.writeBreak()
    }

  implicit def forOption[T: Encoder]: Encoder[Option[T]] =
    Encoder {
      case (w, Some(x)) ⇒ w.writeArrayHeader(1).write(x)
      case (w, None)    ⇒ w.writeArrayHeader(0)
    }

  implicit def forIterable[T: Encoder, M[X] <: Iterable[X]]: Encoder[M[T]] =
    Encoder { (w, x) ⇒
      x.foldLeft(w.writeArrayHeader(x.size))(_ write _)
    }

  implicit def forArray[T <: AnyRef: Encoder]: Encoder[Array[T]] =
    Encoder { (w, x) ⇒
      @tailrec def rec(w: Writer, ix: Int): w.type = if (ix < x.length) rec(w.write(x(ix)), ix + 1) else w
      rec(w.writeArrayHeader(x.length), 0)
    }

  implicit def forMap[A: Encoder, B: Encoder, M[X, Y] <: Map[X, Y]]: Encoder[M[A, B]] =
    Encoder { (w, x) ⇒
      w.writeMapHeader(x.size)
      val iterator = x.iterator
      while (iterator.hasNext) {
        val (k, v) = iterator.next()
        w.write(k).write(v)
      }
    }

  implicit def forEither[A: Encoder, B: Encoder]: Encoder[Either[A, B]] =
    Encoder {
      case (w, Left(a))  ⇒ w.writeMapHeader(1).writeInt(0).write(a)
      case (w, Right(b)) ⇒ w.writeMapHeader(1).writeInt(1).write(b)
    }
}

sealed abstract class LowPrioEncoders extends TupleEncoders {

  implicit def forIterator[T: Encoder]: Encoder[Iterator[T]] =
    Encoder { (w, x) ⇒
      w.writeArrayStart()
      while (x.hasNext) w.write(x.next())
      w.writeBreak()
    }
}
