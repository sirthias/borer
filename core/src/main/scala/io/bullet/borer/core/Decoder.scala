/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.core

import java.lang.{Boolean ⇒ JBoolean, Byte ⇒ JByte, Short ⇒ JShort, Long ⇒ JLong, Float ⇒ JFloat, Double ⇒ JDouble}
import java.math.{BigDecimal ⇒ JBigDecimal, BigInteger ⇒ JBigInteger}

import scala.annotation.tailrec
import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.{ListMap, TreeMap}
import scala.collection.mutable

/**
  * Type class containing the logic for reading an instance of type [[T]] from a [[Reader]].
  *
  * @tparam Bytes The abstraction for byte chunks that this [[Decoder]] relies on.
  *               [[Any]] if it has no specific requirements (i.e. only works with plain byte arrays).
  * @tparam T The type to deserialize
  */
trait Decoder[-Bytes, T] {
  def read(r: Reader[Bytes]): T
}

object Decoder extends LowPrioDecoders {

  /**
    * Type alias used for _providing_ a [[Decoder]] for type [[T]], that works with any `Bytes` abstraction
    * (because it either doesn't consume chunks of bytes at all or relies only on plain byte arrays).
    *
    * Should not be used for implicitly _consuming_ a [[Decoder]]!
    * If you need to implicitly _consume_ a [[Decoder]] (like the `Decoder.forOption` method for example)
    * this should rather be done with a free type parameter.
    */
  type Universal[T] = Decoder[Any, T]

  /**
    * Type alias used for _consuming_ a concrete [[Decoder]] for type [[T]],
    * when there are no specific requirements as to the `Bytes` abstraction.
    *
    * Should not be used for implicitly _providing_ a [[Decoder]]!
    * If you write a [[Decoder]] that can work with any `Bytes` abstraction
    * (because it either doesn't consume chunks of bytes at all or relies only on plain byte arrays)
    * then give it type `Decoder.Universal[T]` instead.
    */
  type Default[T] = Decoder[Array[Byte], T]

  /**
    * Creates a [[Decoder]] from the given function.
    */
  def apply[Bytes, T](f: Reader[Bytes] ⇒ T): Decoder[Bytes, T] = f(_)

  private[this] val builderSingleton = new Builder[Any, Nothing]

  /**
    * Allows for [[Decoder]] definition without having to supply the `Bytes` type parameter.
    */
  def of[T]: Builder[Any, T] = builderSingleton.asInstanceOf[Builder[Any, T]]

  final class Builder[Bytes, T] private[Decoder] {
    def from(f: Reader[Bytes] ⇒ T): Decoder[Bytes, T] = Decoder(f)
    def withBytes[B]: Builder[B, T]                   = builderSingleton.asInstanceOf[Builder[B, T]]
  }

  implicit final class DecoderOps[Bytes, A](val underlying: Decoder[Bytes, A]) extends AnyVal {
    def map[B](f: A ⇒ B)                            = Decoder[Bytes, B](r ⇒ f(underlying.read(r)))
    def mapWithReader[B](f: (Reader[Bytes], A) ⇒ B) = Decoder[Bytes, B](r ⇒ f(r, underlying.read(r)))
  }

  implicit def fromCodec[Bytes, T](implicit codec: Codec[_, Bytes, T]): Decoder[Bytes, T] = codec.decoder

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  implicit val forNull: Decoder.Universal[Null]             = Decoder.of[Null].from(_.readNull())
  implicit val forBoolean: Decoder.Universal[Boolean]       = Decoder.of[Boolean].from(_.readBoolean())
  implicit val forInt: Decoder.Universal[Int]               = Decoder.of[Int].from(_.readInt())
  implicit val forLong: Decoder.Universal[Long]             = Decoder.of[Long].from(_.readLong())
  implicit val forFloat: Decoder.Universal[Float]           = Decoder.of[Float].from(_.readFloat())
  implicit val forDouble: Decoder.Universal[Double]         = Decoder.of[Double].from(_.readDouble())
  implicit val forString: Decoder.Universal[String]         = Decoder.of[String].from(_.readString())
  implicit val forByteArray: Decoder.Universal[Array[Byte]] = Decoder.of[Array[Byte]].from(_.readByteArray())

  implicit val forChar: Decoder.Universal[Char] = forInt.mapWithReader { (r, int) ⇒
    if ((int >> 16) != 0) r.validationFailure(s"Cannot convert int value [$int] to Char")
    int.toChar
  }
  implicit val forByte: Decoder.Universal[Byte] = forInt.mapWithReader { (r, int) ⇒
    if ((int >> 8) != (int >> 31)) r.validationFailure(s"Cannot convert int value [$int] to Byte")
    int.toByte
  }
  implicit val forShort: Decoder.Universal[Short] = forInt.mapWithReader { (r, int) ⇒
    if ((int >> 16) != (int >> 31)) r.validationFailure(s"Cannot convert int value [$int] to Short")
    int.toShort
  }

  implicit def forBoxedBoolean: Decoder.Universal[JBoolean] = forBoolean.asInstanceOf[Decoder.Universal[JBoolean]]
  implicit def forBoxedChar: Decoder.Universal[Character]   = forChar.asInstanceOf[Decoder.Universal[Character]]
  implicit def forBoxedByte: Decoder.Universal[JByte]       = forByte.asInstanceOf[Decoder.Universal[JByte]]
  implicit def forBoxedShort: Decoder.Universal[JShort]     = forShort.asInstanceOf[Decoder.Universal[JShort]]
  implicit def forBoxedInt: Decoder.Universal[Integer]      = forInt.asInstanceOf[Decoder.Universal[Integer]]
  implicit def forBoxedLong: Decoder.Universal[JLong]       = forLong.asInstanceOf[Decoder.Universal[JLong]]
  implicit def forBoxedFloat: Decoder.Universal[JFloat]     = forFloat.asInstanceOf[Decoder.Universal[JFloat]]
  implicit def forBoxedDouble: Decoder.Universal[JDouble]   = forDouble.asInstanceOf[Decoder.Universal[JDouble]]

  implicit val forJBigInteger: Decoder.Universal[JBigInteger] =
    Decoder.of[JBigInteger].from { r ⇒
      def fromOverLong(long: Long) = new JBigInteger(1, Util.toBigEndianBytes(long))
      def fromByteArray()          = new JBigInteger(1, r.readByteArray())
      r.dataItem match {
        case DataItem.Int | DataItem.Long          ⇒ JBigInteger.valueOf(r.readLong())
        case DataItem.PosOverLong                  ⇒ fromOverLong(r.readPosOverLong())
        case DataItem.NegOverLong                  ⇒ fromOverLong(r.readNegOverLong()).not
        case _ if r.tryReadTag(Tag.PositiveBigNum) ⇒ fromByteArray()
        case _ if r.tryReadTag(Tag.NegativeBigNum) ⇒ fromByteArray().not
        case _                                     ⇒ r.unexpectedDataItem(expected = "BigInteger")
      }
    }

  implicit val forBigInteger: Decoder.Universal[BigInt] = forJBigInteger.map(BigInt(_))

  implicit val forJBigDecimal: Decoder.Universal[JBigDecimal] =
    Decoder.of[JBigDecimal].from { r ⇒
      val IntLongOrOverLong = DataItem.Int | DataItem.Long | DataItem.PosOverLong | DataItem.NegOverLong
      if (r.has(IntLongOrOverLong) || r.hasTag(Tag.PositiveBigNum) || r.hasTag(Tag.NegativeBigNum)) {
        new JBigDecimal(forJBigInteger.read(r))
      } else {
        r.readTag(Tag.DecimalFraction)
        if (r.hasArrayHeader) {
          val len = r.readArrayHeader()
          if (len == 2) {
            if (r.hasInt) {
              val exp      = r.readInt()
              val mantissa = if (r.hasLong) JBigInteger.valueOf(r.readLong()) else r.read[JBigInteger]()
              new JBigDecimal(mantissa, exp)
            } else r.unexpectedDataItem(expected = "BigDecimal exponent as Int")
          } else r.unexpectedDataItem(expected = "Array of length 2", actual = s"Array of length $len")
        } else r.unexpectedDataItem(expected = "BigDecimal")
      }
    }

  implicit val forBigDecimal: Decoder.Universal[BigDecimal] = forJBigDecimal.map(BigDecimal(_))

  implicit def forOption[Bytes, T](implicit d: Decoder[Bytes, T]): Decoder[Bytes, Option[T]] =
    Decoder[Bytes, Option[T]] { r ⇒
      r.readArrayHeader() match {
        case 0 ⇒ None
        case 1 ⇒ Some(r[T])
        case x ⇒ r.unexpectedDataItem("Array with length 0 or 1", s"Array with length $x")
      }
    }

  implicit def forIterable[Bytes, T, M[X] <: Iterable[X]](implicit d: Decoder[Bytes, T],
                                                          cbf: CanBuildFrom[M[T], T, M[T]]): Decoder[Bytes, M[T]] =
    Decoder[Bytes, M[T]] { r ⇒
      if (r.hasArrayHeader) {
        @tailrec def rec(remaining: Int, b: mutable.Builder[T, M[T]]): M[T] =
          if (remaining > 0) rec(remaining - 1, b += r[T]) else b.result()
        val size = r.readArrayHeader()
        if (size <= Int.MaxValue) {
          val intSize = size.toInt
          val builder = cbf()
          builder.sizeHint(intSize)
          rec(intSize, builder)
        } else r.overflow(s"Cannot deserialize Iterable with size $size (> Int.MaxValue)")
      } else if (r.hasArrayStart) {
        r.readArrayStart()
        @tailrec def rec(b: mutable.Builder[T, M[T]]): M[T] =
          if (r.tryReadBreak()) b.result() else rec(b += r[T])
        rec(cbf())
      } else r.unexpectedDataItem(expected = "Array for deserializing an Iterable instance")
    }

  implicit def forArray[Bytes, T <: AnyRef](implicit d: Decoder[Bytes, T]): Decoder[Bytes, Array[T]] =
    Decoder[Bytes, Array[T]] { r ⇒
      val size = r.readArrayHeader()
      if (size <= Int.MaxValue) {
        val intSize                         = size.toInt
        val array                           = new Array[AnyRef](intSize).asInstanceOf[Array[T]]
        @tailrec def rec(ix: Int): Array[T] = if (ix < intSize) { array(ix) = r[T]; rec(ix + 1) } else array
        rec(intSize)
      } else r.overflow(s"Cannot deserialize Array with size $size (> Int.MaxValue)")
    }

  implicit def forTreeMap[Bytes, A: Ordering, B](implicit da: Decoder[Bytes, A],
                                                 db: Decoder[Bytes, B]): Decoder[Bytes, TreeMap[A, B]] =
    constructForMap[Bytes, A, B, TreeMap[A, B]](TreeMap.empty)

  implicit def forListMap[Bytes, A, B](implicit da: Decoder[Bytes, A],
                                       db: Decoder[Bytes, B]): Decoder[Bytes, ListMap[A, B]] =
    constructForMap[Bytes, A, B, ListMap[A, B]](ListMap.empty)

  implicit def forEither[Bytes, A, B](implicit da: Decoder[Bytes, A],
                                      db: Decoder[Bytes, B]): Decoder[Bytes, Either[A, B]] =
    Decoder[Bytes, Either[A, B]] { r ⇒
      r.readMapHeader(1).readInt() match {
        case 0 ⇒ Left(r[A])
        case 1 ⇒ Right(r[B])
        case x ⇒
          r.unexpectedDataItem(
            expected = "Map entry with key 0 or 1 for decoding an `Either` instance",
            actual = s"Map entry with key $x")
      }
    }
}

sealed abstract class LowPrioDecoders extends TupleDecoders {

  implicit def forMap[Bytes, A, B](implicit da: Decoder[Bytes, A], db: Decoder[Bytes, B]): Decoder[Bytes, Map[A, B]] =
    constructForMap[Bytes, A, B, Map[A, B]](Map.empty)

  def constructForMap[Bytes, A, B, M <: Map[A, B]](empty: M)(implicit da: Decoder[Bytes, A],
                                                             db: Decoder[Bytes, B]): Decoder[Bytes, M] =
    Decoder[Bytes, M] { r ⇒
      if (r.hasMapHeader) {
        @tailrec def rec(remaining: Int, map: Map[A, B]): M =
          if (remaining > 0) rec(remaining - 1, map.updated(r[A], r[B])) else map.asInstanceOf[M]
        val size = r.readMapHeader()
        if (size <= Int.MaxValue) rec(size.toInt, empty)
        else r.overflow(s"Cannot deserialize Map with size $size (> Int.MaxValue)")
      } else if (r.hasMapStart) {
        r.readMapStart()
        @tailrec def rec(map: Map[A, B]): M =
          if (r.tryReadBreak()) map.asInstanceOf[M] else rec(map.updated(r[A], r[B]))
        rec(empty)
      } else r.unexpectedDataItem(expected = "Map")
    }
}
