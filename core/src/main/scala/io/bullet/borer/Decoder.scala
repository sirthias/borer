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
import scala.collection.compat._
import scala.collection.immutable.{HashMap, ListMap, TreeMap}
import scala.collection.mutable
import scala.reflect.ClassTag

/**
  * Type class responsible for reading an instance of type [[T]] from a [[Reader]].
  */
trait Decoder[T] {
  def read(r: Reader): T
}

object Decoder extends LowPrioDecoders {
  import io.bullet.borer.{DataItem => DI}

  trait DefaultValueAware[T] extends Decoder[T] {
    def withDefaultValue(defaultValue: T): Decoder[T]
  }

  /**
    * Creates a [[Decoder]] from the given function.
    */
  def apply[T](decoder: Decoder[T]): Decoder[T] = decoder

  /**
    * Creates a "unified" [[Decoder]] from two decoders that each target only a single data format.
    */
  def targetSpecific[T](cbor: Decoder[T], json: Decoder[T]): Decoder[T] = { r =>
    if (r.target == Cbor) cbor.read(r) else json.read(r)
  }

  implicit final class DecoderOps[A](val underlying: Decoder[A]) extends AnyVal {
    def map[B](f: A => B): Decoder[B]                     = Decoder(r => f(underlying.read(r)))
    def mapWithReader[B](f: (Reader, A) => B): Decoder[B] = Decoder(r => f(r, underlying.read(r)))

    def withDefaultValue(defaultValue: A): Decoder[A] =
      underlying match {
        case x: Decoder.DefaultValueAware[A] => x withDefaultValue defaultValue
        case x                               => x
      }
  }

  implicit def fromCodec[T](implicit codec: Codec[T]): Decoder[T] = codec.decoder

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  implicit val forNull: Decoder[Null]       = Decoder(_.readNull())
  implicit val forBoolean: Decoder[Boolean] = Decoder(_.readBoolean())
  implicit val forInt: Decoder[Int]         = Decoder(_.readInt())
  implicit val forLong: Decoder[Long]       = Decoder(_.readLong())
  implicit val forFloat: Decoder[Float]     = Decoder(_.readFloat())
  implicit val forDouble: Decoder[Double]   = Decoder(_.readDouble())
  implicit val forString: Decoder[String]   = Decoder(_.readString())

  implicit val forByteArray: Decoder[Array[Byte]] =
    Decoder { r =>
      if (r.hasByteArray) {
        r.readByteArray()
      } else if (r.hasArrayHeader) {
        val size = r.readArrayHeader()
        if (size > 0) {
          if (size <= Int.MaxValue) {
            val intSize = size.toInt
            val array   = new Array[Byte](intSize)

            @tailrec def rec(ix: Int): Array[Byte] =
              if (ix < intSize) {
                array(ix) = r.readByte()
                rec(ix + 1)
              } else array

            rec(0)
          } else r.overflow(s"Cannot deserialize ByteArray with size $size (> Int.MaxValue)")
        } else Array.emptyByteArray
      } else if (r.tryReadArrayStart()) {
        if (!r.tryReadBreak()) {
          r.readUntilBreak(new mutable.ArrayBuilder.ofByte)(_ += r.readByte()).result()
        } else Array.emptyByteArray
      } else r.unexpectedDataItem(expected = "ByteString or Array of bytes")
    }

  implicit val forChar: Decoder[Char] = forInt.mapWithReader { (r, int) =>
    if ((int >> 16) != 0) r.validationFailure(s"Cannot convert int value $int to Char")
    int.toChar
  }
  implicit val forByte: Decoder[Byte] = forInt.mapWithReader { (r, int) =>
    if ((int >> 8) != (int >> 31)) r.validationFailure(s"Cannot convert int value $int to Byte")
    int.toByte
  }
  implicit val forShort: Decoder[Short] = forInt.mapWithReader { (r, int) =>
    if ((int >> 16) != (int >> 31)) r.validationFailure(s"Cannot convert int value $int to Short")
    int.toShort
  }

  implicit def forBoxedBoolean: Decoder[JBoolean] = forBoolean.asInstanceOf[Decoder[JBoolean]]
  implicit def forBoxedChar: Decoder[Character]   = forChar.asInstanceOf[Decoder[Character]]
  implicit def forBoxedByte: Decoder[JByte]       = forByte.asInstanceOf[Decoder[JByte]]
  implicit def forBoxedShort: Decoder[JShort]     = forShort.asInstanceOf[Decoder[JShort]]
  implicit def forBoxedInt: Decoder[Integer]      = forInt.asInstanceOf[Decoder[Integer]]
  implicit def forBoxedLong: Decoder[JLong]       = forLong.asInstanceOf[Decoder[JLong]]
  implicit def forBoxedFloat: Decoder[JFloat]     = forFloat.asInstanceOf[Decoder[JFloat]]
  implicit def forBoxedDouble: Decoder[JDouble]   = forDouble.asInstanceOf[Decoder[JDouble]]

  def forJBigInteger(maxCborByteArraySize: Int = 64, maxJsonNumberStringLength: Int = 64): Decoder[JBigInteger] =
    Decoder { r =>
      def fromByteArray() = {
        val byteArray = r.readByteArray()
        if (byteArray.length > maxCborByteArraySize) {
          r.overflow(
            "ByteArray for decoding JBigInteger is longer than the configured max of " + maxCborByteArraySize + " bytes")
        } else new JBigInteger(1, byteArray)
      }
      r.dataItem match {
        case DI.Int | DI.Long => JBigInteger.valueOf(r.readLong())
        case DI.OverLong =>
          def value = new JBigInteger(1, Util.toBigEndianBytes(r.readOverLong()))
          if (r.overLongNegative) value.not else value
        case DI.NumberString =>
          val numberString = r.readNumberString()
          if (numberString.length > maxJsonNumberStringLength) {
            r.overflow(
              "NumberString for decoding JBigInteger is longer than the configured max of " + maxJsonNumberStringLength + " characters")
          } else new JBigInteger(numberString)
        case _ if r.tryReadTag(Tag.PositiveBigNum) => fromByteArray()
        case _ if r.tryReadTag(Tag.NegativeBigNum) => fromByteArray().not
        case _                                     => r.unexpectedDataItem(expected = "BigInteger")
      }
    }

  implicit val _forJBigInteger: Decoder[JBigInteger] = forJBigInteger()

  implicit val forBigInteger: Decoder[BigInt] = _forJBigInteger.map(BigInt(_))

  implicit def forJBigDecimal(
      maxCborBigIntMantissaByteArraySize: Int = 64,
      maxCborAbsExponent: Int = 999,
      maxJsonNumberStringLength: Int = 64): Decoder[JBigDecimal] = {
    val bigIntMantissaDecoder = forJBigInteger(maxCborByteArraySize = maxCborBigIntMantissaByteArraySize)
    Decoder { r =>
      def fromBigInteger() = new JBigDecimal(_forJBigInteger.read(r))
      r.dataItem match {
        case DI.Int | DI.Long | DI.OverLong => fromBigInteger()
        case DI.Double                      => JBigDecimal.valueOf(r.readDouble())
        case DI.NumberString =>
          val numberString = r.readNumberString()
          if (numberString.length > maxJsonNumberStringLength) {
            r.overflow(
              "NumberString for decoding JBigDecimal is longer than the configured max of " + maxJsonNumberStringLength + " characters")
          } else new JBigDecimal(numberString)
        case _ if r.hasTag(Tag.PositiveBigNum) | r.hasTag(Tag.NegativeBigNum) => fromBigInteger()
        case _ if r.tryReadTag(Tag.DecimalFraction) =>
          if (r.hasArrayHeader) {
            val len = r.readArrayHeader()
            if (len == 2) {
              if (r.hasInt) {
                val exp = r.readInt()
                if (math.abs(exp) <= maxCborAbsExponent) {
                  val mantissa = bigIntMantissaDecoder.read(r)
                  new JBigDecimal(mantissa, exp)
                } else
                  r.overflow(
                    s"Absolute value of JBigDecimal exponent $exp is > the configured max of " + maxCborAbsExponent)
              } else r.unexpectedDataItem(expected = "BigDecimal exponent as Int")
            } else r.unexpectedDataItem("Array of length 2 for decoding a `BigDecimal`", s"Array of length $len")
          } else r.unexpectedDataItem(expected = "BigDecimal")
      }
    }
  }

  implicit val _forJBigDecimal: Decoder[JBigDecimal] = forJBigDecimal()

  implicit val forBigDecimal: Decoder[BigDecimal] = _forJBigDecimal.map(BigDecimal(_))

  implicit def forOption[T: Decoder]: Decoder.DefaultValueAware[Option[T]] =
    new Decoder.DefaultValueAware[Option[T]] {

      def read(r: Reader) = {
        if (r.hasArrayHeader) {
          r.readArrayHeader() match {
            case 0 => None
            case 1 => Some(r.read[T]())
            case x => r.unexpectedDataItem("Array with length 0 or 1 for decoding an `Option`", s"Array with length $x")
          }
        } else if (r.tryReadArrayStart()) {
          if (r.tryReadBreak()) None
          else {
            val x = r.read[T]()
            if (r.tryReadBreak()) Some(x)
            else
              r.unexpectedDataItem(
                "Array with length 0 or 1 for decoding an `Option`",
                "Array with more than one element")
          }
        } else r.unexpectedDataItem("Array with length 0 or 1 for decoding an `Option`")
      }

      def withDefaultValue(defaultValue: Option[T]): Decoder[Option[T]] =
        if (defaultValue ne None) this
        else Decoder[Option[T]](r => Some(r.read[T]()))
    }

  implicit def forIterable[T: Decoder, M[X] <: Iterable[X]](implicit factory: Factory[T, M[T]]): Decoder[M[T]] =
    Decoder { r =>
      if (r.hasArrayHeader) {
        @tailrec def rec(remaining: Int, b: mutable.Builder[T, M[T]]): M[T] =
          if (remaining > 0) rec(remaining - 1, b += r[T]) else b.result()
        val size = r.readArrayHeader()
        if (size <= Int.MaxValue) {
          val intSize = size.toInt
          val builder = factory.newBuilder
          builder.sizeHint(intSize)
          rec(intSize, builder)
        } else r.overflow(s"Cannot deserialize Iterable with size $size (> Int.MaxValue)")
      } else if (r.tryReadArrayStart()) {
        r.readUntilBreak[M, T]()
      } else r.unexpectedDataItem(expected = "Array for deserializing an Iterable instance")
    }

  implicit def forArray[T: ClassTag: Decoder]: Decoder[Array[T]] =
    Decoder { r =>
      if (r.hasArrayHeader) {
        val size = r.readArrayHeader()
        if (size > 0) {
          if (size <= Int.MaxValue) {
            val intSize = size.toInt
            val array   = Array.ofDim[T](intSize)

            @tailrec def rec(ix: Int): Array[T] =
              if (ix < intSize) {
                array(ix) = r.read[T]()
                rec(ix + 1)
              } else array

            rec(0)
          } else r.overflow(s"Cannot deserialize Array with size $size (> Int.MaxValue)")
        } else Util.emptyArray[T]
      } else if (r.tryReadArrayStart()) {
        if (!r.tryReadBreak()) {
          r.readUntilBreak(mutable.ArrayBuilder.make[T])(_ += r.read[T]()).result()
        } else Util.emptyArray[T]
      } else r.unexpectedDataItem(expected = "Array")
    }

  implicit def forTreeMap[A: Ordering: Decoder, B: Decoder]: Decoder[TreeMap[A, B]] =
    constructForMap[A, B, TreeMap[A, B]](TreeMap.empty)

  implicit def forListMap[A: Decoder, B: Decoder]: Decoder[ListMap[A, B]] =
    constructForMap[A, B, ListMap[A, B]](ListMap.empty)

  implicit def forHashMap[A: Decoder, B: Decoder]: Decoder[HashMap[A, B]] =
    constructForMap[A, B, HashMap[A, B]](HashMap.empty)

  implicit def forEither[A: Decoder, B: Decoder]: Decoder[Either[A, B]] =
    Decoder { r =>
      if (r.tryReadArrayStart()) {
        if (r.readArrayStart().tryReadBreak()) {
          val x = r.readArrayStart().read[B]()
          r.readBreak().readBreak()
          Right(x)
        } else {
          val x = r.read[A]()
          r.readBreak().readArrayStart().readBreak().readBreak()
          Left(x)
        }
      } else {
        r.readMapHeader(1).readInt() match {
          case 0 => Left(r[A])
          case 1 => Right(r[B])
          case x =>
            r.unexpectedDataItem(
              expected = "Map entry with key 0 or 1 for decoding an `Either`",
              actual = s"Map entry with key $x")
        }
      }
    }
}

sealed abstract class LowPrioDecoders extends TupleDecoders {

  implicit final def forMap[A: Decoder, B: Decoder]: Decoder[Map[A, B]] =
    constructForMap[A, B, Map[A, B]](Map.empty)

  final def constructForMap[A: Decoder, B: Decoder, M <: Map[A, B]](empty: M): Decoder[M] =
    Decoder { r =>
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
