/*
 * Copyright (c) 2019-2024 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import io.bullet.borer.encodings.BaseEncoding
import io.bullet.borer.internal.Util

import java.lang.{
  Boolean as JBoolean,
  Byte as JByte,
  Double as JDouble,
  Float as JFloat,
  Long as JLong,
  Short as JShort
}
import java.math.{BigDecimal as JBigDecimal, BigInteger as JBigInteger}
import scala.annotation.{nowarn, tailrec, threadUnsafe}
import scala.collection.{mutable, Factory}
import scala.collection.immutable.{HashMap, ListMap, TreeMap}
import scala.deriving.Mirror
import scala.reflect.ClassTag

/**
 * Type class responsible for reading an instance of type [[T]] from a [[Reader]].
 */
trait Decoder[T]:
  def read(r: Reader): T

object Decoder extends LowPrioDecoders:
  import io.bullet.borer.DataItem as DI

  /**
   * A [[Decoder]] that might change its encoding strategy if [[T]] has a default value.
   */
  trait DefaultValueAware[T] extends Decoder[T]:
    def withDefaultValue(defaultValue: T): Decoder[T]

  /**
   * A [[Decoder]] that lazily wraps another [[Decoder]].
   * Useful, for example, for recursive definitions.
   */
  trait Lazy[T] extends Decoder[T]:
    def delegate: Decoder[T]

  /**
   * Creates a [[Decoder]] from the given function.
   */
  inline def apply[T](decoder: Decoder[T]): Decoder[T] = decoder

  /**
   * Gets a given [[Decoder]] for [[T]].
   */
  inline def of[T: Decoder]: Decoder[T] = summon

  /**
   * Creates a [[Decoder]] that decodes a product instance from a simple array of values.
   * Used, for example, as the default 'given' decoder for tuples.
   */
  inline def forProduct[T <: Product: Mirror.ProductOf]: Decoder[T] =
    io.bullet.borer.internal.BasicProductCodec.decoder[T]

  /**
   * Creates a "unified" [[Decoder]] from two decoders that each target only a single data format.
   */
  def targetSpecific[T](cbor: Decoder[T], json: Decoder[T]): Decoder[T] = { r =>
    if (r.target == Cbor) cbor.read(r) else json.read(r)
  }

  extension [A](underlying: Decoder[A])

    /**
     * Maps the result of the underlying [[Decoder]] with the given function.
     * The function can throw exceptions to terminate the decoding process with an error.
     * If the thrown exception is not a [[Borer.Error]] itself it will be
     * wrapped in a [[Borer.Error.General]] instance.
     */
    def map[B](f: A => B): Decoder[B] = Decoder(r => f(underlying.read(r)))

    /**
     * Maps the result of the underlying [[Decoder]] with the given function.
     * Since the function has access to the [[Reader]] instance it can call its
     * helper methods `validationFailure` or `overflow` (among other things) to signal errors.
     */
    def mapWithReader[B](f: (Reader, A) => B): Decoder[B] = Decoder(r => f(r, underlying.read(r)))

    /**
     * Maps the result of the underlying [[Decoder]] with the given function.
     * If the function returns `None` decoding will fail with a [[Borer.Error.ValidationFailure]].
     */
    @nowarn("msg=anonymous class definition will be duplicated at each inline site")
    inline def mapOption[B: Mirror.Of](f: A => Option[B]): Decoder[B] =
      Decoder(r => f(underlying.read(r)).getOrElse(r.unexpectedDataItem(Util.typeName[B])))

    /**
     * Maps the result of the underlying [[Decoder]] with the given function.
     * [[Left]] results will terminate the encoding process with an error.
     * If the [[Throwable]] is not a [[Borer.Error]] itself it will be
     * wrapped in a [[Borer.Error.General]] instance.
     */
    def mapEither[E: DecodingError, B](f: A => Either[E, B]): Decoder[B] =
      Decoder { r =>
        f(underlying.read(r)) match
          case Left(e)  => DecodingError.raise(e, r)
          case Right(b) => b
      }

    /**
     * Changes the default value of the [[Decoder]] to a new value.
     * If the underlying [[Decoder]] is not [[Decoder.DefaultValueAware]]
     * this method has no effect.
     */
    def withDefaultValue(defaultValue: A): Decoder[A] =
      underlying.unwrap match
        case x: Decoder.DefaultValueAware[A] => x withDefaultValue defaultValue
        case x                               => x

    def unwrap: Decoder[A] =
      underlying match
        case x: Lazy[A] => x.delegate.unwrap
        case x          => x

  extension [T](underlying: => Decoder[T])
    /**
     * Wraps a [[Decoder]] definition with lazy initialization.
     */
    def recursive: Decoder[T] =
      new Lazy[T] {
        @threadUnsafe lazy val delegate: Decoder[T] = underlying
        def read(r: Reader): T                      = delegate.read(r)
      }

  /**
   * Type class for turning arbitrary errors into decoding failures.
   */
  trait DecodingError[-E]:
    def raise(error: E, reader: Reader): Nothing

  object DecodingError:

    def raise[E](error: E, reader: Reader)(using ev: DecodingError[E]): Nothing =
      ev.raise(error, reader)

    given DecodingError[Throwable] with
      def raise(error: Throwable, reader: Reader): Nothing = throw error

    given DecodingError[String] with
      def raise(error: String, reader: Reader): Nothing = reader.validationFailure(error)

  given [T](using codec: Codec[T]): Decoder[T]  = codec.decoder
  given [T](using ev: Codec.All[T]): Decoder[T] = ev.delegate.decoder

  /**
   * Helper type serving only as the target of a `derives Encoder.All` clause.
   * The `borer-derivation` module can then provide the respective `derived` method on the companion object.
   */
  final class All[A](delegate: Decoder[A]) extends Decoder[A]:
    inline def read(r: Reader): A = delegate.read(r)

  object All

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  given forNull: Decoder[Null]       = Decoder(_.readNull())
  given forBoolean: Decoder[Boolean] = Decoder(_.readBoolean())
  given forInt: Decoder[Int]         = Decoder(_.readInt())
  given forLong: Decoder[Long]       = Decoder(_.readLong())
  given forFloat: Decoder[Float]     = Decoder(_.readFloat())
  given forDouble: Decoder[Double]   = Decoder(_.readDouble())
  given forString: Decoder[String]   = Decoder(_.readString())

  given forUnit: Decoder[Unit] = Decoder { r =>
    if (r.readInt() != 0) r.unexpectedDataItem(expected = "integer value zero")
  }

  given forByteArrayDefault: Decoder[Array[Byte]] = forByteArray(BaseEncoding.base64)

  def forByteArray(jsonBaseEncoding: BaseEncoding): Decoder[Array[Byte]] =
    Decoder { r =>
      if (r.readingCbor)
        if (r.hasByteArray) r.readByteArray()
        else if (r.hasArrayHeader)
          val size = r.readArrayHeader()
          if (size > 0)
            if (size <= Int.MaxValue)
              val intSize = size.toInt
              val array   = new Array[Byte](intSize)

              @tailrec def rec(ix: Int): Array[Byte] =
                if (ix < intSize)
                  array(ix) = r.readByte()
                  rec(ix + 1)
                else array

              rec(0)
            else r.overflow(s"Cannot deserialize ByteArray with size $size (> Int.MaxValue)")
          else Array.emptyByteArray
        else if (r.tryReadArrayStart())
          if (!r.tryReadBreak())
            r.readUntilBreak(new mutable.ArrayBuilder.ofByte)(_ += r.readByte()).result()
          else Array.emptyByteArray
        else r.unexpectedDataItem(expected = "ByteString or Array of bytes")
      else jsonBaseEncoding.decode(r.readChars())
    }

  given forChar: Decoder[Char] = forChar(forInt)

  def forChar(intDecoder: Decoder[Int]): Decoder[Char] =
    intDecoder.mapWithReader { (r, int) =>
      if ((int >> 16) != 0) r.validationFailure(s"Cannot convert int value $int to Char")
      int.toChar
    }

  given forByte: Decoder[Byte] = forByte(forInt)

  def forByte(intDecoder: Decoder[Int]): Decoder[Byte] =
    intDecoder.mapWithReader { (r, int) =>
      if ((int >> 8) != (int >> 31)) r.validationFailure(s"Cannot convert int value $int to Byte")
      int.toByte
    }

  given forShort: Decoder[Short] = forShort(forInt)

  def forShort(intDecoder: Decoder[Int]): Decoder[Short] =
    intDecoder.mapWithReader { (r, int) =>
      if ((int >> 16) != (int >> 31)) r.validationFailure(s"Cannot convert int value $int to Short")
      int.toShort
    }

  given forBoxedBoolean: Decoder[JBoolean] = forBoolean.asInstanceOf[Decoder[JBoolean]]
  given forBoxedChar: Decoder[Character]   = forChar.asInstanceOf[Decoder[Character]]
  given forBoxedByte: Decoder[JByte]       = forByte.asInstanceOf[Decoder[JByte]]
  given forBoxedShort: Decoder[JShort]     = forShort.asInstanceOf[Decoder[JShort]]
  given forBoxedInt: Decoder[Integer]      = forInt.asInstanceOf[Decoder[Integer]]
  given forBoxedLong: Decoder[JLong]       = forLong.asInstanceOf[Decoder[JLong]]
  given forBoxedFloat: Decoder[JFloat]     = forFloat.asInstanceOf[Decoder[JFloat]]
  given forBoxedDouble: Decoder[JDouble]   = forDouble.asInstanceOf[Decoder[JDouble]]

  def forJBigInteger(
      maxCborByteArraySize: Int = 64,
      maxJsonNumberStringLength: Int = 64,
      acceptStrings: Boolean = false): Decoder[JBigInteger] =
    Decoder { r =>
      def fromByteArray() = {
        val byteArray = r.readByteArray()
        if (byteArray.length > maxCborByteArraySize) {
          r.overflow(
            "ByteArray for decoding JBigInteger is longer than the configured max of " + maxCborByteArraySize + " bytes")
        } else new JBigInteger(1, byteArray)
      }
      def fromString(numberString: String) =
        if (numberString.length > maxJsonNumberStringLength) {
          r.overflow(
            "NumberString for decoding JBigInteger is longer than the configured max of " + maxJsonNumberStringLength + " characters")
        } else new JBigInteger(numberString)
      r.dataItem() match {
        case DI.Int | DI.Long => JBigInteger.valueOf(r.readLong())
        case DI.OverLong =>
          def value = new JBigInteger(1, Util.toBigEndianBytes(r.readOverLong()))
          if (r.overLongNegative) value.not else value
        case DI.NumberString if r.target == Json                   => fromString(r.readNumberString())
        case _ if r.hasString && r.target == Json && acceptStrings => fromString(r.readString())
        case _ if r.tryReadTag(Tag.PositiveBigNum)                 => fromByteArray()
        case _ if r.tryReadTag(Tag.NegativeBigNum)                 => fromByteArray().not
        case _                                                     => r.unexpectedDataItem(expected = "BigInteger")
      }
    }

  given _forJBigInteger: Decoder[JBigInteger] = forJBigInteger()

  given forBigInt: Decoder[BigInt] = _forJBigInteger.map(BigInt(_))

  def forJBigDecimal(
      maxCborBigIntMantissaByteArraySize: Int = 64,
      maxCborAbsExponent: Int = 999,
      maxJsonNumberStringLength: Int = 64,
      acceptStrings: Boolean = false): Decoder[JBigDecimal] =
    val bigIntMantissaDecoder = forJBigInteger(maxCborByteArraySize = maxCborBigIntMantissaByteArraySize)
    Decoder { r =>
      def fromBigInteger() = new JBigDecimal(_forJBigInteger.read(r))
      def fromString(numberString: String) =
        if (numberString.length > maxJsonNumberStringLength) {
          r.overflow(
            "NumberString for decoding JBigDecimal is longer than the configured max of " + maxJsonNumberStringLength + " characters")
        } else new JBigDecimal(numberString)
      r.dataItem() match {
        case DI.Int | DI.Long | DI.OverLong                                   => fromBigInteger()
        case DI.Double                                                        => JBigDecimal.valueOf(r.readDouble())
        case DI.NumberString if r.target == Json                              => fromString(r.readNumberString())
        case _ if r.hasString && r.target == Json && acceptStrings            => fromString(r.readString())
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

  given _forJBigDecimal: Decoder[JBigDecimal] = forJBigDecimal()

  given forBigDecimal: Decoder[BigDecimal] = _forJBigDecimal.map(BigDecimal(_))

  given forOption[T: Decoder]: Decoder.DefaultValueAware[Option[T]] =
    new Decoder.DefaultValueAware[Option[T]] {

      def read(r: Reader) =
        if (r.hasArrayHeader)
          r.readArrayHeader() match
            case 0 => None
            case 1 => Some(r.read[T]())
            case x => r.unexpectedDataItem("Array with length 0 or 1 for decoding an `Option`", s"Array with length $x")
        else if (r.tryReadArrayStart())
          if (r.tryReadBreak()) None
          else
            val x = r.read[T]()
            if (r.tryReadBreak()) Some(x)
            else
              r.unexpectedDataItem(
                "Array with length 0 or 1 for decoding an `Option`",
                "Array with more than one element")
        else r.unexpectedDataItem("Array with length 0 or 1 for decoding an `Option`")

      def withDefaultValue(defaultValue: Option[T]): Decoder[Option[T]] =
        if (defaultValue ne None) this
        else Decoder[Option[T]](r => Some(r.read[T]()))
    }

  given fromFactory[T: Decoder, M[_]](using factory: Factory[T, M[T]]): Decoder[M[T]] =
    Decoder { r =>
      if (r.hasArrayHeader)
        @tailrec def rec(remaining: Int, b: mutable.Builder[T, M[T]]): M[T] =
          if (remaining > 0) rec(remaining - 1, b += r[T]) else b.result()
        val size = r.readArrayHeader()
        if (size <= Int.MaxValue)
          val intSize = size.toInt
          val builder = factory.newBuilder
          builder.sizeHint(intSize)
          rec(intSize, builder)
        else r.overflow(s"Cannot deserialize Iterable with size $size (> Int.MaxValue)")
      else if (r.tryReadArrayStart())
        r.readUntilBreak[M, T]()
      else r.unexpectedDataItem(expected = "Array for deserializing an Iterable instance")
    }

  given forArray[T: ClassTag: Decoder]: Decoder[Array[T]] =
    Decoder { r =>
      if (r.hasArrayHeader)
        val size = r.readArrayHeader()
        if (size > 0)
          if (size <= Int.MaxValue)
            val intSize = size.toInt
            val array   = Array.ofDim[T](intSize)

            @tailrec def rec(ix: Int): Array[T] =
              if (ix < intSize)
                array(ix) = r.read[T]()
                rec(ix + 1)
              else array

            rec(0)
          else r.overflow(s"Cannot deserialize Array with size $size (> Int.MaxValue)")
        else Util.emptyArray[T]
      else if (r.tryReadArrayStart())
        if (!r.tryReadBreak())
          r.readUntilBreak(mutable.ArrayBuilder.make[T])(_ += r.read[T]()).result()
        else Util.emptyArray[T]
      else r.unexpectedDataItem(expected = "Array")
    }

  given forTreeMap[A: Ordering: Decoder, B: Decoder]: Decoder[TreeMap[A, B]] =
    constructForMap[A, B, TreeMap[A, B]](TreeMap.empty)

  given forListMap[A: Decoder, B: Decoder]: Decoder[ListMap[A, B]] =
    constructForMap[A, B, ListMap[A, B]](ListMap.empty)

  given forHashMap[A: Decoder, B: Decoder]: Decoder[HashMap[A, B]] =
    constructForMap[A, B, HashMap[A, B]](HashMap.empty)

  inline given forTuple[T <: Tuple: Mirror.ProductOf]: Decoder[T] = Decoder.forProduct[T]

  /**
   * The default [[Decoder]] for [[Either]] is not automatically in scope,
   * because there is no clear "standard" way of encoding instances of [[Either]].
   */
  object ForEither:

    given default[A: Decoder, B: Decoder]: Decoder[Either[A, B]] =
      Decoder { r =>
        val breakExpected = r.tryReadArrayStart() || { r.readMapHeader(1); false }
        val result =
          r.readInt() match
            case 0 => Left(r.read[A]())
            case 1 => Right(r.read[B]())
            case x => r.unexpectedDataItem(expected = "Int 0 or 1 for decoding an `Either`", actual = s"Int $x")
        if (breakExpected) r.readBreak()
        result
      }

  object StringNumbers:
    given intDecoder: Decoder[Int]     = Decoder(r => if (r.hasString) r.readString().toInt else r.readInt())
    given longDecoder: Decoder[Long]   = Decoder(r => if (r.hasString) r.readString().toLong else r.readLong())
    given floatDecoder: Decoder[Float] = Decoder(r => if (r.hasString) r.readString().toFloat else r.readFloat())

    given doubleDecoder: Decoder[Double] = Decoder(r => if (r.hasString) r.readString().toDouble else r.readDouble())
    given charDecoder: Decoder[Char]     = Decoder.forChar(intDecoder)
    given byteDecoder: Decoder[Byte]     = Decoder.forByte(intDecoder)
    given shortDecoder: Decoder[Short]   = Decoder.forShort(intDecoder)

    given boxedCharDecoder: Decoder[Character] = charDecoder.asInstanceOf[Decoder[Character]]
    given boxedByteDecoder: Decoder[JByte]     = byteDecoder.asInstanceOf[Decoder[JByte]]
    given boxedShortDecoder: Decoder[JShort]   = shortDecoder.asInstanceOf[Decoder[JShort]]
    given boxedIntDecoder: Decoder[Integer]    = intDecoder.asInstanceOf[Decoder[Integer]]
    given boxedLongDecoder: Decoder[JLong]     = longDecoder.asInstanceOf[Decoder[JLong]]
    given boxedFloatDecoder: Decoder[JFloat]   = floatDecoder.asInstanceOf[Decoder[JFloat]]
    given boxedDoubleDecoder: Decoder[JDouble] = doubleDecoder.asInstanceOf[Decoder[JDouble]]

    given forJBigInteger: Decoder[JBigInteger] = Decoder.this.forJBigInteger(acceptStrings = true)
    given forBigInt: Decoder[BigInt]           = forJBigInteger.map(BigInt(_))
    given forJBigDecimal: Decoder[JBigDecimal] = Decoder.this.forJBigDecimal(acceptStrings = true)
    given forBigDecimal: Decoder[BigDecimal]   = forJBigDecimal.map(BigDecimal(_))

  object StringBooleans:

    given booleanDecoder: Decoder[Boolean] = Decoder { r =>
      if (r.hasString)
        r.readString().toLowerCase match
          case "true" | "yes" | "on"  => true
          case "false" | "no" | "off" => false
          case _                      => r.readBoolean()
      else r.readBoolean()
    }

    given boxedBooleanDecoder: Decoder[JBoolean] = booleanDecoder.asInstanceOf[Decoder[JBoolean]]

  object StringNulls:

    given nullDecoder: Decoder[Null] = Decoder { r =>
      r.readString("null")
      null
    }

sealed abstract class LowPrioDecoders:

  given forMap[A: Decoder, B: Decoder]: Decoder[Map[A, B]] =
    constructForMap[A, B, Map[A, B]](Map.empty)

  final def constructForMap[A: Decoder, B: Decoder, M <: Map[A, B]](empty: M): Decoder[M] =
    Decoder { r =>
      if (r.hasMapHeader)
        @tailrec def rec(remaining: Int, map: Map[A, B]): M =
          if (remaining > 0) rec(remaining - 1, map.updated(r[A], r[B])) else map.asInstanceOf[M]
        val size = r.readMapHeader()
        if (size <= Int.MaxValue) rec(size.toInt, empty)
        else r.overflow(s"Cannot deserialize Map with size $size (> Int.MaxValue)")
      else if (r.hasMapStart)
        r.readMapStart()
        @tailrec def rec(map: Map[A, B]): M =
          if (r.tryReadBreak()) map.asInstanceOf[M] else rec(map.updated(r[A], r[B]))
        rec(empty)
      else r.unexpectedDataItem(expected = "Map")
    }

/**
 * An [[AdtDecoder]] is a [[Decoder]] whose `read` method expects to read an envelope
 * (holding the type id) around the actual value encoding.
 *
 * In order to be able to collapse several envelope levels into a single one, when several [[AdtDecoder]] instances
 * call each other, this type also provides `read` overloads which don't read the type id envelope themselves
 * but can receive the type id from the outside.
 */
trait AdtDecoder[T] extends Decoder[T]:

  def read(r: Reader, typeId: Long): T

  def read(r: Reader, typeId: String): T
