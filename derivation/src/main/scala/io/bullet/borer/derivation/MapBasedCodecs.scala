/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.derivation

import io.bullet.borer._
import io.bullet.borer.Borer.Error
import io.bullet.borer.magnolia._

import scala.annotation.tailrec

object MapBasedCodecs {

  object deriveEncoder {
    type Typeclass[T] = Encoder[T]

    def combine[T](ctx: CaseClass[Encoder, T]): Encoder[T] = {
      val params    = ctx.parametersArray
      val len       = params.length
      val paramKeys = new Array[key.Value](len)

      @tailrec def withEncodersPatched(ix: Int, result: Array[Param[Encoder, T]]): Array[Param[Encoder, T]] =
        if (ix < len) {
          val p         = result(ix).asInstanceOf[Param[Encoder, T] { type PType = AnyRef }]
          val typeclass = p.typeclass
          val default   = p.default
          var newResult = result
          if (typeclass.isInstanceOf[Encoder.DefaultValueAware[_]] && default.isDefined) {
            val dva = typeclass.asInstanceOf[Encoder.DefaultValueAware[AnyRef]]
            val wdv = dva withDefaultValue default.get
            if (wdv ne dva) {
              if (result eq params) newResult = params.clone()
              newResult(ix) = p withTypeclass wdv
            }
          }
          paramKeys(ix) = key.find(p.annotationsArray, p.label)
          withEncodersPatched(ix + 1, newResult)
        } else result

      val potentiallyPatchedParams = withEncodersPatched(0, ctx.parametersArray)

      Encoder { (w, value) =>
        @tailrec def rec(effectiveParams: Array[Param[Encoder, T]], w: Writer, ix: Int): Writer =
          if (ix < len) {
            val p      = effectiveParams(ix).asInstanceOf[Param[Encoder, T] { type PType = AnyRef }]
            val pValue = p.dereference(value)
            p.typeclass match {
              case x: Encoder.PossiblyWithoutOutput[AnyRef] if !x.producesOutput(pValue) => // skip
              case x =>
                x.write(key.Value.write(w, paramKeys(ix)), pValue)
            }
            rec(effectiveParams, w, ix + 1)
          } else w

        if (w.writingCbor) {
          val effectiveLen =
            if (potentiallyPatchedParams ne params) {
              // we might have Encoder.PossiblyWithoutOutput instances that are not actually writing anything,
              // so we need to count the number of elements that actually going to be written beforehand
              @tailrec def count(ix: Int, result: Int): Int =
                if (ix < len) {
                  val p = potentiallyPatchedParams(ix).asInstanceOf[Param[Encoder, T] { type PType = AnyRef }]
                  val d = p.typeclass match {
                    case x: Encoder.PossiblyWithoutOutput[AnyRef] if !x.producesOutput(p dereference value) => 0
                    case _                                                                                  => 1
                  }
                  count(ix + 1, result + d)
                } else result
              count(0, 0)
            } else len
          rec(potentiallyPatchedParams, w.writeMapHeader(effectiveLen), 0)
        } else rec(potentiallyPatchedParams, w.writeMapStart(), 0).writeBreak()
      }
    }

    def dispatch[T](ctx: SealedTrait[Encoder, T]): Encoder[T] = {
      val subtypes = ctx.subtypesArray
      val len      = subtypes.length
      val typeIds  = key.getTypeIds(ctx.typeName.full, subtypes)
      Encoder { (w, value) =>
        @tailrec def rec(ix: Int): Writer =
          if (ix < len) {
            val sub = subtypes(ix)
            if (sub.cast isDefinedAt value) {
              def writeEntry(w: Writer) = key.Value.write(w, typeIds(ix)).write(sub.cast(value))(sub.typeclass)
              if (w.writingCbor) writeEntry(w.writeMapHeader(1))
              else writeEntry(w.writeMapStart()).writeBreak()
            } else rec(ix + 1)
          } else throw new IllegalArgumentException(s"The given value `$value` is not a sub type of `${ctx.typeName}`")
        rec(0)
      }
    }

    def apply[T]: Encoder[T] = macro Magnolia.gen[T]
  }

  object deriveDecoder {
    type Typeclass[T] = Decoder[T]

    def combine[T](ctx: CaseClass[Decoder, T]): Decoder[T] = {
      @inline def typeName            = ctx.typeName.full
      @inline def expected(s: String) = s"$s decoding an instance of type `$typeName`"

      val len = ctx.parametersArray.length
      if (len > 128) sys.error(s"Cannot derive Decoder[$typeName]: More than 128 members are unsupported")
      val paramKeys = new Array[key.Value](len)

      @tailrec def withDecodersPatched(ix: Int, result: Array[Param[Decoder, T]]): Array[Param[Decoder, T]] =
        if (ix < len) {
          val p         = result(ix).asInstanceOf[Param[Decoder, T] { type PType = AnyRef }]
          val typeclass = p.typeclass
          val default   = p.default
          var newResult = result
          if (typeclass.isInstanceOf[Decoder.DefaultValueAware[_]] && default.isDefined) {
            val dva = typeclass.asInstanceOf[Decoder.DefaultValueAware[AnyRef]]
            val wdv = dva withDefaultValue default.get
            if (wdv ne dva) {
              if (result eq ctx.parametersArray) newResult = ctx.parametersArray.clone()
              newResult(ix) = p.withTypeclass(wdv)
            }
          }
          paramKeys(ix) = key.find(p.annotationsArray, p.label)
          withDecodersPatched(ix + 1, newResult)
        } else result

      val params = withDecodersPatched(0, ctx.parametersArray)

      Decoder { r =>
        val constructorArgs = new Array[Any](len)

        def failSizeOverflow() = r.overflow("Maps with size >= 2^63 are not supported")
        def failDuplicate(k: key.Value) =
          throw new Error.InvalidInputData(
            r.lastPosition,
            expected(s"Duplicate map key `${k.value}` encountered during"))

        @tailrec def fillArgsAndConstruct(filledCount: Int, remaining: Int, filledMask0: Long, filledMask1: Long): T = {

          @tailrec def fillMissingMembers(missingMask0: Long, missingMask1: Long): Boolean = {
            import java.lang.Long.{lowestOneBit => lob, numberOfTrailingZeros => ntz}
            var i     = 0
            var mask0 = missingMask0
            var mask1 = missingMask1
            (mask0 != 0L && { i = ntz(mask0); mask0 &= ~lob(mask0); true } ||
            mask1 != 0L && { i = 64 + ntz(mask1); mask1 &= ~lob(mask1); true }) && {
              params(i).default match {
                case Some(value) =>
                  constructorArgs(i) = value
                  fillMissingMembers(mask0, mask1)
                case None =>
                  throw new Error.InvalidInputData(
                    r.lastPosition,
                    expected(s"Missing map key `${paramKeys(i).value}` for"))
              }
            } // else we were able to fill all missing members w/ default values
          }

          def membersMissing(): Boolean = {
            val xorMask = (1L << len) - 1
            var mask0   = filledMask0
            var mask1   = filledMask1
            if (len < 64) {
              mask0 ^= xorMask
              mask1 = 0
            } else {
              mask0 = ~mask0
              mask1 ^= xorMask
            }
            fillMissingMembers(mask0, mask1)
          }

          @tailrec def skipExtraMembers(rem: Int): Boolean =
            (rem >= 0 || !r.tryReadBreak()) && rem != 0 && {
              @tailrec def verifyNoDuplicate(i: Int): Unit =
                if (i < len) {
                  key.tryRead(r, paramKeys, 0) match {
                    case -1 => verifyNoDuplicate(i + 1)
                    case ix => failDuplicate(paramKeys(ix))
                  }
                } else r.skipElement().skipElement() // ok, no duplicate, so skip this key/value pair
              verifyNoDuplicate(0)
              skipExtraMembers(rem - 1)
            } // else we've skipped all extra members and didn't find duplicates

          val moreToRead = (remaining >= 0 || !r.tryReadBreak()) && remaining != 0
          if ((filledCount < len || moreToRead && skipExtraMembers(remaining)) && (moreToRead || membersMissing())) {
            // we're still missing members and there is more to read, so recurse
            var mask0           = filledMask0
            var mask1           = filledMask1
            val nextArgIx       = key.tryRead(r, paramKeys, filledCount)
            var nextFilledCount = filledCount
            if (nextArgIx >= 0) {
              val mask      = 1L << nextArgIx
              val p         = params(nextArgIx)
              var checkMask = 0L
              if (nextArgIx < 64) {
                checkMask = mask0; mask0 |= mask
              } else {
                checkMask = mask1; mask1 |= mask
              }
              if ((checkMask & mask) != 0) failDuplicate(paramKeys(nextArgIx))
              constructorArgs(nextArgIx) = p.typeclass.read(r)
              nextFilledCount += 1
            } else r.skipElement().skipElement() // none of the params matches this key/value pair, so skip it
            if (remaining == Long.MinValue) failSizeOverflow()
            fillArgsAndConstruct(nextFilledCount, remaining - 1, mask0, mask1)
          } else ctx.rawConstruct(constructorArgs)
        }

        if (r.tryReadMapStart()) fillArgsAndConstruct(0, -1, 0L, 0L)
        else if (r.hasMapHeader) {
          val mapLength = r.readMapHeader()
          if (mapLength > Int.MaxValue) failSizeOverflow()
          fillArgsAndConstruct(0, mapLength.toInt, 0L, 0L)
        } else r.unexpectedDataItem(expected(s"Map Start or Map Header announcing <= $len elements for"))
      }
    }

    def dispatch[T](ctx: SealedTrait[Decoder, T]): Decoder[T] = {
      val subtypes            = ctx.subtypesArray
      val typeIds             = key.getTypeIds(ctx.typeName.full, subtypes)
      def expected(s: String) = s"$s for decoding an instance of type `${ctx.typeName.full}`"

      Decoder { r =>
        def readTypeIdAndValue(): T =
          key.tryRead(r, typeIds, 0) match {
            case -1 => r.unexpectedDataItem(s"Any type id key of [${typeIds.map(_.value).mkString(", ")}]")
            case ix => subtypes(ix).typeclass.read(r)
          }

        if (r.tryReadMapStart()) {
          val result = readTypeIdAndValue()
          if (r.tryReadBreak()) result
          else r.unexpectedDataItem(expected("Single-entry Map"), "at least one extra element")
        } else if (r.tryReadMapHeader(1)) {
          readTypeIdAndValue()
        } else r.unexpectedDataItem(expected("Single-entry Map"))
      }
    }

    def apply[T]: Decoder[T] = macro Magnolia.gen[T]
  }

  def deriveCodec[T]: Codec[T] = macro Macros.deriveCodec[T]
}
