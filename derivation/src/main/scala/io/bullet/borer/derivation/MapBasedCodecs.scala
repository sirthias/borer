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
      val params = ctx.parametersArray
      val len    = params.length

      final class UnwrappedOptionEncoder(innerEncoder: Encoder[AnyRef]) extends Encoder[AnyRef] {
        def write(w: Writer, value: AnyRef) =
          value match {
            case None            ⇒ w
            case Some(x: AnyRef) ⇒ w.write(x)(innerEncoder)
          }
      }

      @tailrec def withOptionEncodersPatched(ix: Int, result: Array[Param[Encoder, T]]): Array[Param[Encoder, T]] =
        if (ix < len) {
          val p = result(ix).asInstanceOf[Param[Encoder, T] { type PType = AnyRef }]
          // if the param is an Option[T] with the pre-defined OptionEncoder and `None` as default value we switch to an
          // alternative Option encoding, which simply encodes the default value as "not-present" and Some as "present"
          val typeclass = p.typeclass
          val default   = p.default
          val newResult =
            if (typeclass.isInstanceOf[Encoder.OptionEncoder[_]] && default.isDefined && (default.get eq None)) {
              val innerEncoder   = typeclass.asInstanceOf[Encoder.OptionEncoder[AnyRef]].innerEncoder
              val patchedEncoder = new UnwrappedOptionEncoder(innerEncoder).asInstanceOf[Encoder[AnyRef]]
              val res            = if (result eq params) params.clone() else result
              res(ix) = p.withTypeclass(patchedEncoder)
              res
            } else result
          withOptionEncodersPatched(ix + 1, newResult)
        } else result

      val potentiallyPatchedParams = withOptionEncodersPatched(0, ctx.parametersArray)

      Encoder { (w, value) ⇒
        @tailrec def rec(effectiveParams: Array[Param[Encoder, T]], w: Writer, ix: Int): Writer =
          if (ix < len) {
            val p         = effectiveParams(ix).asInstanceOf[Param[Encoder, T] { type PType = AnyRef }]
            val pValue    = p.dereference(value)
            val typeclass = p.typeclass
            if ((pValue ne None) || !typeclass.isInstanceOf[UnwrappedOptionEncoder]) {
              typeclass.write(w.writeString(p.label), pValue)
            } // else the parameter is an undefined Option with `None` as the default value, so we can skip writing it
            rec(effectiveParams, w, ix + 1)
          } else w

        if (w.writingCbor) {
          val effectiveLen =
            if (potentiallyPatchedParams ne params) {
              // we might have undefined Option values with `None` as the default value,
              // so we first count the number of members we actually have to write
              @tailrec def count(ix: Int, result: Int): Int =
                if (ix < len) {
                  val p = potentiallyPatchedParams(ix).asInstanceOf[Param[Encoder, T] { type PType = AnyRef }]
                  val d =
                    if ((p.dereference(value) eq None) && p.typeclass.isInstanceOf[UnwrappedOptionEncoder]) 0 else 1
                  count(ix + 1, result + d)
                } else result
              count(0, 0)
            } else len
          rec(potentiallyPatchedParams, w.writeMapHeader(effectiveLen), 0)
        } else rec(potentiallyPatchedParams, w.writeMapStart(), 0).writeBreak()
      }
    }

    @inline def dispatch[T](ctx: SealedTrait[Encoder, T]): Encoder[T] =
      ArrayBasedCodecs.deriveEncoder.dispatch(ctx)

    def apply[T]: Encoder[T] = macro Magnolia.gen[T]
  }

  object deriveDecoder {
    type Typeclass[T] = Decoder[T]

    def combine[T](ctx: CaseClass[Decoder, T]): Decoder[T] = {
      @inline def typeName            = ctx.typeName.full
      @inline def expected(s: String) = s"$s decoding an instance of type [$typeName]"

      val len = ctx.parametersArray.length
      if (len > 128) sys.error(s"Cannot derive Decoder[$typeName]: More than 128 members are unsupported")

      @tailrec def withOptionDecodersPatched(ix: Int, result: Array[Param[Decoder, T]]): Array[Param[Decoder, T]] =
        if (ix < len) {
          val p = result(ix).asInstanceOf[Param[Decoder, T] { type PType = AnyRef }]
          // if the param is an Option[T] with the pre-defined OptionDecoder and `None` as default value we switch to an
          // alternative Option decoding, which simply decodes "not-present" as the default value and "present" as Some
          val typeclass = p.typeclass
          val default   = p.default
          val newResult =
            if (typeclass.isInstanceOf[Decoder.OptionDecoder[_]] && default.isDefined && (default.get eq None)) {
              val someDecoder = typeclass.asInstanceOf[Decoder.OptionDecoder[AnyRef]].someDecoder
              val res         = if (result eq ctx.parametersArray) ctx.parametersArray.clone() else result
              res(ix) = p.withTypeclass(someDecoder.asInstanceOf[Decoder[AnyRef]])
              res
            } else result
          withOptionDecodersPatched(ix + 1, newResult)
        } else result

      val params = withOptionDecodersPatched(0, ctx.parametersArray)

      Decoder { r ⇒
        val constructorArgs = new Array[Any](len)

        def failSizeOverflow() = r.overflow("Maps with size >= 2^63 are not supported")
        def failDuplicate(p: Param[Decoder, T]) =
          throw new Error.InvalidInputData(
            r.lastPosition,
            expected(s"Duplicate map key [${p.label}] encountered during"))

        @tailrec def fillArgsAndConstruct(filledCount: Int, remaining: Int, filledMask0: Long, filledMask1: Long): T = {

          @tailrec def findIndexOfNextArg(i: Int, end: Int): Int =
            if (i < end) {
              if (r.tryReadString(params(i).label)) i
              else findIndexOfNextArg(i + 1, end)
            } else if (end == len) findIndexOfNextArg(0, filledCount)
            else -1

          @tailrec def tryConstructWithMissingMembers(missingMask0: Long, missingMask1: Long): T = {
            import java.lang.Long.{numberOfTrailingZeros ⇒ ntz, lowestOneBit ⇒ lob}
            var i     = 0
            var mask0 = missingMask0
            var mask1 = missingMask1
            if (mask0 != 0L && { i = ntz(mask0); mask0 &= ~lob(mask0); true } ||
                mask1 != 0L && { i = 64 + ntz(mask1); mask1 &= ~lob(mask1); true }) {
              val p = params(i)
              p.default match {
                case Some(value) ⇒
                  constructorArgs(i) = value
                  tryConstructWithMissingMembers(mask0, mask1)
                case None ⇒
                  throw new Error.InvalidInputData(r.lastPosition, expected(s"Missing map key [${p.label}] for"))
              }
            } else ctx.rawConstruct(constructorArgs) // yay, we were able to fill all missing members w/ default values
          }

          @tailrec def skipExtraMembers(rem: Int): T =
            if ((rem >= 0 || !r.tryReadBreak()) && rem != 0) {
              @tailrec def verifyNoDuplicate(i: Int): Unit =
                if (i < len) {
                  val p = params(i)
                  if (r.tryReadString(p.label)) failDuplicate(p)
                  verifyNoDuplicate(i + 1)
                } else r.skipElement().skipElement() // ok, no duplicate, so skip this key/value pair
              verifyNoDuplicate(0)
              skipExtraMembers(rem - 1)
            } else ctx.rawConstruct(constructorArgs) // ok, we've skipped all extra members and didn't find duplicates

          var mask0       = filledMask0
          var mask1       = filledMask1
          val doneReading = remaining < 0 && r.tryReadBreak() || remaining == 0
          if (filledCount < len) {
            if (!doneReading) {
              // we're still missing members and there is more to read, so recurse
              val nextArgIx       = findIndexOfNextArg(filledCount, len)
              var nextFilledCount = filledCount
              if (nextArgIx >= 0) {
                val mask      = 1L << nextArgIx
                val p         = params(nextArgIx)
                var checkMask = 0L
                if (nextArgIx < 64) { checkMask = mask0; mask0 |= mask } else { checkMask = mask1; mask1 |= mask }
                if ((checkMask & mask) != 0) failDuplicate(p)
                constructorArgs(nextArgIx) = p.typeclass.read(r)
                nextFilledCount += 1
              } else r.skipElement().skipElement() // none of the params matches this key/value pair, so skip it
              if (remaining == Long.MinValue) failSizeOverflow()
              fillArgsAndConstruct(nextFilledCount, remaining - 1, mask0, mask1)
            } else {
              // we're still missing members but there is nothing more to read
              val xorMask = (1L << len) - 1
              if (len < 64) {
                mask0 = filledMask0 ^ xorMask
                mask1 = 0
              } else {
                mask0 = ~filledMask0
                mask1 = filledMask1 ^ xorMask
              }
              tryConstructWithMissingMembers(mask0, mask1)
            }
          } else if (doneReading) ctx.rawConstruct(constructorArgs)
          else skipExtraMembers(remaining)
        }

        if (r.tryReadMapStart()) fillArgsAndConstruct(0, -1, 0L, 0L)
        else if (r.hasMapHeader) {
          val mapLength = r.readMapHeader()
          if (mapLength > Int.MaxValue) failSizeOverflow()
          fillArgsAndConstruct(0, mapLength.toInt, 0L, 0L)
        } else r.unexpectedDataItem(expected(s"Map Start or Map Header announcing <= $len elements for"))
      }
    }

    @inline def dispatch[T](ctx: SealedTrait[Decoder, T]): Decoder[T] =
      ArrayBasedCodecs.deriveDecoder.dispatch(ctx)

    def apply[T]: Decoder[T] = macro Magnolia.gen[T]
  }

  def deriveCodec[T]: Codec[T] = macro Macros.deriveCodec[T]
}
