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
          // if the param is an Option[T] with the pre-defined OptionEncoder and a default value we switch to an
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
      if (len > 64) sys.error(s"Cannot derive Decoder[$typeName]: More than 64 members are unsupported")

      @tailrec def withOptionDecodersPatched(ix: Int, result: Array[Param[Decoder, T]]): Array[Param[Decoder, T]] =
        if (ix < len) {
          val p = result(ix)
          // if the param is an Option[T] with the pre-defined OptionDecoder and a default value we switch to an
          // alternative Option decoding, which simply decodes "not-present" as the default value and "present" as Some
          val typeclass = p.typeclass
          val newResult =
            if (typeclass.isInstanceOf[Decoder.OptionDecoder[_]] && p.default.isDefined) {
              val someDecoder = typeclass.asInstanceOf[Decoder.OptionDecoder[_]].someDecoder
              val res         = if (result eq ctx.parametersArray) ctx.parametersArray.clone() else result
              res(ix) = p.withTypeclass(someDecoder.asInstanceOf[Decoder[p.PType]])
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

        @tailrec def fillArgsAndConstruct(remaining: Int, filledMask: Long): T = {
          val alreadyFilledCount = java.lang.Long.bitCount(filledMask)

          @tailrec def findAndFillNextArg(i: Int, end: Int): Long =
            if (i < end) {
              val p = params(i)
              if (r.tryReadString(p.label)) {
                val mask = 1L << i
                if ((filledMask & mask) != 0) failDuplicate(p)
                constructorArgs(i) = p.typeclass.read(r)
                filledMask | mask
              } else findAndFillNextArg(i + 1, end)
            } else if (end == len) findAndFillNextArg(0, alreadyFilledCount)
            else {
              // none of the params matches this key/value pair, so skip it
              r.skipDataItem().skipDataItem()
              filledMask
            }

          @tailrec def tryConstructWithMissingMembers(missingMask: Long): T =
            if (missingMask != 0L) {
              val i     = java.lang.Long.numberOfTrailingZeros(missingMask)
              val p     = params(i)
              val iMask = ~java.lang.Long.lowestOneBit(missingMask)
              p.default match {
                case Some(value) ⇒
                  constructorArgs(i) = value
                  tryConstructWithMissingMembers(missingMask & iMask)
                case None ⇒
                  throw new Error.InvalidInputData(r.lastPosition, expected(s"Missing map key [${p.label}] for"))
              }
            } else ctx.rawConstruct(constructorArgs) // yay, we were able to fill all missing members w/ default values

          @tailrec def skipSurplusMembers(rem: Int): T =
            if ((rem >= 0 || !r.tryReadBreak()) && rem != 0) {
              @tailrec def verifyNoDuplicate(i: Int): Unit =
                if (i < len) {
                  val p = params(i)
                  if (r.tryReadString(p.label)) failDuplicate(p)
                  verifyNoDuplicate(i + 1)
                } else r.skipDataItem().skipDataItem() // ok, no duplicate, so skip this key/value pair
              verifyNoDuplicate(0)
              skipSurplusMembers(rem - 1)
            } else ctx.rawConstruct(constructorArgs) // ok, we've skipped all surplus members and didn't find duplicates

          val doneReading = remaining < 0 && r.tryReadBreak() || remaining == 0
          if (alreadyFilledCount < len) {
            if (!doneReading) { // we're still missing members and there is more to read, so recurse
              val newFilledMask = findAndFillNextArg(alreadyFilledCount, len)
              if (remaining == Long.MinValue) failSizeOverflow()
              fillArgsAndConstruct(remaining - 1, newFilledMask)
            } else tryConstructWithMissingMembers(filledMask ^ (1L << len) - 1)
          } else if (doneReading) ctx.rawConstruct(constructorArgs)
          else skipSurplusMembers(remaining)
        }

        if (r.tryReadMapStart()) fillArgsAndConstruct(-1, 0L)
        else if (r.hasMapHeader) {
          val mapLength = r.readMapHeader()
          if (mapLength > Int.MaxValue) failSizeOverflow()
          fillArgsAndConstruct(mapLength.toInt, 0L)
        } else r.unexpectedDataItem(expected(s"Map Start or Map Header announcing <= $len elements for"))
      }
    }

    @inline def dispatch[T](ctx: SealedTrait[Decoder, T]): Decoder[T] =
      ArrayBasedCodecs.deriveDecoder.dispatch(ctx)

    def apply[T]: Decoder[T] = macro Magnolia.gen[T]
  }

  def deriveCodec[T]: Codec[T] = macro Macros.deriveCodec[T]
}
