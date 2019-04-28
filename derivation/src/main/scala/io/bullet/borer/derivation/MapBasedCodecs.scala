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
import magnolia._

import scala.annotation.tailrec

object MapBasedCodecs {

  object deriveEncoder {
    type Typeclass[T] = Encoder[T]

    def combine[T](ctx: CaseClass[Encoder, T]): Encoder[T] = {
      val params = ctx.parameters
      val len    = params.size
      Encoder { (w, value) ⇒
        @tailrec def rec(w: Writer, ix: Int): Writer =
          if (ix < len) {
            val p = params(ix)
            w.writeString(p.label)
            rec(p.typeclass.write(w, p.dereference(value)), ix + 1)
          } else w

        if (w.writingJson) rec(w.writeMapStart(), 0).writeBreak()
        else rec(w.writeMapHeader(len), 0)
      }
    }

    @inline def dispatch[T](ctx: SealedTrait[Encoder, T]): Encoder[T] =
      ArrayBasedCodecs.deriveEncoder.dispatch(ctx)

    def apply[T]: Encoder[T] = macro Magnolia.gen[T]
  }

  object deriveDecoder {
    type Typeclass[T] = Decoder[T]

    def combine[T](ctx: CaseClass[Decoder, T]): Decoder[T] = {
      @inline def typeName = ctx.typeName.full
      val params           = ctx.parameters
      val len              = params.size
      if (len > 64) sys.error(s"Cannot derive Decoder[$typeName]: More than 64 members are unsupported")
      def expected(s: String) = s"$s decoding an instance of type [$typeName]"

      Decoder { r ⇒
        val constructorArgs = new Array[AnyRef](len)

        def failSizeOverflow() = r.overflow("Maps with size >= 2^63 are not supported")
        def failDuplicate(p: Param[Decoder, T]) =
          throw new Error.InvalidInputData(r.position, expected(s"Duplicate map key [${p.label}] encountered during"))

        @tailrec def fillArgsAndConstruct(remaining: Int, filledMask: Long): T = {
          val alreadyFilledCount = java.lang.Long.bitCount(filledMask)

          @tailrec def findAndFillNextArg(i: Int, end: Int): Long =
            if (i < end) {
              val p = params(i)
              if (r.tryReadString(p.label)) {
                val mask = 1L << i
                if ((filledMask & mask) != 0) failDuplicate(p)
                constructorArgs(i) = p.typeclass.read(r).asInstanceOf[AnyRef]
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
                  constructorArgs(i) = value.asInstanceOf[AnyRef]
                  tryConstructWithMissingMembers(missingMask & iMask)
                case None ⇒ throw new Error.InvalidInputData(r.position, expected(s"Missing map key [${p.label}] for"))
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
