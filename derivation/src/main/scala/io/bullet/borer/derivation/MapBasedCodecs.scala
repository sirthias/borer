/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.derivation

import io.bullet.borer.core.{Codec, Decoder, Encoder, Writer}
import magnolia._

import scala.annotation.tailrec
import scala.collection.mutable

object MapBasedCodecs {

  object deriveEncoder {
    type Typeclass[T] = Encoder.Universal[T]

    def combine[T](ctx: CaseClass[Encoder.Universal, T]): Encoder.Universal[T] = {
      val params = ctx.parameters.asInstanceOf[mutable.WrappedArray[Param[Encoder.Universal, T]]].array
      Encoder[Nothing, T, Unit] { (w, value) ⇒
        @tailrec def rec(w: Writer.Universal, ix: Int): Unit =
          if (ix < params.length) {
            val p = params(ix)
            w.writeString(p.label)
            rec(p.typeclass.write(w, p.dereference(value)), ix + 1)
          }
        rec(w.writeMapHeader(params.length), 0)
      }
    }

    def dispatch[T](ctx: SealedTrait[Encoder.Universal, T]): Encoder.Universal[T] =
      ArrayBasedCodecs.deriveEncoder.dispatch(ctx)

    def apply[T]: Encoder.Universal[T] = macro Magnolia.gen[T]
  }

  object deriveDecoder {
    type Typeclass[T] = Decoder.Universal[T]

    def combine[T](ctx: CaseClass[Decoder.Universal, T]): Decoder.Universal[T] = {
      val params = ctx.parameters.asInstanceOf[mutable.WrappedArray[Param[Decoder.Universal, T]]].array
      Decoder.of[T].from { r ⇒
        val constructorArgs = new mutable.WrappedArray.ofRef(new Array[AnyRef](params.length))
        @tailrec def rec(ix: Int): T =
          if (ix < params.length) {
            val label = r.readString()
            @tailrec def findParam(i: Int): AnyRef =
              if (i < params.length) {
                val p = params(ix)
                if (p.label == label) p.typeclass.read(r).asInstanceOf[AnyRef]
                else findParam(i + 1)
              } else r.unexpectedDataItem(s"a member of type [${ctx.typeName.full}]", s"member with name [$label]")
            constructorArgs(ix) = findParam(0)
            rec(ix + 1)
          } else ctx.rawConstruct(constructorArgs)

        if (!r.tryReadMapHeader(params.length)) {
          val m = s"Map Header with length ${params.length} for decoding an instance of type [${ctx.typeName.full}]"
          r.unexpectedDataItem(m)
        } else rec(0)
      }
    }

    def dispatch[T](ctx: SealedTrait[Decoder.Universal, T]): Decoder.Universal[T] =
      ArrayBasedCodecs.deriveDecoder.dispatch(ctx)

    def apply[T]: Decoder.Universal[T] = macro Magnolia.gen[T]
  }

  def deriveCodec[T]: Codec.Universal[T] = macro Macros.deriveCodecImpl[T]
}
