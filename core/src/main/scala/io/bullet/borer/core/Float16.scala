/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.core

/**
  * Wrapper for Float value that gets encoded to and decoded from 16-bit Half-Precision Floats (two-byte IEEE 754).
  */
final case class Float16(value: Float)

/**
  * Logic for converting half-precision (16-bit) floats to and from bytes.
  *
  * Source: https://stackoverflow.com/questions/6162651
  * (The author says it's public domain.)
  *
  * The documented author's "extensions" have been removed for clean round-tripping to and from Float.
  */
object Float16 {

  implicit val codec: Codec.Universal[Float16] = Codec(
    Encoder((w, x) ⇒ w.writeFloat16(x.value)),
    Decoder(r ⇒ Float16(r.readFloat16()))
  )

  /**
    * Hi-word of parameter value is ignored.
    */
  def shortToFloat(hbits: Int): Float = {
    var mant = hbits & 0x03ff // 10 bits mantissa
    var exp  = hbits & 0x7c00 // 5 bits exponent

    if (exp == 0x7c00) { // NaN/Inf
      exp = 0x3fc00
    } else if (exp != 0) { // normalized value
      exp += 0x1c000 // exp - 15 + 127
    } else {
      if (mant != 0) { // && exp==0 -> subnormal
        exp = 0x1c400 // make it normal
        do {
          mant <<= 1   // mantissa * 2
          exp -= 0x400 // decrease exp by 1
        } while ((mant & 0x400) == 0) // while not normal
        mant &= 0x3ff                 // discard subnormal bit
      }
    }
    java.lang.Float.intBitsToFloat((hbits & 0x8000) << 16 | (exp | mant) << 13)
  }

  /**
    * Result has only low-word set.
    */
  def floatToShort(value: Float): Int = {
    val fbits   = java.lang.Float.floatToIntBits(value)
    val sign    = fbits >>> 16 & 0x8000 // sign only
    val lfbits  = fbits & 0x7fffffff
    val rounded = lfbits + 0x1000
    if (rounded >= 0x47800000) { // might be or become NaN/Inf
      val sign2 = sign | 0x7c00
      if (rounded < 0x7f800000) { // is or must become NaN/Inf
        // was value but too large
        sign2 // make it +/-Inf
      } else {
        sign2 | // remains +/-Inf or NaN
        (fbits & 0x007fffff) >>> 13 // keep NaN (and Inf) bits
      }
    } else if (rounded >= 0x38800000) { // remains normalized value
      sign | rounded - 0x38000000 >>> 13 // exp - 127 + 15
    } else if (rounded < 0x33000000) { // too small for subnormal
      sign // becomes +/-0
    } else {
      val rounded2 = lfbits >>> 23; // tmp exp for subnormal calc
      sign | ((fbits & 0x7fffff | 0x800000) + // add subnormal bit
      (0x800000 >>> rounded2 - 102) >>>       // round depending on cut off
      126 - rounded2) // div by 2^(1-(exp-127+15)) and >> 13 | exp=0
    }
  }
}
