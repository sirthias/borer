/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.internal

// Direct transcription of the faster variant of the DFA-based UTF-8 Decoder by Bjoern Hoehrmann.
//
// Copyright (c) 2008-2009 Bjoern Hoehrmann <bjoern@hoehrmann.de>
// See http://bjoern.hoehrmann.de/utf-8/decoder/dfa/ for details.
object Utf8Decoder {

  private final val utf8d: Array[Byte] = Array[Byte](
    // The first part of the table maps bytes to character classes that
    // reduce the size of the transition table and create bitmasks
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
    9, 9, 9, 9, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 8, 8, 2,
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 10, 3, 3, 3, 3, 3, 3, 3, 3,
    3, 3, 3, 3, 4, 3, 3, 11, 6, 6, 6, 5, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
    // The second part is a transition table that maps a combination
    // of a state of the automaton and a character class to a state.
    0, 12, 24, 36, 60, 96, 84, 12, 12, 12, 48, 72, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 0, 12, 12, 12,
    12, 12, 0, 12, 0, 12, 12, 12, 24, 12, 12, 12, 12, 12, 24, 12, 24, 12, 12, 12, 12, 12, 12, 12, 12, 12, 24, 12, 12,
    12, 12, 12, 24, 12, 12, 12, 12, 12, 12, 12, 24, 12, 12, 12, 12, 12, 12, 12, 12, 12, 36, 12, 36, 12, 12, 12, 36, 12,
    12, 12, 12, 12, 36, 12, 36, 12, 12, 12, 36, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12,
  )

  final val INITIAL_STATE = 0L

  final val UTF8_ACCEPT = 0
  final val UTF8_REJECT = 12

  /**
    * Decodes the given `byte` and returns the new state.
    *
    * Start decoding with `state` set to [[INITIAL_STATE]].
    * The returned value can then be fed to the [[state]] and [[codepoint]]
    * methods to continue the process.
    */
  def decode(stateAndCodepoint: Long, byte: Byte): Long = {
    val state     = (stateAndCodepoint >> 32).toInt
    var codePoint = stateAndCodepoint.toInt
    val tpe       = utf8d(byte & 0xFF) & 0xFF

    codePoint = if (state != UTF8_ACCEPT) {
      (byte & 0x3f) | (codePoint << 6)
    } else {
      (0xff >> tpe) & byte
    }

    ((utf8d(256 + state + tpe) & 0xFFL) << 32) | codePoint.toLong
  }

  /**
    * Returns
    * - [[UTF8_ACCEPT]] if the decoding is done and [[codepoint]] can be called to retrieve the decoded codepoint
    * - [[UTF8_REJECT]] if the decoding has failed because the byte sequence was not valid UTF-8
    * - another value to signal that more bytes are needed
    */
  @inline def state(stateAndCodepoint: Long): Int = (stateAndCodepoint >>> 32).toInt

  /**
    * Returns the decoded codepoint.
    * Only valid if [[state]] returns [[UTF8_ACCEPT]] on the same argument.
    */
  @inline def codepoint(stateAndCodepoint: Long): Int = stateAndCodepoint.toInt
}
