/*
 * Copyright (c) 2019-2024 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.encodings

abstract class LookupBaseEncoding(_name: String, _bitsPerChar: Int, val alphabet: String)
    extends BaseEncoding(_name, _bitsPerChar):

  protected val alphabetChars = alphabet.toCharArray

  protected val lookup =
    val array = new Array[Byte](128)
    java.util.Arrays.fill(array, -1: Byte)
    (0 until alphabetChars.length).foreach(i => array(alphabetChars(i).toInt) = i.toByte)
    array
