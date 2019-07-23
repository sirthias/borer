/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.encodings

abstract class BaseEncoding(val name: String, val bitsPerChar: Int) {

  def encode(bytes: Array[Byte]): Array[Char]

  def decode(chars: Array[Char]): Array[Byte]
}

object BaseEncoding {

  /**
    * @see http://philzimmermann.com/docs/human-oriented-base-32-encoding.txt
    */
  val zbase32 = ZBase32

  /**
    * @see https://tools.ietf.org/html/rfc4648#section-8
    */
  val base16 = Base16

  /**
    * @see https://tools.ietf.org/html/rfc4648#section-6
    */
  val base32 = new Base32("base32", "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567")

  /**
    * @see https://tools.ietf.org/html/rfc4648#section-7
    */
  val base32hex = new Base32("base32hex", "0123456789ABCDEFGHIJKLMNOPQRSTUV")

  /**
    * @see https://en.wikipedia.org/wiki/Base32#Crockford's_Base32
    */
  val base32crockford = new Base32("base32crockford", "0123456789ABCDEFGHJKMNPQRSTVWXYZ")

  /**
    * @see https://tools.ietf.org/html/rfc4648#section-4
    */
  val base64 = new Base64("base64", "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

  /**
    * @see https://tools.ietf.org/html/rfc4648#section-5
    */
  val base64url = new Base64("base32", "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_")

}
