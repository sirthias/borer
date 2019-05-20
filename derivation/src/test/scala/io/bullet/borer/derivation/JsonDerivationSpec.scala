/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.derivation

import java.nio.charset.StandardCharsets

import io.bullet.borer.{Decoder, Dom, Encoder, Json}

object JsonDerivationSpec extends DerivationSpec(Json) {
  import Dom._

  def encode[T: Encoder](value: T): String =
    Json.encode(value).withConfig(Json.EncodingConfig.default.copy(bufferSize = 13)).toUtf8String
  def decode[T: Decoder](encoded: String): T = Json.decode(encoded getBytes StandardCharsets.UTF_8).to[T].value
  def tryDecode[T: Decoder](encoded: String) = Json.decode(encoded getBytes StandardCharsets.UTF_8).to[T].valueTry

  def arrayBasedFooDom =
    ArrayElem.Unsized(
      IntElem(120),
      IntElem(66),
      IntElem(-10000),
      IntElem(1234567),
      IntElem(-1),
      DoubleElem(1.5f),
      DoubleElem(26.8),
      StringElem("borer"),
      ArrayElem.Unsized(),
      ArrayElem.Unsized(
        ArrayElem.Unsized(IntElem(255), IntElem(0), IntElem(0), IntElem(255)),
        ArrayElem.Unsized(IntElem(0), IntElem(255), IntElem(0), IntElem(255)),
        ArrayElem.Unsized(IntElem(0), IntElem(0), IntElem(255), IntElem(255))
      ))

  def arrayBasedMissingElemErrorMsg = "Cannot convert int value -10000 to Byte (input position 4)"

  def mapBasedFooDom =
    MapElem.Unsized(
      "char"   -> IntElem(120),
      "byte"   -> IntElem(66),
      "short"  -> IntElem(-10000),
      "int"    -> IntElem(1234567),
      "long"   -> IntElem(-1),
      "float"  -> DoubleElem(1.5f),
      "dub"    -> DoubleElem(26.8),
      "string" -> StringElem("borer"),
      "empty"  -> MapElem.Unsized(),
      "colors" -> ArrayElem.Unsized(
        MapElem.Unsized("red" -> IntElem(255), "green" -> IntElem(0), "blue"   -> IntElem(0), "alpha"   -> IntElem(255)),
        MapElem.Unsized("red" -> IntElem(0), "green"   -> IntElem(255), "blue" -> IntElem(0), "alpha"   -> IntElem(255)),
        MapElem.Unsized("red" -> IntElem(0), "green"   -> IntElem(0), "blue"   -> IntElem(255), "alpha" -> IntElem(255))
      ))

  // format: OFF
  def mapBased100Dom =
    MapElem.Unsized(
      "x00" -> BooleanElem(false),
      "x01" -> IntElem(120),
      "x02" -> IntElem(66),
      "x03" -> IntElem(-10000),
      "x04" -> IntElem(1234567),
      "x05" -> LongElem(2147483648L),
      "x06" -> DoubleElem(1.5),
      "x07" -> DoubleElem(26.8),
      "x08" -> StringElem("borer"),
      "x09" -> MapElem.Unsized("red" -> IntElem(0), "green" -> IntElem(0), "blue" -> IntElem(0), "alpha" -> IntElem(255)),
      "x10" -> BooleanElem(false),
      "x11" -> IntElem(120),
      "x12" -> IntElem(66),
      "x13" -> IntElem(-10000),
      "x14" -> IntElem(1234567),
      "x15" -> LongElem(2147483648L),
      "x16" -> DoubleElem(1.5),
      "x17" -> DoubleElem(26.8),
      "x18" -> StringElem("borer"),
      "x19" -> MapElem.Unsized("red" -> IntElem(0), "green" -> IntElem(0), "blue" -> IntElem(0), "alpha" -> IntElem(255)),
      "x20" -> BooleanElem(false),
      "x21" -> IntElem(120),
      "x22" -> IntElem(66),
      "x23" -> IntElem(-10000),
      "x24" -> IntElem(1234567),
      "x25" -> LongElem(2147483648L),
      "x26" -> DoubleElem(1.5),
      "x27" -> DoubleElem(26.8),
      "x28" -> StringElem("borer"),
      "x29" -> MapElem.Unsized("red" -> IntElem(0), "green" -> IntElem(0), "blue" -> IntElem(0), "alpha" -> IntElem(255)),
      "x30" -> BooleanElem(false),
      "x31" -> IntElem(120),
      "x32" -> IntElem(66),
      "x33" -> IntElem(-10000),
      "x34" -> IntElem(1234567),
      "x35" -> LongElem(2147483648L),
      "x36" -> DoubleElem(1.5),
      "x37" -> DoubleElem(26.8),
      "x38" -> StringElem("borer"),
      "x39" -> MapElem.Unsized("red" -> IntElem(0), "green" -> IntElem(0), "blue" -> IntElem(0), "alpha" -> IntElem(255)),
      "x40" -> BooleanElem(false),
      "x41" -> IntElem(120),
      "x42" -> IntElem(66),
      "x43" -> IntElem(-10000),
      "x44" -> IntElem(1234567),
      "x45" -> LongElem(2147483648L),
      "x46" -> DoubleElem(1.5),
      "x47" -> DoubleElem(26.8),
      "x48" -> StringElem("borer"),
      "x49" -> MapElem.Unsized("red" -> IntElem(0), "green" -> IntElem(0), "blue" -> IntElem(0), "alpha" -> IntElem(255)),
      "x50" -> BooleanElem(false),
      "x51" -> IntElem(120),
      "x52" -> IntElem(66),
      "x53" -> IntElem(-10000),
      "x54" -> IntElem(1234567),
      "x55" -> LongElem(2147483648L),
      "x56" -> DoubleElem(1.5),
      "x57" -> DoubleElem(26.8),
      "x58" -> StringElem("borer"),
      "x59" -> MapElem.Unsized("red" -> IntElem(0), "green" -> IntElem(0), "blue" -> IntElem(0), "alpha" -> IntElem(255)),
      "x60" -> BooleanElem(false),
      "x61" -> IntElem(120),
      "x62" -> IntElem(66),
      "x63" -> IntElem(-10000),
      "x64" -> IntElem(1234567),
      "x65" -> LongElem(2147483648L),
      "x66" -> DoubleElem(1.5),
      "x67" -> DoubleElem(26.8),
      "x68" -> StringElem("borer"),
      "x69" -> MapElem.Unsized("red" -> IntElem(0), "green" -> IntElem(0), "blue" -> IntElem(0), "alpha" -> IntElem(255)),
      "x70" -> BooleanElem(false),
      "x71" -> IntElem(120),
      "x72" -> IntElem(66),
      "x73" -> IntElem(-10000),
      "x74" -> IntElem(1234567),
      "x75" -> LongElem(2147483648L),
      "x76" -> DoubleElem(1.5),
      "x77" -> DoubleElem(26.8),
      "x78" -> StringElem("borer"),
      "x79" -> MapElem.Unsized("red" -> IntElem(0), "green" -> IntElem(0), "blue" -> IntElem(0), "alpha" -> IntElem(255)),
      "x80" -> BooleanElem(false),
      "x81" -> IntElem(120),
      "x82" -> IntElem(66),
      "x83" -> IntElem(-10000),
      "x84" -> IntElem(1234567),
      "x85" -> LongElem(2147483648L),
      "x86" -> DoubleElem(1.5),
      "x87" -> DoubleElem(26.8),
      "x88" -> StringElem("borer"),
      "x89" -> MapElem.Unsized("red" -> IntElem(0), "green" -> IntElem(0), "blue" -> IntElem(0), "alpha" -> IntElem(255)),
      "x90" -> BooleanElem(false),
      "x91" -> IntElem(120),
      "x92" -> IntElem(66),
      "x93" -> IntElem(-10000),
      "x94" -> IntElem(1234567),
      "x95" -> LongElem(2147483648L),
      "x96" -> DoubleElem(1.5),
      "x97" -> DoubleElem(26.8),
      "x98" -> StringElem("borer"),
      "x99" -> MapElem.Unsized("red" -> IntElem(0), "green" -> IntElem(0), "blue" -> IntElem(0), "alpha" -> IntElem(255)),
    )
  // format: ON

  def arrayBasedAnimalsDom =
    ArrayElem.Unsized(
      ArrayElem.Unsized(StringElem("Dog"), ArrayElem.Unsized(IntElem(12), StringElem("Fred"))),
      ArrayElem
        .Unsized(StringElem("TheCAT"), ArrayElem.Unsized(DoubleElem(1.0f), StringElem("none"), StringElem("there"))),
      ArrayElem.Unsized(StringElem("Dog"), ArrayElem.Unsized(IntElem(4), StringElem("Lolle"))),
      ArrayElem.Unsized(IntElem(42), BooleanElem.True))

  def mapBasedAnimalsDom =
    ArrayElem.Unsized(
      MapElem.Sized("Dog" -> MapElem.Sized("age" -> IntElem(12), "name" -> StringElem("Fred"))),
      MapElem.Sized(
        "TheCAT" -> MapElem
          .Sized("weight" -> Float16Elem(1.0f), "color" -> StringElem("none"), "home" -> StringElem("there"))),
      MapElem.Sized("Dog"       -> MapElem.Sized("age"  -> IntElem(4), "name" -> StringElem("Lolle"))),
      MapElem.Sized(IntElem(42) -> MapElem.Sized("tail" -> BooleanElem.True)))
}
