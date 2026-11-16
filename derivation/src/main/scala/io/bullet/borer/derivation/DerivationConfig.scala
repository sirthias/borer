/*
 * Copyright (c) 2019-2026 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.derivation

import io.bullet.borer.internal.Util

/**
 * Allows for custom configuration of certain derivation behaviors.
 *
 * Customizations must be brought into implicit scope for activation, e.g.
 *
 * {{{
 *   given DerivationConfig = DerivationConfig(
 *     encodeCaseClassMemberDefaultValues = true
 *   )
 * }}}
 *
 * @param encodeCaseClassMemberDefaultValues
 *   If `true` case class members whose value equals their default value are still written during encoding.
 *   If `false` (the default) they are omitted. (Only relevant for map-based codecs.)
 *
 * @param flatAdtEncoding
 *   Selects the encoding of ADTs (sealed traits, sealed abstract classes and enums) in map-based codecs.
 *   If `false` (the default and recommended setting) an ADT instance is encoded as a single-element map with
 *   the type ID as key and the instance encoding as the value, e.g. `{"Dog":{"age":2,"name":"Lolle"}}`.
 *   If `true` the type ID is instead written as an extra map member (with the key `flatAdtTypeMemberName`),
 *   e.g. `{"_type":"Dog","age":2,"name":"Lolle"}`. This requires that all ADT sub types encode to a map and is
 *   less efficient (with regard to encoding/decoding process as well as encoded size) than the default.
 *   The type member will be the first member in the encoding but can be anywhere during decoding.
 *   (The earlier it appears the better the decoding performance and the lesser the caching memory requirements.)
 *
 * @param flatAdtTypeMemberName
 *   The name of the map member holding the type ID if `flatAdtEncoding` is enabled.
 *
 * @param flatAdtMaxBufferSize
 *   The maximum number of bytes that may be buffered during decoding when looking for the type member if
 *   `flatAdtEncoding` is enabled. Must be a power of 2.
 *
 * @param encodeEnumCasesAsObjects
 *   By default singleton `enum` cases (i.e. ones without parameters) are encoded simply as their type ID,
 *   e.g. `"Earth"`. If this flag is `true` they are instead encoded like case objects, i.e. as regular ADT members
 *   with an empty value, e.g. `{"Earth":{}}` (map-based, default ADT encoding), `{"_type":"Earth"}`
 *   (map-based, flat ADT encoding) or `["Earth",[]]` (array-based).
 *   Decoding always accepts both forms, independently of this setting.
 */
final case class DerivationConfig(
    encodeCaseClassMemberDefaultValues: Boolean = false,
    flatAdtEncoding: Boolean = false,
    flatAdtTypeMemberName: String = "_type",
    flatAdtMaxBufferSize: Int = 1024 * 1024,
    encodeEnumCasesAsObjects: Boolean = false
):
  require(
    Util.isPowerOf2(flatAdtMaxBufferSize),
    s"flatAdtMaxBufferSize must be a power of 2 but was $flatAdtMaxBufferSize")

object DerivationConfig {
  given default: DerivationConfig = DerivationConfig()
}
