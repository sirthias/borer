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
 * A `DerivationConfig` is a purely static (compile-time) construct, which is evaluated by the derivation macros
 * during compilation. Therefore customizations must be brought into implicit scope as an `inline given`
 * with a statically known value, e.g.
 *
 * {{{
 *   inline given DerivationConfig = DerivationConfig(
 *     encodeCaseClassMemberDefaultValues = true
 *   )
 * }}}
 *
 * or
 *
 * {{{
 *   inline given DerivationConfig = DerivationConfig().withFlatAdtEncoding().withMemberNaming(Naming.snake_case)
 * }}}
 *
 * All arguments must be literals (or other statically known values like `inline val`s or `Naming` cases).
 * Non-inline givens or runtime values result in a compile-time error.
 *
 * @param encodeCaseClassMemberDefaultValues
 *   If `true` case class members whose value equals their default value are still written during encoding.
 *   If `false` (the default) they are omitted. (Only relevant for map-based codecs.)
 *
 * @param encodeEnumCasesAsObjects
 *   By default singleton `enum` cases (i.e. ones without parameters) are encoded simply as their type ID,
 *   e.g. `"Earth"`. If this flag is `true` they are instead encoded like case objects, i.e. as regular ADT members
 *   with an empty value, e.g. `{"Earth":{}}` (map-based, default ADT encoding), `{"_type":"Earth"}`
 *   (map-based, flat ADT encoding) or `["Earth",[]]` (array-based).
 *   Decoding always accepts both forms, independently of this setting.
 *
 * @param flatAdtEncoding
 *   Selects the encoding of ADTs (sealed traits, sealed abstract classes and enums) in map-based codecs.
 *   If `None` (the default and recommended setting) an ADT instance is encoded as a single-element map with
 *   the type ID as key and the instance encoding as the value, e.g. `{"Dog":{"age":2,"name":"Lolle"}}`.
 *   If defined the type ID is instead written as an extra map member (with the key `typeMemberName`),
 *   e.g. `{"_type":"Dog","age":2,"name":"Lolle"}`. This requires that all ADT sub types encode to a map and is
 *   less efficient (with regard to encoding/decoding process as well as encoded size) than the default.
 *   The type member will be the first member in the encoding but can be anywhere during decoding.
 *   (The earlier it appears the better the decoding performance and the lesser the caching memory requirements.)
 *
 * @param memberNaming
 *   How object member names should be encoded if they carry no explicit @key annotation.
 *   By default the name of case class fields are used as they are.
 *   Doesn't apply to the `typeMemberName` of the flat ADT encoding and
 *   also not to "real" `Map[K, V]` keys (which don't pass through derivation code anyway).
 *
 * @param adtTypeIdNaming
 *   Same as `memberNaming` but for ADT type IDs (incl. singleton enum cases).
 */
final case class DerivationConfig(
    encodeCaseClassMemberDefaultValues: Boolean = false,
    encodeEnumCasesAsObjects: Boolean = false,
    flatAdtEncoding: Option[DerivationConfig.FlatAdtEncoding] = None,
    memberNaming: DerivationConfig.Naming = DerivationConfig.Naming.Unchanged,
    adtTypeIdNaming: DerivationConfig.Naming = DerivationConfig.Naming.Unchanged,
):
  inline def withCaseClassMemberDefaultValueEncoding: DerivationConfig =
    copy(encodeCaseClassMemberDefaultValues = true)

  inline def withEnumCasesAsObjectEncoding: DerivationConfig =
    copy(encodeEnumCasesAsObjects = true)

  inline def withFlatAdtEncoding(typeMemberName: String = "_type", maxBufferSize: Int = 1024 * 1024): DerivationConfig =
    copy(flatAdtEncoding = Some(DerivationConfig.FlatAdtEncoding(typeMemberName, maxBufferSize)))

  inline def withNaming(scheme: DerivationConfig.Naming): DerivationConfig =
    copy(memberNaming = scheme, adtTypeIdNaming = scheme)

  inline def withMemberNaming(scheme: DerivationConfig.Naming): DerivationConfig =
    copy(memberNaming = scheme)

  inline def withAdtTypeIdNaming(scheme: DerivationConfig.Naming): DerivationConfig =
    copy(adtTypeIdNaming = scheme)

object DerivationConfig:
  inline given default: DerivationConfig = DerivationConfig()

  /**
   * Configuration of Flat ADT Encoding.
   *
   * @param typeMemberName
   *   The name of the map member holding the type ID.
   *
   * @param maxBufferSize
   *   The maximum number of bytes that may be buffered during decoding when looking for the type member.
   *   Must be a power of 2.
   */
  final case class FlatAdtEncoding(typeMemberName: String, maxBufferSize: Int):
    require(Util.isPowerOf2(maxBufferSize), s"maxBufferSize must be a power of 2 but was $maxBufferSize")

  /**
   * The available member name transformations.
   *
   * Names are split into words at `_`, `-` and spaces as well as at case transitions, whereby
   * acronyms are kept together as one word (e.g. `HTTPServer` -> `HTTP` + `Server`) and digits stick to
   * the preceding characters (e.g. `field1` or `v2Api` -> `v2` + `Api`).
   * Leading underscores are preserved (e.g. `_myType` -> `_my_type` with `snake_case`).
   */
  enum Naming:
    case Unchanged    // fooBarHTTP -> fooBarHTTP
    case camelCase    // fooBarHTTP -> fooBarHttp
    case `kebab-case` // fooBarHTTP -> foo-bar-http
    case PascalCase   // fooBarHTTP -> FooBarHttp
    case snake_case   // fooBarHTTP -> foo_bar_http
    case SNAKE_CASE   // fooBarHTTP -> FOO_BAR_HTTP

    def apply(name: String): String =
      if (this == Unchanged) name
      else
        val prefixLen = name.indexWhere(_ != '_') match
          case -1 => name.length
          case x  => x
        val words = Naming.splitWords(name.substring(prefixLen))
        if (words.isEmpty) name
        else
          val sb                    = new java.lang.StringBuilder(name.length + 8).append(name, 0, prefixLen)
          def lower(s: String)      = s.map(_.toLower)
          def upper(s: String)      = s.map(_.toUpper)
          def capitalize(s: String) = s"${s.charAt(0).toUpper}${lower(s.substring(1))}"
          def join(sep: String, f: String => String): Unit =
            words.zipWithIndex.foreach { (w, ix) =>
              if (ix > 0) sb.append(sep)
              sb.append(f(w))
            }
          this match
            case `camelCase` =>
              sb.append(lower(words.head))
              words.tail.foreach(w => sb.append(capitalize(w)))
            case `kebab-case` => join("-", lower)
            case `PascalCase` => join("", capitalize)
            case `snake_case` => join("_", lower)
            case `SNAKE_CASE` => join("_", upper)
            case `Unchanged`  => throw new IllegalStateException
          sb.toString

  object Naming:

    private[derivation] def splitWords(s: String): List[String] =
      val words         = List.newBuilder[String]
      val sb            = new java.lang.StringBuilder
      def flush(): Unit =
        if (sb.length > 0)
          words += sb.toString
          sb.setLength(0)
      var i = 0
      while (i < s.length)
        val c = s.charAt(i)
        if (c == '_' || c == '-' || c == ' ') flush()
        else
          if (c.isUpper && i > 0)
            val prev = s.charAt(i - 1)
            val next = if (i + 1 < s.length) s.charAt(i + 1) else ' '
            if (prev.isLower || prev.isDigit || prev.isUpper && next.isLower) flush()
          sb.append(c)
        i += 1
      flush()
      words.result()
