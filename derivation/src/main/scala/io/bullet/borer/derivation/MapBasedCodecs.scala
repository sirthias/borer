/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.derivation

import io.bullet.borer._
import io.bullet.borer.derivation.internal._
import io.bullet.borer.deriver.DeriveWith

import scala.reflect.macros.blackbox

/**
  * Derivation macros for map-based encodings.
  */
object MapBasedCodecs {

  /**
    * Macro that creates an [[Encoder]] for [[T]] provided that
    * - [[T]] is a `case class`, `sealed abstract class` or `sealed trait`
    * - [[Encoder]] instances for all members of [[T]] (if [[T]] is a `case class`)
    *   or all sub-types of [[T]] (if [[T]] is an ADT) are implicitly available
    *
    * Case classes are converted into a map of values, one key-value pair for each member.
    * The key for each member is a `String` holding the member's name.
    * This can be customized with the [[key]] annotation.
    */
  def deriveEncoder[T]: Encoder[T] = macro Macros.encoder[T]

  /**
    * Macro that creates an [[Encoder]] for [[T]] and all direct and indirect sub-types of [[T]],
    * which are concrete, i.e. not abstract.
    * [[T]] must be a `sealed abstract class` or `sealed trait`.
    *
    * It works by generating a code block such as this one:
    *
    * {{{
    *   implicit val a = deriveEncoder[A]     // one such line is generated for each concrete
    *   implicit val b = deriveEncoder[B]     // direct or indirect sub-type of T which doesn't
    *   implicit val c = deriveEncoder[C]     // already have an implicit Encoder available
    *   ...
    *   deriveEncoder[T]
    * }}}
    *
    * If an [[Encoder]] for a certain concrete sub-type `S <: T` is already implicitly available
    * at the macro call-site the respective line for the sub-type is **not** generated.
    *
    * If an [[Encoder]] for a certain abstract sub-type `S <: T` is already implicitly available
    * at the macro call-site the respective lines for **all** sub-types of `S` are **not** generated.
    *
    * This means that you can specify your own custom Encoders for concrete sub-types or whole branches
    * of the sub-type hierarchy and they will be properly picked up rather than create conflicts.
    */
  def deriveAllEncoders[T]: Encoder[T] = macro Macros.allEncoders[T]

  /**
    * Macro that creates a [[Decoder]] for [[T]] provided that
    * - [[T]] is a `case class`, `sealed abstract class` or `sealed trait`
    * - [[Decoder]] instances for all members of [[T]] (if [[T]] is a `case class`)
    *   or all sub-types of [[T]] (if [[T]] is an ADT) are implicitly available
    *
    * Case classes are created from a map of values, one key-value pair for each member.
    * The key for each member is a `String` holding the member's name.
    * This can be customized with the [[key]] annotation.
    */
  def deriveDecoder[T]: Decoder[T] = macro Macros.decoder[T]

  /**
    * Macro that creates a [[Decoder]] for [[T]] and all direct and indirect sub-types of [[T]],
    * which are concrete, i.e. not abstract.
    * [[T]] must be a `sealed abstract class` or `sealed trait`.
    *
    * It works by generating a code block such as this one:
    *
    * {{{
    *   implicit val a = deriveDecoder[A]     // one such line is generated for each concrete
    *   implicit val b = deriveDecoder[B]     // direct or indirect sub-type of T which doesn't
    *   implicit val c = deriveDecoder[C]     // already have an implicit Decoder available
    *   ...
    *   deriveDecoder[T]
    * }}}
    *
    * If a [[Decoder]] for a certain concrete sub-type `S <: T` is already implicitly available
    * at the macro call-site the respective line for the sub-type is **not** generated.
    *
    * If a [[Decoder]] for a certain abstract sub-type `S <: T` is already implicitly available
    * at the macro call-site the respective lines for **all** sub-types of `S` are **not** generated.
    *
    * This means that you can specify your own custom Decoders for concrete sub-types or whole branches
    * of the sub-type hierarchy and they will be properly picked up rather than create conflicts.
    */
  def deriveAllDecoders[T]: Decoder[T] = macro Macros.allDecoders[T]

  /**
    * Macro that creates an [[Encoder]] and [[Decoder]] pair for [[T]].
    * Convenience shortcut for `Codec(deriveEncoder[T], deriveDecoder[T])"`.
    */
  def deriveCodec[T]: Codec[T] = macro Macros.codec[T]

  /**
    * Macro that creates an [[Encoder]] and [[Decoder]] pair for [[T]] and all direct and indirect sub-types of [[T]].
    * Convenience shortcut for `Codec(deriveAllEncoders[T], deriveAllDecoders[T])"`.
    */
  def deriveAllCodecs[T]: Codec[T] = macro Macros.allCodecs[T]

  private object Macros {
    import MacroSupport._

    def encoder[T: ctx.WeakTypeTag](ctx: blackbox.Context): ctx.Tree = DeriveWith[T](ctx) {
      new CodecDeriver[ctx.type](ctx) {
        import c.universe._

        def deriveForCaseObject(tpe: Type, module: ModuleSymbol) =
          q"$encoderCompanion[${module.typeSignature}]((w, _) => w.writeEmptyMap())"

        def deriveForCaseClass(
            tpe: Type,
            companion: ModuleSymbol,
            params: List[CaseParam],
            annotationTrees: List[Tree],
            constructorIsPrivate: Boolean) = {

          def encName(p: CaseParam, suffix: String = "") = TermName(s"e${p.index}$suffix")

          val encodersForParams = params.map(p => p -> p.getImplicit(encoderType)).toMap
          val (basicParams, nonBasicParams) =
            params.partition(p => p.isBasicType && encodersForParams(p).exists(isDefinedOn(_, encoderCompanion)))
          val fieldEncDefs = nonBasicParams.map { p =>
            val paramType = p.paramType.tpe
            val fieldEnc = encodersForParams(p).getOrElse {
              error(s"Could not find implicit Encoder[$paramType] for parameter `${p.name}` of case class $tpe")
            }
            val fieldEncWithDefault = p.defaultValueMethod match {
              case Some(x) => q"$fieldEnc.withDefaultValue($x)"
              case None    => fieldEnc
            }
            q"private[this] val ${encName(p)} = $fieldEncWithDefault"
          }
          val nonBasicDefaultValues = nonBasicParams.flatMap { p =>
            p.defaultValueMethod.map(defaultValue => q"val ${encName(p, "d")} = $defaultValue")
          }
          val basicFieldOutputFlags = basicParams.flatMap { p =>
            p.defaultValueMethod.map { defaultValue =>
              q"val ${encName(p, "o")} = (value.${p.name} != $defaultValue) || { count -= 1; false }"
            }
          }
          val nonBasicFieldOutputFlags = nonBasicParams.map { p =>
            val tail = p.defaultValueMethod match {
              case Some(_) => q"value.${p.name} != ${encName(p, "d")}"
              case None    => q"true"
            }
            val pt = tq"$encoderCompanion.PossiblyWithoutOutput[${p.paramType.tpe}]"
            q"""val ${encName(p, "o")} =
                  (${encName(p)} match {
                    case x: $pt => x producesOutputFor value.${p.name}
                    case _      => $tail
                  }) || { count -= 1; false }"""
          }
          val writeEntries = params.map { p =>
            val key           = p.key()
            val writeKey      = q"w.${TermName(s"write${key.productPrefix}")}(${literal(key.value)})"
            val isBasic       = basicParams contains p
            val method        = TermName(s"write${if (isBasic) p.paramType.tpe.toString else ""}")
            val rawWriteEntry = q"$writeKey.$method(value.${p.name})"
            if (isBasic) {
              if (p.defaultValueMethod.isDefined) q"if (${encName(p, "o")}) $rawWriteEntry" else rawWriteEntry
            } else q"if (${encName(p, "o")}) $rawWriteEntry(${encName(p)})"
          }
          val encoderName = TypeName(s"${tpe.typeSymbol.name.decodedName}Encoder")

          q"""final class $encoderName {
                ..$nonBasicDefaultValues
                ..$fieldEncDefs
                def write(w: $writerType, value: $tpe): w.type = {
                  var count = ${params.size}
                  ..$basicFieldOutputFlags
                  ..$nonBasicFieldOutputFlags
                  def writeEntries(w: $writerType): w.type = {
                    ..$writeEntries
                    w
                  }
                  if (w.writingCbor) writeEntries(w.writeMapHeader(count))
                  else writeEntries(w.writeMapStart()).writeBreak()
                }
              }

              new $encoderType[$tpe] {
                private[this] var inner: $encoderName = _
                def write(w: $writerType, value: $tpe) = {
                  if (inner eq null) inner = new $encoderName
                  inner.write(w, value)
                }
              }: $encoderType[$tpe]"""
        }

        def deriveForSealedTrait(node: AdtTypeNode) =
          deriveAdtEncoder(
            node,
            x => q"""val typeName = ${node.tpe.toString}
                val strategy = implicitly[$borerPkg.AdtEncodingStrategy]
                strategy.writeAdtEnvelopeOpen(w, typeName)
                $x
                strategy.writeAdtEnvelopeClose(w, typeName)""")
      }
    }

    def allEncoders[T: c.WeakTypeTag](c: blackbox.Context): c.Tree =
      deriveAll(c)(isEncoder = true, "MapBasedCodecs", "deriveAllEncoders", "deriveEncoder")

    def decoder[T: ctx.WeakTypeTag](ctx: blackbox.Context): ctx.Tree = DeriveWith[T](ctx) {
      new CodecDeriver[ctx.type](ctx) {
        import c.universe._

        def deriveForCaseObject(tpe: Type, module: ModuleSymbol) =
          q"$decoderCompanion(r => r.readMapClose(r.readMapOpen(0), $module))"

        def deriveForCaseClass(
            tpe: Type,
            companion: ModuleSymbol,
            params: List[CaseParam],
            annotationTrees: List[Tree],
            constructorIsPrivate: Boolean) = {
          if (constructorIsPrivate)
            error(s"Cannot derive Decoder[$tpe] because the primary constructor of `$tpe` is private")

          def decName(p: CaseParam) = TermName(s"d${p.index}")
          def varName(p: CaseParam) = TermName(s"p${p.index}")
          def expected(s: String)   = s"$s for decoding an instance of type `$tpe`"

          val arity         = params.size
          val keysAndParams = new Array[(Key, CaseParam)](arity)
          params.foreach(p => keysAndParams(p.index) = p.key() -> p)
          val keysAndParamsSorted = keysAndParams.clone()
          sortAndVerifyNoCollisions(keysAndParamsSorted) {
            case (k, a, b) =>
              c.abort(
                ctx.enclosingPosition,
                s"@key collision: parameters `${a.name}` and `${b.name}` " +
                  s"of case class ADT `$tpe` share the same type id `${k.value}`")
          }

          val decodersForParams = params.map(p => p -> p.getImplicit(decoderType)).toMap
          val nonBasicParams =
            params.filterNot(p => p.isBasicType && decodersForParams(p).exists(isDefinedOn(_, decoderCompanion)))

          def readField(p: CaseParam) = {
            val tpe = p.paramType.tpe
            if (nonBasicParams contains p) q"r.read[$tpe]()(${decName(p)})" else q"r.${TermName(s"read$tpe")}()"
          }

          val fieldDecDefs = nonBasicParams.map { p =>
            val paramType = p.paramType.tpe
            val fieldDec = decodersForParams(p).getOrElse {
              error(s"Could not find implicit Decoder[$paramType] for parameter `${p.name}` of case class $tpe")
            }
            val fieldDecWithDefault = p.defaultValueMethod match {
              case Some(x) => q"$fieldDec.withDefaultValue($x)"
              case None    => fieldDec
            }
            q"private[this] val ${decName(p)} = $fieldDecWithDefault"
          }

          val maskDef =
            if (arity <= 32) q"var mask = 0" :: Nil
            else if (arity <= 64) q"var mask = 0L" :: Nil
            else if (arity <= 128) q"var mask0 = 0L" :: q"var mask1 = 0L" :: Nil
            else error("Case classes mit > 128 fields are not supported")

          def setMaskBit(p: CaseParam) =
            if (arity <= 32) q"mask |= ${1 << p.index}"
            else if (arity <= 64) q"mask |= ${1L << p.index}"
            else q"${TermName(s"mask${p.index >> 6}")} |= ${1L << p.index}"

          def maskBitSet(p: CaseParam) =
            if (arity <= 32) q"(mask & ${1 << p.index}) != 0"
            else if (arity <= 64) q"(mask & ${1L << p.index}) != 0"
            else q"(${TermName(s"mask${p.index >> 6}")} & ${1L << p.index}) != 0"

          val maskIncomplete =
            if (arity <= 32) q"mask != ${(1 << arity) - 1}"
            else if (arity <= 64) q"mask != ${(1L << arity) - 1}"
            else q"mask0 != -1 || mask1 != ${(1L << (arity - 64)) - 1}"

          val maskAsParams =
            if (arity <= 32) q"mask: Int" :: Nil
            else if (arity <= 64) q"mask: Long" :: Nil
            else q"mask0: Long" :: q"mask1: Long" :: Nil

          val maskAsArgs = if (arity <= 64) q"mask" :: Nil else q"mask0" :: q"mask1" :: Nil

          val fieldVarDefs: List[Tree] = keysAndParams.map {
            case (key, p) =>
              val defaultValue = p.defaultValueMethod.getOrElse(q"null.asInstanceOf[${p.paramType.tpe}]")
              q"""var ${varName(p)} = if (rem != 0 && ${r("tryRead", key)}) {
                  ${setMaskBit(p)}
                  rem -= 1
                  ${readField(p)}
                } else $defaultValue"""
          }.toList

          def readFields(start: Int, end: Int): Tree =
            if (start < end) {
              val mid        = (start + end) >> 1
              val (key, p)   = keysAndParamsSorted(mid)
              val methodName = TermName(s"readFields_${start}_$end")
              val onMatch =
                q"""if (${maskBitSet(p)}) failDuplicate(${literal(key.value)})
                    ${varName(p)} = ${readField(p)}
                    ${setMaskBit(p)}"""
              if (start < mid) {
                q"""def $methodName(): Unit = {
                  val cmp = ${r("tryRead", key, "Compare")}
                  if (cmp < 0) ${readFields(start, mid)}
                  else if (cmp > 0) ${readFields(mid + 1, end)}
                  else $onMatch
                }
                $methodName()"""
              } else q"if (${r("tryRead", key, "Compare")} == 0) $onMatch else r.skipTwoElements()"
            } else q"r.skipTwoElements()"

          val typeName    = tpe.typeSymbol.name.decodedName.toString
          val decoderName = TypeName(s"${typeName}Decoder")

          val failMissingDef =
            if (params.nonEmpty) {
              q"""def failMissing(..$maskAsParams) = {
                    $helpers.failMissing(r, ${literal(typeName)},
                      ..$maskAsArgs, Array(..${params.map(p => literal(p.stringName))}))}"""
            } else q"()"

          val construct = {
            val compApply = q"$companion.apply(..${params.map(varName)})"
            if (params.nonEmpty) {
              val testMaskDefs = {
                def reqMask(iter: Iterator[CaseParam]) = iter.foldLeft(0L) { (acc, p) =>
                  if (p.defaultValueMethod.isDefined) acc | (1L << p.index) else acc
                }
                if (arity <= 32) {
                  q"val testMask = mask | ${literal(reqMask(params.iterator).toInt | (-1 << arity))}" :: Nil
                } else if (arity <= 64) {
                  q"val testMask = mask | ${literal(reqMask(params.iterator) | (-1L << arity))}" :: Nil
                } else {
                  q"val testMask0 = mask0 | ${literal(reqMask(params.iterator take 64))}" ::
                  q"val testMask1 = mask1 | ${literal(reqMask(params.iterator drop 64) | (-1L << (arity - 64)))}" :: Nil
                }
              }
              val testMaskTest = if (arity <= 64) q"testMask" else q"testMask0 & testMask1"
              val testMaskArgs = if (arity <= 64) q"testMask" :: Nil else q"testMask0" :: q"testMask1" :: Nil
              q"""..$testMaskDefs
                  if ($testMaskTest == -1) $compApply
                  else failMissing(..$testMaskArgs)"""
            } else compApply
          }

          q"""final class $decoderName {
                ..$fieldDecDefs
                def read(r: $readerType): $tpe = {
                  def failDuplicate(k: Any) =
                    throw new $borerPkg.Borer.Error.InvalidInputData(r.position,
                      StringContext("Duplicate map key `", ${expected("` encountered during")}).s(k))
                  $failMissingDef
                  def readObject(remaining: scala.Int): $tpe = {
                    var rem = remaining
                    ..$maskDef
                    ..$fieldVarDefs
                    while (rem > 0 || rem < 0 && !r.tryReadBreak()) {
                      if ($maskIncomplete) {
                        ..${readFields(0, arity)}
                      } else r.skipTwoElements()
                      rem -= 1
                    }
                    $construct
                  }
                  if (r.tryReadMapStart()) readObject(-1)
                  else if (r.hasMapHeader) {
                    val mapLength = r.readMapHeader()
                    if (mapLength > Int.MaxValue) r.overflow("Maps with more than 2^31 entries are not supported")
                    readObject(mapLength.toInt)
                  } else r.unexpectedDataItem(${expected("Map Start or Map Header")})
                }
              }

              new $decoderType[$tpe] {
                private[this] var inner: $decoderName = _
                def read(r: $readerType): $tpe = {
                  if (inner eq null) inner = new $decoderName
                  inner.read(r)
                }
              }: $decoderType[$tpe]"""
        }

        def deriveForSealedTrait(node: AdtTypeNode) =
          deriveAdtDecoder(
            node,
            q"private[this] val strategy = implicitly[$borerPkg.AdtEncodingStrategy]",
            x => q"""val typeName = ${node.tpe.toString}
                val opening = strategy.readAdtEnvelopeOpen(r, typeName)
                val result = $x
                strategy.readAdtEnvelopeClose(r, opening, typeName)
                result""")
      }
    }

    def allDecoders[T: c.WeakTypeTag](c: blackbox.Context): c.Tree =
      deriveAll(c)(isEncoder = false, "MapBasedCodecs", "deriveAllDecoders", "deriveDecoder")

    def codec[T: c.WeakTypeTag](c: blackbox.Context): c.Tree =
      codecMacro(c)("MapBasedCodecs", "deriveEncoder", "deriveDecoder")

    def allCodecs[T: c.WeakTypeTag](c: blackbox.Context): c.Tree =
      codecMacro(c)("MapBasedCodecs", "deriveAllEncoders", "deriveAllDecoders")
  }
}
