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

import scala.reflect.macros.blackbox

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
    * Macro that creates an [[Encoder]] and [[Decoder]] pair for [[T]].
    * Convenience shortcut for `Codec(deriveEncoder[T], deriveDecoder[T])"`.
    */
  def deriveCodec[T]: Codec[T] = macro Macros.codec[T]

  private object Macros {
    import MacroSupport._

    def encoder[T: ctx.WeakTypeTag](ctx: blackbox.Context): ctx.Tree = DeriveWith[T](ctx) {
      new CodecDeriver[ctx.type](ctx) {
        import c.universe._

        def deriveForCaseClass(
            tpe: Type,
            companion: ModuleSymbol,
            params: List[CaseParam],
            annotationTrees: List[Tree],
            constructorIsPrivate: Boolean) = {

          def encName(p: CaseParam, suffix: String = "") = TermName(s"e${p.index}$suffix")
          val nonBasicParams                             = params.filterNot(_.isBasicType)
          val fieldEncDefs = nonBasicParams.map { p =>
            val paramType = p.paramType.tpe
            val fieldEnc =
              Option(c.inferImplicitValue(toType(tq"_root_.io.bullet.borer.Encoder[$paramType]")))
                .filterNot(_.isEmpty)
                .getOrElse {
                  error(s"Could not find implicit Encoder[$paramType] for parameter `${p.name}` of case class $tpe")
                }
            val fieldEncWithDefault = p.defaultValueMethod match {
              case Some(x) => q"$fieldEnc.withDefaultValue($x)"
              case None    => fieldEnc
            }
            q"private[this] val ${encName(p)} = $fieldEncWithDefault"
          }
          val fieldOutputFlags = nonBasicParams.map { p =>
            q"val ${encName(p, "o")} = ${encName(p)}.producesOutputFor(value.${p.name}) || { count -= 1; false }"
          }
          val writeEntries = params.map { p =>
            val key           = p.key()
            val writeKey      = q"w.${TermName(s"write${key.productPrefix}")}(${literal(key.value)})"
            val method        = TermName(s"write${p.basicTypeNameOrEmpty}")
            val rawWriteEntry = q"$writeKey.$method(value.${p.name})"
            if (p.isBasicType) rawWriteEntry
            else q"if (${encName(p, "o")}) $rawWriteEntry(${encName(p)})"
          }
          val encoderName = TypeName(s"${tpe.typeSymbol.name.decodedName}Encoder")

          q"""final class $encoderName {
                ..$fieldEncDefs
                def write(w: _root_.io.bullet.borer.Writer, value: $tpe): w.type = {
                  var count = ${params.size}
                  ..$fieldOutputFlags
                  def writeEntries(w: _root_.io.bullet.borer.Writer): w.type = {
                    ..$writeEntries
                    w
                  }
                  if (w.writingCbor) writeEntries(w.writeMapHeader(count))
                  else writeEntries(w.writeMapStart()).writeBreak()
                }
              }

              new _root_.io.bullet.borer.Encoder[$tpe] {
                private[this] var inner: $encoderName = _
                def write(w: _root_.io.bullet.borer.Writer, value: $tpe) = {
                  if (inner eq null) inner = new $encoderName
                  inner.write(w, value)
                }
              }: _root_.io.bullet.borer.Encoder[$tpe]"""
        }

        def deriveForSealedTrait(tpe: Type, subTypes: List[SubType]) = {
          val cases = adtSubtypeWritingCases(tpe, subTypes)
          q"""_root_.io.bullet.borer.Encoder { (w, value) =>
              def writeEntry(w: Writer): w.type = value match { case ..$cases }
              if (w.writingCbor) writeEntry(w.writeMapHeader(1))
              else writeEntry(w.writeMapStart()).writeBreak()
           }"""
        }
      }
    }

    def decoder[T: ctx.WeakTypeTag](ctx: blackbox.Context): ctx.Tree = DeriveWith[T](ctx) {
      new CodecDeriver[ctx.type](ctx) {
        import c.universe._

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

          def readField(p: CaseParam) = {
            val tpe = p.paramType.tpe
            if (isBasicType(tpe)) q"r.${TermName(s"read$tpe")}()" else q"r.read[$tpe]()(${decName(p)})"
          }

          val arity         = params.size
          val keysAndParams = new Array[(Key, CaseParam)](arity)
          params.foreach(p => keysAndParams(p.index) = p.key() -> p)
          val keysAndParamsSorted = keysAndParams.clone()
          java.util.Arrays.sort(keysAndParamsSorted.asInstanceOf[Array[Object]], KeyPairOrdering)

          val nonBasicParams = params.filterNot(_.isBasicType)
          val fieldDecDefs = nonBasicParams.map { p =>
            val paramType = p.paramType.tpe
            val fieldDec =
              Option(c.inferImplicitValue(toType(tq"_root_.io.bullet.borer.Decoder[$paramType]")))
                .filterNot(_.isEmpty)
                .getOrElse {
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
                    _root_.io.bullet.borer.derivation.internal.Helpers.failMissing(r, ${literal(typeName)},
                      ..$maskAsArgs, Array(..${params.map(p => literal(p.name.decodedName.toString))}))}"""
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
                def read(r: _root_.io.bullet.borer.Reader): $tpe = {
                  def failDuplicate(k: Any) =
                    throw new _root_.io.bullet.borer.Borer.Error.InvalidInputData(r.position,
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

              new _root_.io.bullet.borer.Decoder[$tpe] {
                private[this] var inner: $decoderName = _
                def read(r: _root_.io.bullet.borer.Reader): $tpe = {
                  if (inner eq null) inner = new $decoderName
                  inner.read(r)
                }
              }: _root_.io.bullet.borer.Decoder[$tpe]"""
        }

        def deriveForSealedTrait(tpe: Type, subTypes: List[SubType]) = {
          val typeIdsAndSubTypes: Array[(Key, SubType)] = getTypeIds(tpe, subTypes).zip(subTypes)
          java.util.Arrays.sort(typeIdsAndSubTypes.asInstanceOf[Array[Object]], KeyPairOrdering)

          def rec(start: Int, end: Int): Tree =
            if (start < end) {
              val mid           = (start + end) >> 1
              val (typeId, sub) = typeIdsAndSubTypes(mid)
              val cmp           = r("tryRead", typeId, "Compare")
              if (start < mid) {
                q"""val cmp = $cmp
                  if (cmp < 0) ${rec(start, mid)}
                  else if (cmp > 0) ${rec(mid + 1, end)}
                  else r.read[${sub.tpe}]()"""
              } else q"if ($cmp == 0) r.read[${sub.tpe}]() else fail()"
            } else q"fail()"

          def expected(s: String) = s"$s for decoding an instance of type `$tpe`"

          q"""_root_.io.bullet.borer.Decoder { r =>
                def fail() = r.unexpectedDataItem(${s"type id key for subtype of `$tpe`"})
                def readTypeIdAndValue(): $tpe = ${rec(0, typeIdsAndSubTypes.length)}

                if (r.tryReadMapStart()) {
                  val result = readTypeIdAndValue()
                  if (r.tryReadBreak()) result
                  else r.unexpectedDataItem(${expected("Single-entry Map")}, "at least one extra element")
                } else if (r.tryReadMapHeader(1)) {
                  readTypeIdAndValue()
                } else r.unexpectedDataItem(${expected("Single-entry Map")})
              }"""
        }
      }
    }

    def codec[T: c.WeakTypeTag](c: blackbox.Context): c.Tree = codecMacro(c)("MapBasedCodecs")
  }
}
