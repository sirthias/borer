/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.derivation.internal

import io.bullet.borer._
import io.bullet.borer.derivation.key

import scala.annotation.tailrec
import scala.reflect.macros.blackbox

abstract private[derivation] class CodecDeriver[C <: blackbox.Context](ctx: C) extends Deriver[C](ctx) {
  import c.universe._
  import MacroSupport._

  val borerPkg              = c.mirror.staticPackage("_root_.io.bullet.borer")
  lazy val encoderType      = symbolOf[Encoder[_]]
  lazy val encoderCompanion = encoderType.companion
  lazy val writerType       = symbolOf[Writer]
  lazy val writerCompanion  = writerType.companion
  lazy val decoderType      = symbolOf[Decoder[_]]
  lazy val decoderCompanion = decoderType.companion
  lazy val readerType       = symbolOf[Reader]
  lazy val readerCompanion  = readerType.companion

  def adtSubtypeWritingCases(tpe: Type, subTypes: List[SubType]): List[Tree] = {
    val exploded = flattenAbstracts(subTypes) { sub =>
      // if we cannot find an explicit encoder for an abstract subType we also take care of that subType's sub types
      inferImplicit(encoderType, sub.tpe).isEmpty
    }
    val typeIds = getTypeIds(tpe, exploded)
    exploded.zip(typeIds).map {
      case (subType, typeId) =>
        val writeTypeId = TermName(s"write${typeId.productPrefix}")
        cq"x: ${subType.tpe} => w.$writeTypeId(${literal(typeId.value)}).write(x)"
    }
  }

  def typeIdsAndSubTypesSorted(tpe: Type, subTypes: List[SubType]): Array[(Key, SubType)] = {
    val exploded = flattenAbstracts(subTypes) { sub =>
      // if we cannot find an explicit decoder for an abstract subType we also take care of that subType's sub types
      inferImplicit(decoderType, sub.tpe).isEmpty
    }
    val typeIdsAndSubTypes: Array[(Key, SubType)] = getTypeIds(tpe, exploded).zip(exploded)
    java.util.Arrays.sort(typeIdsAndSubTypes.asInstanceOf[Array[Object]], KeyPairOrdering)
    typeIdsAndSubTypes
  }

  def r(methodNamePrefix: String, key: Key, methodNameSuffix: String = "") = {
    val method = TermName(s"$methodNamePrefix${key.productPrefix}$methodNameSuffix")
    q"r.$method(${literal(key.value)})"
  }

  def getTypeIds(tpe: Type, subTypes: List[SubType]): Array[Key] = {
    val annos: Array[Key] = subTypes.map(_.key()).toArray

    @tailrec def rec(i: Int, j: Int): Array[Key] =
      if (i < annos.length) {
        if (j < annos.length) {
          if (i != j && annos(i) == annos(j)) {
            c.abort(
              tpe.typeSymbol.pos,
              s"@key collision: sub types `${subTypes(i).tpe}` and `${subTypes(j).tpe}` " +
                s"of ADT `$tpe` share the same type id `${annos(i).value}`")
          } else rec(i, j + 1)
        } else rec(i + 1, 0)
      } else annos
    rec(0, 0)
  }

  def isBasicType(tpe: Type): Boolean =
    tpe =:= typeOf[String] ||
      tpe =:= definitions.IntTpe ||
      tpe =:= definitions.LongTpe ||
      tpe =:= definitions.BooleanTpe ||
      tpe =:= definitions.DoubleTpe ||
      tpe =:= definitions.FloatTpe ||
      tpe =:= definitions.CharTpe ||
      tpe =:= definitions.ByteTpe ||
      tpe =:= definitions.ShortTpe

  def literal(value: Any) = Literal(Constant(value))

  def isDefinedOn(tree: Tree, symbol: Symbol): Boolean =
    tree match {
      case Select(x, _) => x.symbol == symbol
      case _            => false
    }

  def inferImplicit(typeClass: Symbol, tpe: Type): Option[Tree] = {
    val applied     = tq"$typeClass[$tpe]"
    val typeChecked = c.typecheck(applied, c.TYPEmode).tpe
    val tree        = c.inferImplicitValue(typeChecked)
    Option(tree).filterNot(_.isEmpty)
  }

  implicit class RichCaseParam(underlying: CaseParam) {
    def isBasicType: Boolean = CodecDeriver.this.isBasicType(underlying.paramType.tpe)

    def getImplicit(typeClass: Symbol): Option[Tree] = inferImplicit(typeClass, underlying.paramType.tpe)
  }

  implicit class RichWithAnnotations(underlying: WithAnnotations) {

    def key(): Key = {
      val keyAnnos = underlying.annotations
        .filter(_.tpe =:= typeOf[key])
        .flatMap(_.children.tail.collect {
          case Literal(Constant(x: String)) => Key.String(x)
          case Literal(Constant(x: Int))    => Key.Long(x.toLong)
          case Literal(Constant(x: Long))   => Key.Long(x)
          case x                            => c.abort(x.pos, s"The '@key' annotation only supports String or Int/Long literal arguments.")
        })
      keyAnnos.lengthCompare(1) match {
        case -1 => Key.String(underlying.name.decodedName.toString)
        case 0  => keyAnnos.head
        case 1  => c.abort(underlying.annotations.head.pos, s"Duplicate '@key' annotation")
      }
    }
  }
}
