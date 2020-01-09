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
import scala.collection.mutable.ArrayBuffer
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

  def typeIdsAndFlattenedSubsSorted(node: AdtTypeNode, typeClass: Symbol): List[(Key, AdtTypeNode)] = {
    val result = flattenedSubs(node, typeClass).map(x => x._1.key() -> x._1)

    @tailrec def verifyNoCollisions(i: Int, j: Int): Array[(Key, AdtTypeNode)] =
      if (i < result.length) {
        if (j < result.length) {
          if (i != j && result(i)._1 == result(j)._1) {
            c.abort(
              node.tpe.typeSymbol.pos,
              s"@key collision: sub types `${node.subs(i).tpe}` and `${node.subs(j).tpe}` " +
                s"of ADT `${node.tpe}` share the same type id `${result(i)._1.value}`")
          } else verifyNoCollisions(i, j + 1)
        } else verifyNoCollisions(i + 1, 0)
      } else result

    java.util.Arrays.sort(verifyNoCollisions(0, 0).asInstanceOf[Array[Object]], KeyPairOrdering)
    result.toList
  }

  // returns all (recursively reachable, i.e. descendant) sub-types of `node` along with a flag showing
  // whether an instance of the given typeclass is implicitly available for the respective sub-type
  // NOTE: Abstract sub-types whose flag is `true` are returned and not descended into,
  // Abstract sub-types whose flag would be `false` are skipped (and their descendants recursed into)
  def flattenedSubs(node: AdtTypeNode, typeClass: Symbol): Array[(AdtTypeNode, Boolean)] = {
    val buf = new ArrayBuffer[(AdtTypeNode, Boolean)]
    @tailrec def rec(remaining: List[AdtTypeNode]): Array[(AdtTypeNode, Boolean)] =
      remaining match {
        case head :: tail =>
          val implicitAvailable = inferImplicit(typeClass, head.tpe).nonEmpty
          def appendHead()      = if (!buf.exists(_._1.tpe =:= head.tpe)) buf += head -> implicitAvailable
          rec {
            if (head.isAbstract) {
              if (implicitAvailable) {
                appendHead()
                tail
              } else {
                // if we cannot find an explicit encoder/decoder for an abstract sub we flatten that sub's subs
                head.subs ::: tail
              }
            } else {
              appendHead()
              tail
            }
          }
        case Nil => buf.toArray
      }
    rec(node.subs)
  }

  def adtSubtypeWritingCases(node: AdtTypeNode): List[Tree] =
    typeIdsAndFlattenedSubsSorted(node, encoderType).map {
      case (typeId, sub) =>
        val writeTypeId = TermName(s"write${typeId.productPrefix}")
        cq"x: ${sub.tpe} => w.$writeTypeId(${literal(typeId.value)}).write(x)"
    }

  def r(methodNamePrefix: String, key: Key, methodNameSuffix: String = "") = {
    val method = TermName(s"$methodNamePrefix${key.productPrefix}$methodNameSuffix")
    q"r.$method(${literal(key.value)})"
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

  def isDefinedOn(tree: Tree, symbol: Symbol): Boolean =
    tree match {
      case Select(x, _) => x.symbol == symbol
      case _            => false
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
