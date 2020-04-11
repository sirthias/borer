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
import io.bullet.borer.deriver.Deriver

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
  lazy val adtEncoderType   = symbolOf[AdtEncoder[_]]
  lazy val adtDecoderType   = symbolOf[AdtDecoder[_]]
  lazy val helpers          = c.mirror.staticModule("_root_.io.bullet.borer.derivation.internal.Helpers")

  def deriveAdtEncoder(node: AdtTypeNode, withEnvelope: c.Tree => c.Tree): c.Tree = {
    import c.universe._
    val flattened             = flattenedSubs(node, decoderType, deepRecurse = false)
    val typeIdsAndNodesSorted = extractTypeIdsAndSort(node, flattened)
    val cases = typeIdsAndNodesSorted.toList.map {
      case (typeId, sub) => cq"x: ${sub.tpe} => writeAdtValue(w, ${literal(typeId.value)}, x)"
    }

    val writeAdtValueLong =
      if (typeIdsAndNodesSorted.exists(_._1.isInstanceOf[Key.Long])) {
        val T = TypeName(c.freshName("T"))
        q"""private def writeAdtValue[$T](w: $writerType, typeId: _root_.scala.Long, value: $T)(implicit enc: $encoderType[$T]) =
              enc match {
                case enc: $adtEncoderType[$T] => enc.write(w, value)
                case enc                      => ${withEnvelope(q"enc.write(w.writeLong(typeId), value)")}
              }"""
      } else q"()"

    val writeAdtValueString =
      if (typeIdsAndNodesSorted.exists(_._1.isInstanceOf[Key.String])) {
        val T = TypeName(c.freshName("T"))
        q"""private def writeAdtValue[$T](w: $writerType, typeId: _root_.java.lang.String, value: $T)(implicit enc: $encoderType[$T]) =
              enc match {
                case enc: $adtEncoderType[$T] => enc.write(w, value)
                case enc                      => ${withEnvelope(q"enc.write(w.writeString(typeId), value)")}
              }"""
      } else q"()"

    q"""new $adtEncoderType[${node.tpe}] {
          def write(w: $writerType, value: ${node.tpe}): $writerType = value match { case ..$cases }
          $writeAdtValueLong
          $writeAdtValueString
        }"""
  }

  def deriveAdtDecoder(node: AdtTypeNode, memberDefs: c.Tree, withEnvelope: c.Tree => c.Tree): c.Tree = {
    import c.universe._
    val flattened          = flattenedSubs(node, decoderType, deepRecurse = true)
    val abstracts          = flattened.collect { case (sub, _) if sub.isAbstract => sub }
    val typeIdsAndSubTypes = extractTypeIdsAndSort(node, flattened)

    def rec(array: Array[(Key, AdtTypeNode)], comp: Key => Tree)(start: Int = 0, end: Int = array.length): Tree =
      if (start < end) {
        val mid           = (start + end) >> 1
        val (typeId, sub) = array(mid)
        val readTpe       = sub.nodePath().find(abstracts.contains).map(_.tpe) getOrElse sub.tpe
        val typeIdLit     = literal(typeId.value)
        val cmp           = comp(typeId)
        if (start < mid) {
          q"""val cmp = $cmp
                  if (cmp < 0) ${rec(array, comp)(start, mid)}
                  else if (cmp > 0) ${rec(array, comp)(mid + 1, end)}
                  else $helpers.readAdtValue[$readTpe](r, $typeIdLit)"""
        } else q"if ($cmp == 0) $helpers.readAdtValue[$readTpe](r, $typeIdLit) else fail(r)"
      } else q"fail(r)"

    val readWithEnvelope = withEnvelope(rec(typeIdsAndSubTypes, r("tryRead", _, "Compare"))())

    val readWithLongTypeId = {
      val longTypeIdsAndSubTypes = typeIdsAndSubTypes.filter(_._1.isInstanceOf[Key.Long])
      rec(longTypeIdsAndSubTypes, tid => q"_root_.java.lang.Long.compare(typeId, ${literal(tid.value)})")()
    }
    val readWithStringTypeId = {
      val stringTypeIdsAndSubTypes = typeIdsAndSubTypes.filter(_._1.isInstanceOf[Key.String])
      rec(stringTypeIdsAndSubTypes, tid => q"typeId compare ${literal(tid.value)}")()
    }

    q"""new $adtDecoderType[${node.tpe}] {
          $memberDefs
          def read(r: $readerType): ${node.tpe} = $readWithEnvelope
          def read(r: $readerType, typeId: _root_.scala.Long): ${node.tpe} = $readWithLongTypeId
          def read(r: $readerType, typeId: _root_.java.lang.String): ${node.tpe} = $readWithStringTypeId
          private def fail(r: $readerType) = r.unexpectedDataItem(${s"type id key for subtype of `${node.tpe}`"})
        }"""
  }

  def extractTypeIdsAndSort(node: AdtTypeNode, flattened: Array[(AdtTypeNode, Boolean)]): Array[(Key, AdtTypeNode)] = {
    val result = flattened.map(x => x._1.key() -> x._1)
    sortAndVerifyNoCollisions(result) {
      case (k, a, b) =>
        c.abort(
          node.tpe.typeSymbol.pos,
          s"@key collision: sub types `${a.tpe}` and `${b.tpe}` of ADT `${node.tpe}` share the same type id `${k.value}`")
    }
    result
  }

  // returns all (recursively reachable, i.e. descendant) sub-types of `node` along with a flag showing
  // whether an instance of the given typeclass is implicitly available for the respective sub-type
  //
  // The `deepRecurse` flag determines, whether to recurse into abstract sub-types whose flag is
  // `true` (deepRecurse == true) or not (deepRecurse == false).
  //
  // Abstract sub-types whose flag is `true` are always returned
  // while abstract sub-types whose flag is `false` are never part of the result.
  def flattenedSubs(node: AdtTypeNode, typeClass: Symbol, deepRecurse: Boolean): Array[(AdtTypeNode, Boolean)] = {
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
                if (deepRecurse) head.subs ::: tail else tail
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
