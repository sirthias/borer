/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.derivation.internal

import io.bullet.borer.{Decoder, Encoder}
import io.bullet.borer.deriver.DeriveWith

import scala.annotation.tailrec
import scala.reflect.macros.blackbox

private[derivation] object MacroSupport {

  sealed trait Key extends Product {
    def value: Any
  }

  object Key {
    final case class String(value: java.lang.String) extends Key
    final case class Long(value: scala.Long)         extends Key
  }

  val EncoderPlaceholder = Encoder[Any]((_, _) => sys.error("Internal Error: Unresolved Encoder Placeholder"))
  val DecoderPlaceholder = Decoder[Any](_ => sys.error("Internal Error: Unresolved Decoder Placeholder"))

  def sortAndVerifyNoCollisions[T](array: Array[(Key, T)])(onCollision: (Key, T, T) => Nothing): Unit = {
    def lessThan(comp: Int, k: Key, a: T, b: T): Boolean = if (comp == 0) onCollision(k, a, b) else comp < 0
    java.util.Arrays.sort(array, Ordering.fromLessThan[(Key, T)] {
      case ((k @ Key.Long(x), a), (Key.Long(y), b))     => lessThan(java.lang.Long.compare(x, y), k, a, b)
      case ((k @ Key.String(x), a), (Key.String(y), b)) => lessThan(x compare y, k, a, b)
      case ((x, _), _)                                  => x.isInstanceOf[Key.Long] // we sort LongKeys before StringKeys
    })
  }

  def codecMacro[T: c.WeakTypeTag](c: blackbox.Context)(objectName: String, de: String, dd: String): c.Tree = {
    import c.universe._
    val tpe      = weakTypeOf[T]
    val borerPkg = c.mirror.staticPackage("_root_.io.bullet.borer")
    val prefix   = q"$borerPkg.derivation.${TermName(objectName)}"
    val encName  = TermName(c.freshName("encoder"))
    val decName  = TermName(c.freshName("decoder"))
    q"""val $encName = $prefix.${TermName(de)}[$tpe]
        val $decName = $prefix.${TermName(dd)}[$tpe]
        $borerPkg.Codec($encName, $decName)"""
  }

  def deriveAll[T: ctx.WeakTypeTag](ctx: blackbox.Context)(
      isEncoder: Boolean,
      objectName: String,
      macroName: String,
      altMacroName: String): ctx.Tree =
    DeriveWith[T](ctx) {
      new CodecDeriver[ctx.type](ctx) {
        import c.universe._

        def deriveForCaseObject(tpe: Type, module: ModuleSymbol) =
          c.abort(
            c.enclosingPosition,
            s"The `$macroName` macro can only be used on sealed traits or sealed abstract " +
              s"classes, not on case objects. Use `$altMacroName` instead!")

        def deriveForCaseClass(
            tpe: Type,
            companion: ModuleSymbol,
            params: List[CaseParam],
            annotationTrees: List[Tree],
            constructorIsPrivate: Boolean) =
          c.abort(
            c.enclosingPosition,
            s"The `$macroName` macro can only be used on sealed traits or sealed abstract " +
              s"classes, not on case classes. Use `$altMacroName` instead!")

        def deriveForSealedTrait(node: AdtTypeNode) = {
          val altMacro     = q"$borerPkg.derivation.${TermName(objectName)}.${TermName(altMacroName)}"
          val tc           = if (isEncoder) encoderType else decoderType
          val relevantSubs = flattenedSubs(node, tc, deepRecurse = false).collect { case (node, false) => node }.toList

          // for each non-abstract sub-type S we collect the "links",
          // which are the ADT sub-types that appear somewhere within the member definitions of S and,
          // as such, need to have their typeclass be implicitly available _before_ the implicit derivation of S
          val relevantSubsWithLinks =
            relevantSubs.map { node =>
              node -> {
                if (!node.isAbstract) {
                  val memberTypes =
                    node.tpe.decls.collect {
                      case m: MethodSymbol if m.isCaseAccessor => m.asMethod.typeSignatureIn(node.tpe).resultType
                    }
                  relevantSubs.filter(x => memberTypes.exists(_.contains(x.tpe.typeSymbol)))
                } else Nil
              }
            }
          @tailrec def topoSort(
              remaining: List[(AdtTypeNode, List[AdtTypeNode])],
              blackSet: List[AdtTypeNode],
              result: List[AdtTypeNode]): List[AdtTypeNode] =
            remaining match {
              case (n, links) :: tail if links.forall(_ containedIn result) =>
                // we already have all links of the node in the result, to we can simply add the node itself
                topoSort(tail, blackSet, n :: result)
              case (n, links) :: _ =>
                // if the node has links that we haven't processed yet we need to pull these forward
                // but we need to be careful to detect unresolvable cycles, which we do via the "blackSet"
                if (!n.containedIn(blackSet)) {
                  val (toPullForward, otherRemaining) = remaining.partition(_._1 containedIn links)
                  topoSort(toPullForward ::: otherRemaining, n :: blackSet, result)
                } else {
                  c.abort(
                    c.enclosingPosition,
                    s"The ADT `${node.tpe}` contains a circular dependency involving `${n.tpe}` that you need to " +
                      s"break manually, e.g. by explicitly defining the implicit typeclass for `${n.tpe}`."
                  )
                }
              case Nil => result.reverse
            }

          val topologicallySortedSubs = topoSort(relevantSubsWithLinks, Nil, Nil)

          q"""{
            ..${topologicallySortedSubs.map(x => q"implicit val ${TermName(c.freshName())} = $altMacro[${x.tpe}]")}
            $altMacro[${node.tpe}]
          }"""
        }
      }
    }
}
