/*
 * Copyright (c) 2019-2021 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.deriver

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.reflect.macros.blackbox
import scala.reflect.macros.contexts.Context

object DeriveWith {

  def apply[T: c.WeakTypeTag](c: blackbox.Context)(deriver: Deriver[c.type]): c.Tree =
    deriver.deriveFor(c.universe.weakTypeOf[T].dealias)
}

/**
  * Heavily inspired by and some parts actually copied from Magnolia (https://github.com/propensive/magnolia),
  * which is Copyright 2018 Jon Pretty, Propensive Ltd. and licensed under the Apache License, Version 2.0.
  */
abstract class Deriver[C <: blackbox.Context](val c: C) {
  import c.universe._

  sealed protected trait CaseParamType {
    def tpe: Type
  }
  protected case class SameAs(earlierParam: CaseParam) extends CaseParamType { def tpe = earlierParam.paramType.tpe }
  protected case class OwnType(tpe: Type)              extends CaseParamType

  sealed abstract protected class WithAnnotations {
    def annotations: List[Tree]
    def name: Name
    lazy val stringName: String = name.decodedName.toString
  }

  protected case class CaseParam(
      symbol: MethodSymbol,
      index: Int,
      paramType: CaseParamType,
      defaultValueMethod: Option[Tree],
      annotations: List[Tree],
      repeated: Boolean)
      extends WithAnnotations {
    def name: TermName = symbol.asTerm.name
  }

  protected case class AdtTypeNode(tpe: Type, subs: List[AdtTypeNode]) extends WithAnnotations {
    def name: TypeName                                = tpe.typeSymbol.asType.name
    def isAbstract: Boolean                           = tpe.typeSymbol.isAbstract
    def annotations: List[Tree]                       = annotationTrees(tpe.typeSymbol)
    def containedIn(list: List[AdtTypeNode]): Boolean = list.exists(_ eq this)

    def allNodes: List[AdtTypeNode]                  = this :: subs.flatMap(_.allNodes)
    def allDescendants: List[AdtTypeNode]            = allNodes.tail
    def allNonAbstractDescendants: List[AdtTypeNode] = allDescendants.filterNot(_.isAbstract)

    def isRoot: Boolean = parent.isEmpty

    def nodePath(suffix: List[AdtTypeNode] = Nil): List[AdtTypeNode] =
      if (isRoot) suffix else parent.get.nodePath(this :: suffix)

    /////////////////// internal ////////////////////

    // write-once, before actually making it to the user
    private[this] var parent: Option[AdtTypeNode] = _

    private[Deriver] def initAllParentBacklinks(p: Option[AdtTypeNode]): Unit = {
      if (parent ne null) throw new IllegalStateException
      parent = p
      val someThis = Some(this)
      subs.foreach(_.initAllParentBacklinks(someThis))
    }
  }

  protected def deriveForCaseObject(tpe: Type, module: ModuleSymbol): Tree

  protected def deriveForCaseClass(
      tpe: Type,
      companion: ModuleSymbol,
      params: List[CaseParam],
      annotations: List[Tree],
      constructorIsPrivate: Boolean): Tree

  protected def deriveForSealedTrait(node: AdtTypeNode): Tree

  final def deriveFor(tpe: c.Type): Tree = {
    val typeSymbol = tpe.typeSymbol
    val classType  = if (typeSymbol.isClass) Some(typeSymbol.asClass) else None
    val result = classType match {
      case Some(x) if x.isModuleClass => deriveForCaseObject(tpe, x.module.asModule)
      case Some(x) if x.isCaseClass   => forCaseClass(tpe, x)
      case Some(x) if x.isSealed =>
        val node = adtTypeNode(tpe)
        node.initAllParentBacklinks(None)
        if (node.subs.isEmpty) error(s"Could not find any direct subtypes of `$typeSymbol`")
        deriveForSealedTrait(node)
      case _ =>
        val mac = c.enclosingMacros.headOption.fold("")(x =>
          s" (macro application `${x.universe.showCode(x.macroApplication)}`)")
        error(s"`$tpe` is not a case class or sealed abstract data type$mac")
    }
    result
  }

  final protected def literal(value: Any) = Literal(Constant(value))

  final protected def inferImplicit(typeClass: Symbol, tpe: Type): Option[Tree] = {
    val applied     = tq"$typeClass[$tpe]"
    val typeChecked = c.typecheck(applied, c.TYPEmode).tpe
    val tree        = c.inferImplicitValue(typeChecked)
    Option(tree).filterNot(_.isEmpty)
  }

  /**
    * When generating type class instances for a whole type hierarchy this method returns all descendant nodes of
    * an ADT super type that don't have implicit type class instances already available.
    *
    * The resulting list is already ordered in a way that allows intra-dependencies (i.e. between sub types) be resolved
    * properly if the missing implicits are generated in order.
    * (If there are circular dependencies a compiler error is automatically thrown.)
    */
  final protected def subsWithoutImplicitTypeclassInstances(
      node: AdtTypeNode,
      typeclass: TypeSymbol): List[AdtTypeNode] = {
    val relevantSubs = flattenedSubs(node, typeclass, deepRecurse = false).collect { case (node, false) => node }.toList

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

    topoSort(relevantSubsWithLinks, Nil, Nil)
  }

  // returns all (recursively reachable, i.e. descendant) sub-types of `node` along with a flag showing
  // whether an instance of the given typeclass is implicitly available for the respective sub-type
  //
  // The `deepRecurse` flag determines, whether to recurse into abstract sub-types whose flag is
  // `true` (deepRecurse == true) or not (deepRecurse == false).
  //
  // Abstract sub-types whose flag is `true` are always returned (if reached during the tree walk)
  // while abstract sub-types whose flag is `false` are never part of the result.
  protected def flattenedSubs(
      node: AdtTypeNode,
      typeClass: TypeSymbol,
      deepRecurse: Boolean): Array[(AdtTypeNode, Boolean)] = {
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
                // if we cannot find an implicit typeclass instance for an abstract sub we flatten that sub's subs
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

  private def forCaseClass(tpe: Type, typeSymbol: ClassSymbol): Tree = {
    val companion          = findCompanion(tpe).asModule
    val primaryConstructor = typeSymbol.primaryConstructor
    val headParamList = primaryConstructor.asMethod.typeSignature.paramLists.headOption
      .getOrElse(error(s"Cannot get parameter list of primary constructor of `$tpe`"))
      .map(_.asTerm)
    val caseParamsReversed = {
      val repeatedParamClass = definitions.RepeatedParamClass
      val scalaSeqType       = typeOf[Seq[_]].typeConstructor
      val caseParamMethods   = tpe.decls.collect { case m: MethodSymbol if m.isCaseAccessor => m.asMethod }
      if (caseParamMethods.size != headParamList.size)
        error(s"Cannot properly determine case class parameters of `$tpe`")
      val defaults: Iterator[Option[Tree]] = {
        val companionSym         = companion.info
        val primaryFactoryMethod = companionSym.decl(TermName("apply")).alternatives.lastOption
        // cause the namer/typer to generate the synthetic default methods
        primaryFactoryMethod.foreach(_.asMethod.typeSignature)
        headParamList.iterator.zipWithIndex.map {
          case (p, i) if p.isParamWithDefault => Some(q"$companion.${TermName("apply$default$" + (i + 1))}")
          case _                              => None
        }
      }
      val annotations: Iterator[List[Tree]] = headParamList.iterator.map(annotationTrees)
      val indices                           = Iterator.from(0)
      caseParamMethods.foldLeft(List.empty[CaseParam]) { case (acc, paramSymbol) =>
        val (repeated, paramType) = paramSymbol.typeSignatureIn(tpe).resultType match {
          case TypeRef(_, `repeatedParamClass`, typeArgs) => true  -> appliedType(scalaSeqType, typeArgs)
          case x                                          => false -> x
        }
        val caseParamType = acc.find(_.paramType.tpe =:= paramType) match {
          case None    => OwnType(paramType)
          case Some(x) => SameAs(x)
        }
        CaseParam(paramSymbol, indices.next(), caseParamType, defaults.next(), annotations.next(), repeated) :: acc
      }
    }

    deriveForCaseClass(
      tpe = tpe,
      companion = companion,
      params = caseParamsReversed.reverse,
      annotations = annotationTrees(typeSymbol),
      constructorIsPrivate = primaryConstructor.isPrivate)
  }

  private def adtTypeNode(tpe: Type): AdtTypeNode =
    AdtTypeNode(
      tpe = tpe,
      subs = tpe.typeSymbol.asClass.knownDirectSubclasses
        .map { sub =>
          val subType     = sub.asType.toType // FIXME: Broken for path dependent types
          val typeParams  = sub.asType.typeParams
          val typeArgs    = c.internal.thisType(sub).baseType(tpe.typeSymbol).typeArgs
          val mapping     = typeArgs.map(_.typeSymbol).zip(tpe.typeArgs).toMap
          val newTypeArgs = typeParams.map(mapping.withDefault(_.asType.toType))
          val applied     = appliedType(subType.typeConstructor, newTypeArgs)
          c.internal.existentialAbstraction(typeParams, applied)
        }
        .iterator
        .map(adtTypeNode)
        .toList
    )

  private def annotationTrees(symbol: Symbol) = {
    val javaAnnotationType                = typeOf[java.lang.annotation.Annotation]
    val isJavaAnnotation: Tree => Boolean = _.tpe <:< javaAnnotationType
    symbol.info // enforce type information completeness and availability of annotations
    symbol.annotations.map(_.tree).filterNot(isJavaAnnotation)
  }

  private def findCompanion(tpe: Type): Symbol = {
    // Borrowed and refactored from Chimney: https://github.com/scalalandio/chimney/blob/master/chimney/src/main/scala/io/scalaland/chimney/internal/CompanionUtils.scala#L10-L63
    // Copied from Magnolia: https://github.com/propensive/magnolia/blob/master/core/shared/src/main/scala/globalutil.scala
    // From Shapeless: https://github.com/milessabin/shapeless/blob/master/core/src/main/scala/shapeless/generic.scala#L698
    // Cut-n-pasted (with most original comments) and slightly adapted from https://github.com/scalamacros/paradise/blob/c14c634923313dd03f4f483be3d7782a9b56de0e/plugin/src/main/scala/org/scalamacros/paradise/typechecker/Namers.scala#L568-L613
    def patchedCompanionRef(tpe: Type): Tree = {
      val global     = c.universe.asInstanceOf[scala.tools.nsc.Global]
      val globalType = tpe.asInstanceOf[global.Type]
      val original   = globalType.typeSymbol
      global.gen
        .mkAttributedRef(
          globalType.prefix,
          original.companion.orElse {
            import global.{error => _, _}
            val name          = original.name.companionName
            val expectedOwner = original.owner
            var ctx           = c.asInstanceOf[Context].callsiteTyper.asInstanceOf[global.analyzer.Typer].context
            var res: Symbol   = NoSymbol
            while (res == NoSymbol && ctx.outer != ctx) {
              // NOTE: original implementation says `val s = ctx.scope lookup name`
              // but we can't use it, because Scope.lookup returns wrong results when the lookup is ambiguous
              // and that triggers https://github.com/scalamacros/paradise/issues/64
              val s = ctx.scope
                .lookupAll(name)
                .filter(sym => (original.isTerm || sym.hasModuleFlag) && sym.isCoDefinedWith(original))
                .toList match {
                case Nil           => NoSymbol
                case unique :: Nil => unique
                case _             => error(s"Unexpected multiple results for a companion symbol lookup for $original")
              }
              if (s != NoSymbol && s.owner == expectedOwner) res = s
              else ctx = ctx.outer
            }
            res
          })
        .asInstanceOf[Tree]
    }

    val comp = tpe.typeSymbol.companion
    if (comp.isModule) comp
    else patchedCompanionRef(tpe).symbol
  }

  protected def error(msg: String) = c.abort(c.enclosingPosition, msg)

  protected def showResult(pos: Position, tree: Tree): Tree = {
    c.echo(pos, showCode(tree))
    tree
  }
}
