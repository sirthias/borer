/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.derivation.internal

import scala.collection.mutable.ListBuffer
import scala.reflect.macros.blackbox
import scala.reflect.macros.contexts.Context

private[derivation] object DeriveWith {

  def apply[T: c.WeakTypeTag](c: blackbox.Context)(deriver: Deriver[c.type]): c.Tree =
    deriver.deriveFor(c.universe.weakTypeOf[T].dealias)
}

abstract private[derivation] class Deriver[C <: blackbox.Context](val c: C) {
  import c.universe._

  sealed trait CaseParamType {
    def tpe: Type
  }
  case class SameAs(earlierParam: CaseParam) extends CaseParamType { def tpe = earlierParam.paramType.tpe }
  case class OwnType(tpe: Type)              extends CaseParamType

  sealed trait WithAnnotations {
    def annotations: List[Tree]
    def name: Name
  }
  case class CaseParam(
      symbol: MethodSymbol,
      index: Int,
      paramType: CaseParamType,
      defaultValueMethod: Option[Tree],
      annotations: List[Tree],
      repeated: Boolean)
      extends WithAnnotations {
    def name: TermName = symbol.asTerm.name
  }

  case class SubType(tpe: Type, index: Int, annotations: List[Tree]) extends WithAnnotations {
    def name: TypeName = tpe.typeSymbol.asType.name
  }

  def deriveForCaseObject(tpe: Type, module: ModuleSymbol): Tree

  def deriveForCaseClass(
      tpe: Type,
      companion: ModuleSymbol,
      params: List[CaseParam],
      annotations: List[Tree],
      constructorIsPrivate: Boolean): Tree

  def deriveForSealedTrait(tpe: Type, subTypes: List[SubType]): Tree

  final def deriveFor(tpe: c.Type): Tree = {
    val debug = c.macroApplication.symbol.annotations
      .find(_.tree.tpe <:< typeOf[debug])
      .flatMap(_.tree.children.tail.collectFirst { case Literal(Constant(s: String)) => s })

    val typeSymbol = tpe.typeSymbol
    val classType  = if (typeSymbol.isClass) Some(typeSymbol.asClass) else None
    val result = classType match {
      case Some(x) if x.isModuleClass => deriveForCaseObject(tpe, typeSymbol.asModule)
      case Some(x) if x.isCaseClass   => forCaseClass(tpe, x)
      case Some(x) if x.isSealed      => forSealedTrait(tpe, x)
      case None                       => error(s"`$classType` is not a case class or sealed abstract data type")
    }

    if (debug.isDefined && tpe.toString.contains(debug.get)) {
      c.echo(c.enclosingPosition, s"Derivation macro expansion for `$tpe`")
      c.echo(NoPosition, s"... = ${showCode(result)}\n\n")
    }

    result
  }

  private def forCaseClass(tpe: Type, typeSymbol: ClassSymbol): Tree = {
    val companion          = findCompanion(tpe).asModule
    val primaryConstructor = typeSymbol.primaryConstructor
    val headParamList = primaryConstructor.asMethod.typeSignature.paramLists.headOption
      .getOrElse(error(s"Could not get parameter list of primary constructor of `$tpe`"))
      .map(_.asTerm)
    val caseParamsReversed = {
      val repeatedParamClass = definitions.RepeatedParamClass
      val scalaSeqType       = typeOf[Seq[_]].typeConstructor
      val caseParamMethods   = tpe.decls.collect { case m: MethodSymbol if m.isCaseAccessor => m.asMethod }
      if (caseParamMethods.size != headParamList.size)
        error(s"Could not properly determine case class parameters of `$tpe`")
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
      caseParamMethods.foldLeft(List.empty[CaseParam]) {
        case (acc, paramSymbol) =>
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

  private def forSealedTrait(tpe: Type, typeSymbol: ClassSymbol): Tree = {
    def knownSubclasses(sym: ClassSymbol): List[Symbol] = {
      val children                       = sym.knownDirectSubclasses.toList
      val (abstractTypes, concreteTypes) = children.partition(_.isAbstract)
      abstractTypes.flatMap(x => knownSubclasses(x.asClass)) ::: concreteTypes
    }
    val subtypes = new ListBuffer[SubType]
    val index    = Iterator.from(0)
    knownSubclasses(typeSymbol).foreach { sub =>
      val subType     = sub.asType.toType // FIXME: Broken for path dependent types
      val typeParams  = sub.asType.typeParams
      val typeArgs    = c.internal.thisType(sub).baseType(typeSymbol).typeArgs
      val mapping     = typeArgs.map(_.typeSymbol).zip(tpe.typeArgs).toMap
      val newTypeArgs = typeParams.map(mapping.withDefault(_.asType.toType))
      val applied     = appliedType(subType.typeConstructor, newTypeArgs)
      val theType     = c.internal.existentialAbstraction(typeParams, applied)
      if (subtypes.forall(_.tpe != theType)) {
        subtypes += SubType(theType, index.next(), annotationTrees(theType.typeSymbol))
      }
    }

    if (subtypes.isEmpty) error(s"Could not find any direct subtypes of `$typeSymbol`")

    deriveForSealedTrait(tpe: Type, subtypes.toList)
  }

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
        .mkAttributedRef(globalType.prefix, original.companion.orElse {
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
}
