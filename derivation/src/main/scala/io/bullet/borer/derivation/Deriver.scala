/*
 * Copyright (c) 2019-2024 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.derivation

import scala.language.implicitConversions
import io.bullet.borer.*

import scala.annotation.threadUnsafe
import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.deriving.*
import scala.compiletime.*
import scala.quoted.*
import scala.reflect.ClassTag

object Derive {
  def apply[F[_], T: Type](deriver: Deriver[F, T, ?]): Expr[F[T]] = deriver.derive

  def deriveAll[F[_]: Type, T: Type](macroName: String, altMacroName: String)(macroCall: MacroCall[F])(
      using Quotes): Expr[F[T]] =
    Derive[F, T] {
      import quotes.reflect.*

      new Deriver[F, T, quotes.type]:

        private val msg =
          s"The `$macroName` macro can only be used on sealed traits or sealed abstract " +
            s"classes, not on case objects. Use `$altMacroName` instead!"

        def deriveForCaseObject(moduleTermSymbol: Symbol): Expr[F[T]] = fail(msg)

        def deriveForCaseClass(tpe: TypeRepr, classSym: Symbol, companion: Symbol, fields: IArray[Field]): Expr[F[T]] =
          fail(msg)

        def deriveForSealedTrait(rootNode: AdtTypeNode): Expr[F[T]] =
          val (regularSubtypes, cycleBreakers) = subtypesNeedingTypeclassDerivation(rootNode)

          // first, we define mutable variables for all cycle breaker types
          withVars {
            IArray.from {
              cycleBreakers.map {
                _.asType match
                  case '[t] => Val.of[F[t]]('{ null.asInstanceOf[F[t]] })
              }
            }
          } { cycleBreakerVars =>
            // next, we define simple forwarder givens for all cycle breaker types, pointing to the variables
            renderEach[Var, Unit, F[T]](cycleBreakerVars)(_.tpe) {
              [FX] => (v: Var) => tail => '{ given FX = ${ v.as[FX].get }; ${ tail(()) } }
            } { _ =>
              // next, we define the givens for all "regular" sub types
              renderEach[Type[?], Unit, F[T]](IArray.from(regularSubtypes.map(_.asType)))(identity) {
                [X] => (_: Type[?]) => tail => '{ given F[X] = ${ macroCall[X] }; ${ tail(()) } }
              } { _ =>
                // then, we give the cycle breaker variables their actual contents
                val tuples = IArray.from(cycleBreakers.map(_.asType)).zip(cycleBreakerVars)
                renderEach[(Type[?], Var), Unit, F[T]](tuples)(_._1) {
                  [X] => (tup: (Type[?], Var)) => tail => '{ ${ tup._2.as[F[X]].set(macroCall[X]) } ; ${ tail(()) } }
                } { _ =>
                  // finally we can call the generation macro for the ADT root type
                  macroCall[T]
                }
              }
            }
          }
    }

  trait MacroCall[F[_]: Type] {
    def apply[A: Type](using Quotes): Expr[F[A]]
  }
}

abstract private[derivation] class Deriver[F[_]: Type, T: Type, Q <: Quotes](using val q: Q) {
  import q.reflect.*

  val theT: TypeRepr    = TypeRepr.of[T]
  val keyAnno: TypeRepr = TypeRepr.of[io.bullet.borer.derivation.key]
  val theTname: String  = theT.show(using Printer.TypeReprShortCode)

  sealed trait FieldType {
    def tpe: TypeRepr
  }
  case class SameAs(other: Field)   extends FieldType { def tpe = other.fieldType.tpe }
  case class OwnType(tpe: TypeRepr) extends FieldType

  sealed abstract class WithAnnotations {
    def name: String
    def annotations: List[Term]

    lazy val key: String | Long = {
      val keys: List[String | Long] = annotations.filter(_.tpe <:< keyAnno).flatMap {
        case Apply(_, List(Literal(StringConstant(x)))) => Some(x)
        case Apply(_, List(Literal(LongConstant(x))))   => Some(x)
        case Apply(_, List(Literal(IntConstant(x))))    => Some(x.toLong)
        case x =>
          report.errorAndAbort("The '@key' annotation only supports String or Int/Long literal arguments.", x.pos)
      }
      keys.lengthCompare(1) match {
        case -1 => name
        case 0  => keys.head
        case 1  => report.errorAndAbort("Duplicate '@key' annotation", annotations.head.pos)
      }
    }
  }

  case class Field(
      symbol: Symbol,
      index: Int,
      fieldType: FieldType,
      defaultValueMethod: Option[Term],
      annotations: List[Term])
      extends WithAnnotations {

    def name: String = symbol.name

    def defaultValue[A: Type]: Option[Expr[A]] = defaultValueMethod.map(_.asExprOf[A])
    def hasDefaultValue: Boolean               = defaultValueMethod.isDefined

    def theType: Type[?] = fieldType.tpe.asType

    def ownTypeIndex: Int =
      fieldType match
        case OwnType(_)    => index
        case SameAs(other) => other.ownTypeIndex

    def isBasicType: Boolean =
      theType match {
        case '[String] | '[Int] | '[Long] | '[Boolean] | '[Double] | '[Float] | '[Char] | '[Byte] | '[Short] => true
        case _                                                                                               => false
      }
  }

  case class AdtTypeNode(tpe: TypeRepr, subs: List[AdtTypeNode]) extends WithAnnotations {
    @threadUnsafe lazy val name: String = if (tpe.isSingleton) tpe.termSymbol.name else tpe.typeSymbol.name
    def annotations: List[Term]                       = tpe.typeSymbol.annotations
    def containedIn(list: List[AdtTypeNode]): Boolean = list.exists(_ eq this)
    def isEnumSingletonCase: Boolean = tpe.isSingleton && parent.exists(_.tpe.typeSymbol.flags.is(Flags.Enum))
    def isAbstract: Boolean =
      val flags = tpe.typeSymbol.flags
      flags.is(Flags.Trait) || flags.is(Flags.Abstract) || flags.is(Flags.JavaDefined)

    def isRoot: Boolean = parent.isEmpty

    def nodePath(suffix: List[AdtTypeNode] = Nil): List[AdtTypeNode] =
      if (isRoot) suffix else parent.get.nodePath(this :: suffix)

    def enumRef: Expr[T] =
      val companion = parent.getOrElse(fail("Illegal state in macro logic")).tpe.typeSymbol.companionModule
      Select.unique(Ref(companion), name).asExprOf[T]

    def allSubs: Iterator[AdtTypeNode] = Iterator.single(this) ++ subs.flatMap(_.allSubs)

    /////////////////// internal ////////////////////

    // write-once, before actually making it to the user
    private var parent: Option[AdtTypeNode] = _

    private[Deriver] def initAllParentBacklinks(p: Option[AdtTypeNode]): this.type = {
      if (parent ne null) throw new IllegalStateException
      parent = p
      val someThis = Some(this)
      subs.foreach(_.initAllParentBacklinks(someThis))
      this
    }
  }

  def deriveForCaseObject(moduleTermSymbol: Symbol): Expr[F[T]]

  def deriveForCaseClass(tpe: TypeRepr, classSym: Symbol, companion: Symbol, fields: IArray[Field]): Expr[F[T]]

  def deriveForSealedTrait(rootNode: AdtTypeNode): Expr[F[T]]

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  final def derive: Expr[F[T]] =
    val widened = theT.dealias.widen
    val flags   = widened.typeSymbol.flags
    widened match {
      case x if flags.is(Flags.Module) && flags.is(Flags.Case) =>
        deriveForCaseObject(if (x.isSingleton) x.termSymbol else x.typeSymbol.companionModule)
      case x if flags.is(Flags.Case) =>
        forCaseClass(x, x.classSymbol.getOrElse(fail(s"Cannot get classSymbol for `$theTname`")))
      case x if flags.is(Flags.Sealed) && (flags.is(Flags.Trait) || flags.is(Flags.Abstract)) =>
        def adtNode(tpe: TypeRepr): AdtTypeNode = AdtTypeNode(tpe, adtChildren(tpe).map(adtNode))
        deriveForSealedTrait(adtNode(x).initAllParentBacklinks(None))
      case _ =>
        fail(
          s"Cannot derive ${Type.show[F[T]]}. Macro-based derivation only works for case objects, " +
            s"case classes, sealed traits and sealed abstract classes.")
    }

  private def forCaseClass(tpe: TypeRepr, classSym: Symbol): Expr[F[T]] = {
    val companion = classSym.companionModule

    def defaultValue(fieldSym: Symbol, ix: Int): Option[Term] =
      if (fieldSym.flags.is(Flags.HasDefault)) {
        tpe.typeSymbol.companionClass
          .methodMember(s"$$lessinit$$greater$$default$$${ix + 1}")
          .headOption
          .orElse(fail(s"Cannot find default value for $fieldSym in `$theTname`"))
          .map { methodSym =>
            val selected = Ref(companion.companionModule).select(methodSym)
            methodSym.paramSymss match
              case Nil => selected
              case List(params) if params.exists(_.isTypeParam) =>
                typeArgs(tpe) match
                  case Nil  => fail(s"Expected `$theTname` to be an applied type")
                  case args => TypeApply(selected, args.map(Inferred(_)))
              case _ =>
                fail(
                  s"Param list of default method for field `${fieldSym.name}` of class `$theTname` is too complex: ${methodSym.paramSymss}")
          }
      } else None

    def caseParamType(buf: collection.Seq[Field], fieldSym: Symbol): FieldType = {
      val fieldType = tpe.memberType(fieldSym).dealias
      buf.find(_.fieldType.tpe =:= fieldType) match {
        case None    => OwnType(fieldType)
        case Some(x) => SameAs(x)
      }
    }

    val fieldsBuf =
      classSym.caseFields.iterator.zipWithIndex.foldLeft(new ArrayBuffer[Field]) { case (b, (fieldSym, ix)) =>
        b += Field(fieldSym, ix, caseParamType(b, fieldSym), defaultValue(fieldSym, ix), fieldSym.annotations)
      }
    deriveForCaseClass(tpe, classSym, companion, IArray.unsafeFromArray(fieldsBuf.toArray))
  }

  final def deriveAdtEncoderBody(
      rootNode: AdtTypeNode,
      self: Expr[DerivedAdtEncoder[T]],
      w: Expr[Writer],
      value: Expr[T])(using Quotes): Expr[Writer] = {
    val flattened             = flattenedSubs[Encoder](rootNode, deepRecurse = false, includeEnumSingletonCases = true)
    val typeIdsAndNodesSorted = extractTypeIdsAndSort(rootNode, flattened)

    def writeEnumSingletonTypeId(typeId: Long | String): Expr[Writer] =
      typeId match
        case x: Long   => '{ $w.writeLong(${ Expr(x) }) }
        case x: String => '{ $w.writeString(${ Expr(x) }) }

    def writeAdtValue[A: Type](
        typeId: Long | String,
        valueAsA: Expr[A],
        encA: Expr[Encoder[A]]): Expr[Writer] =
      typeId match
        case x: Long   => '{ $self.writeAdtValue[A]($w, ${ Expr(x) }, $valueAsA)(using $encA) }
        case x: String => '{ $self.writeAdtValue[A]($w, ${ Expr(x) }, $valueAsA)(using $encA) }

    def rec(ix: Int): Expr[Writer] =
      if (ix < typeIdsAndNodesSorted.length) {
        val (typeId, sub) = typeIdsAndNodesSorted(ix)
        if (sub.isEnumSingletonCase) {
          val enumRef = sub.enumRef.asExprOf[AnyRef]
          lazy val writeTypeId = writeEnumSingletonTypeId(typeId)
          lazy val writeAsProduct = sub.tpe.asType match
            case '[a] =>
              val valueAsA = '{ $value.asInstanceOf[a] }
              val encA = '{
                new Encoder[a] {
                  def write(w: Writer, value: a): Writer = {
                    w.writeMapStart()
                    w.writeBreak()
                  }
                }
              }
              writeAdtValue[a](typeId, valueAsA, encA)

          '{
            if ($value.asInstanceOf[AnyRef] eq $enumRef)
              if ($self.enumCasesAsProduct) $writeAsProduct else $writeTypeId
            else
              ${ rec(ix + 1) }
          }
        } else {
          val testType = sub.tpe match
            case AppliedType(x, _) => x
            case x                 => x
          sub.tpe.asType match {
            case '[a] =>
              val valueAsA = '{ $value.asInstanceOf[a] }
              val encA     = Expr.summon[Encoder[a]].getOrElse(fail(s"Cannot find given Encoder[${Type.show[a]}]"))
              val writeKeyed = writeAdtValue[a](typeId, valueAsA, encA)
              testType.asType match
                case '[b] => '{ if ($value.isInstanceOf[b]) $writeKeyed else ${ rec(ix + 1) } }
          }
        }
      } else '{ throw new MatchError($value) }

    rec(0)
  }

  type TypeIdsAndSubTypes = Array[(Long | String, AdtTypeNode)]

  final def sealedTraitMethodBody(rootNode: AdtTypeNode): SealedTraitMethodBody =
    new SealedTraitMethodBody {
      private val flattened = flattenedSubs[Decoder](rootNode, deepRecurse = true, includeEnumSingletonCases = true)
      private val abstracts = flattened.collect { case (sub, _) if sub.isAbstract => sub }
      private val typeIdsAndSubTypes = extractTypeIdsAndSort(rootNode, flattened)

      private def rec(
          self: Expr[DerivedAdtDecoder[T]],
          r: Expr[Reader],
          array: TypeIdsAndSubTypes,
          comp: Long | String => Expr[Int])(start: Int = 0, end: Int = array.length)(using Quotes): Expr[T] =
        if (start < end) {
          val mid           = (start + end) >> 1
          val (typeId, sub) = array(mid)
          val cmp           = comp(typeId)
          val readAdtVal =
            if (sub.isEnumSingletonCase) sub.enumRef
            else {
              val readTpe = sub.nodePath().find(abstracts.contains).map(_.tpe) getOrElse sub.tpe
              readTpe.asType match {
                case '[a] =>
                  val decA = Expr
                    .summon[Decoder[a]]
                    .getOrElse(fail(s"Cannot find given Decoder[${Type.show[a]}]")) // TODO: cache!!!
                  typeId match
                    case x: Long   => '{ helpers.readAdtValue[T, a]($r, ${ Expr(x) }, $decA) }
                    case x: String => '{ helpers.readAdtValue[T, a]($r, ${ Expr(x) }, $decA) }
              }
            }
          if (start < mid)
            '{
              val c = $cmp
              if (c < 0) ${ rec(self, r, array, comp)(start, mid) }
              else if (c > 0) ${ rec(self, r, array, comp)(mid + 1, end) }
              else $readAdtVal
            }
          else '{ if ($cmp == 0) $readAdtVal else $self.failExpectedTypeId($r) }
        } else '{ $self.failExpectedTypeId($r) }

      def derive(
          self: Expr[DerivedAdtDecoder[T]],
          r: Expr[Reader],
          filter: TypeIdsAndSubTypes => TypeIdsAndSubTypes,
          comp: Long | String => Expr[Int])(using Quotes): Expr[T] = rec(self, r, filter(typeIdsAndSubTypes), comp)()
    }

  trait SealedTraitMethodBody {
    def derive(
        self: Expr[DerivedAdtDecoder[T]],
        r: Expr[Reader],
        filter: TypeIdsAndSubTypes => TypeIdsAndSubTypes,
        comp: Long | String => Expr[Int])(using Quotes): Expr[T]
  }

  // returns all (recursively reachable, i.e. descendant) sub-types of `rootNode` along with a flag showing
  // whether an instance of the given typeclass is implicitly available for the respective sub-type
  //
  // The `deepRecurse` flag determines, whether to recurse into abstract sub-types whose flag is
  // `true` (deepRecurse == true) or not (deepRecurse == false).
  //
  // Abstract sub-types whose flag is `true` are themselves always returned (if reached during the tree walk)
  // while abstract sub-types whose flag is `false` are themselves never part of the result.
  private def flattenedSubs[F[_]: Type](
      rootNode: AdtTypeNode,
      deepRecurse: Boolean,
      includeEnumSingletonCases: Boolean)(using Quotes): Array[(AdtTypeNode, Boolean)] = {
    val buf = new ArrayBuffer[(AdtTypeNode, Boolean)]
    @tailrec def rec(remaining: List[AdtTypeNode]): Array[(AdtTypeNode, Boolean)] =
      remaining match {
        case head :: tail =>
          head.tpe.asType match
            case '[t] =>
              val implicitAvailable = Expr.summon[F[t]].isDefined
              def appendHead()      = if (!buf.exists(_._1.tpe =:= head.tpe)) buf += head -> implicitAvailable
              rec {
                if (head.isEnumSingletonCase) {
                  if (includeEnumSingletonCases) appendHead()
                  tail
                } else if (head.isAbstract) {
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
    rec(rootNode.subs)
  }

  private def extractTypeIdsAndSort(
      node: AdtTypeNode,
      flattened: Array[(AdtTypeNode, Boolean)]): Array[(Long | String, AdtTypeNode)] = {
    val result = flattened.map(x => x._1.key -> x._1)
    sortAndVerifyNoCollisions(result) { case (k, a, b) =>
      report.errorAndAbort(
        s"@key collision: sub types `${a.name}` and `${b.name}` of ADT `${node.name}` share the same type id `$k`",
        node.tpe.typeSymbol.pos.getOrElse(Position.ofMacroExpansion))
    }
    result
  }

  final def sortAndVerifyNoCollisions[A](array: Array[(Long | String, A)])(
      onCollision: (Long | String, A, A) => Nothing): Unit = {
    def lessThan(comp: Int, k: Long | String, a: A, b: A): Boolean = if (comp == 0) onCollision(k, a, b) else comp < 0
    java.util.Arrays.sort(
      array,
      Ordering.fromLessThan[(Long | String, A)] {
        case ((x: Long, a), (y: Long, b))     => lessThan(java.lang.Long.compare(x, y), x, a, b)
        case ((x: String, a), (y: String, b)) => lessThan(x.compareTo(y), x, a, b)
        case ((x, _), _)                      => x.isInstanceOf[Long] // we sort Long keys before String keys
      })
  }

  /**
   * When generating type class instances for a whole type hierarchy this method returns
   * - all ADT subtypes that don't have implicit type class instances already available
   * - all ADT subtypes that can act as "cycle-breakers", breaking circular dependencies
   *
   * The first list is already ordered in a way that allows intra-dependencies (i.e. between sub types) be resolved
   * properly if the missing implicits are generated in order.
   * The second list needs a special mutable construct in order to allow for deferred definitions,
   * breaking dependency cycles.
   */
  final def subtypesNeedingTypeclassDerivation[F[_] : Type](rootNode: AdtTypeNode): (List[TypeRepr], List[TypeRepr]) = {
    val subNodesInclSingletonCases = flattenedSubs[F](rootNode, deepRecurse = false, includeEnumSingletonCases = true)
    val (singletonCases, subNodes) = subNodesInclSingletonCases.partition(_._1.isEnumSingletonCase)
    if (subNodes.isEmpty)
      fail {
        if (singletonCases.isEmpty)
          s"""Could not find any sub-types of `$theTname`, likely because `$theTname` is not a fully closed ADT.
             |Do you maybe have a `sealed trait` somewhere, which has no subclasses?""".stripMargin
        else
          s"""This enum does not define any sub-classes.
             |You can therefore replace the `deriveAll` with a simple `derive`""".stripMargin
      }

    val subTypesWithoutTC = subNodes.toList.collect { case (node, false) => node.tpe }
    if (subTypesWithoutTC.isEmpty)
      val fName = TypeRepr.of[F].typeSymbol.name
      fail(s"It looks like all subtypes of `$theTname` already have a given $fName available. " +
        "You can therefore replace the `deriveAll` with a simple `derive`.")

    type SubsWithMembers = List[(TypeRepr, List[TypeRepr])]

    // for each subtype we record all membertypes, recursing into type-args
    val subsWithMembers: SubsWithMembers = subTypesWithoutTC.map { sub =>
      sub.asType match
        case '[t] =>
          Expr.summon[Mirror.Of[t]] match
            case Some('{ $m: Mirror.Product {type MirroredElemTypes = elementTypes} }) =>
              @tailrec def withRecursiveTypeArgs(remaining: List[TypeRepr], result: List[TypeRepr]): List[TypeRepr] =
                remaining match
                  case Nil => result
                  case head :: tail => withRecursiveTypeArgs(head.typeArgs.reverse_:::(tail), head :: result)

              sub -> withRecursiveTypeArgs(typeReprsOf[elementTypes], Nil)
            case _ => sub -> Nil
    }

    // flattenedSubs above doesn't return abstract subtypes that have no typeclass defined for them
    // but since they might be used as membertypes somewhere we look for them here and include them if found
    val augmentedSubsWithMembers = subsWithMembers.foldLeft(Nil: SubsWithMembers) { case (acc, x@(_, memberTypes)) =>
      memberTypes.foldLeft(x :: acc) { (acc, memberType) =>
        if (!(memberType <:< rootNode.tpe) || (memberType =:= rootNode.tpe) ||
          subNodes.exists(_._1.tpe =:= memberType) || acc.exists(_._1 =:= memberType)) acc
        else (memberType -> adtChildren(memberType)) :: acc // found an abstract subtype that we have to include
      }
    }

    // for each subtype S we collect the "dependencies", which are the ADT subtypes that appear somewhere within the member
    // definitions of S and, as such, need to have their typeclass implicitly available _before_ the implicit derivation of S
    val subsWithDeps = augmentedSubsWithMembers.map { case (sub, memberTypes) =>
      sub -> memberTypes.filter(t => augmentedSubsWithMembers.exists(_._1 =:= t))
    }

    @tailrec def topoSort(remaining: SubsWithMembers, blackList: List[TypeRepr], result: List[TypeRepr],
                          cycleBreakers: List[TypeRepr]): (List[TypeRepr], List[TypeRepr]) =
      remaining match {
        case (n, deps) :: tail =>
          val (toPullForward, otherRemaining) = remaining.partition(x => deps.exists(_ =:= x._1))
          if (toPullForward.isEmpty) {
            // all dependencies of the node are already in the result, so we can simply add the node itself
            topoSort(tail, blackList, n :: result, cycleBreakers)
          } else {
            if (blackList.contains(n)) {
              // we have a cycle that can be broken by treating `n` specially,
              // so we remove `n` from the process (and the result!) but add it to the `cycleBreakers`
              topoSort(tail, blackList, result, n :: cycleBreakers)
            } else {
              // we have some types that need to be handled first, so we don't actually make any progress
              // but simply reorder the `remaining` list and try again, thereby remembering that already processed `n`
              topoSort(toPullForward ::: otherRemaining, n :: blackList, result, cycleBreakers)
            }
          }
        case Nil => result.reverse -> cycleBreakers.reverse
      }

    topoSort(subsWithDeps, Nil, Nil, Nil)
  }

  private def typeReprsOf[Ts: Type]: List[TypeRepr] =
    Type.of[Ts] match
      case '[EmptyTuple] => Nil
      case '[t *: ts]    => TypeRepr.of[t] :: typeReprsOf[ts]

  private def adtChildren(tpe: TypeRepr): List[TypeRepr] =
    tpe.asType match
      case '[t] =>
        Expr.summon[Mirror.Of[t]].toList.flatMap {
          case '{ $m: Mirror.SumOf[t] { type MirroredElemTypes = subs } } => typeReprsOf[subs]
          case x                                                          => Nil
        }

  /**
   * Somewhat "unorthodox" way of abstracting over the rendering of a variable number of definitions
   * (e.g. values, variables or givens) before some nested block in a stack-safe fashion.
   *
   * @param as A collection of values of some type `A`. Each value is rendered into one definition.
   * @param tpe Simple function providing the capability of extracting the definition type form a value of type `A`
   * @param f A polymorphic function (with an `X: Type` context bound) turning a value of type `A`
   *          into an `Expr[Any]` (the respective definition) using a callback taking a value of type `B`.
   *          The `B` values of all definitions are collected and passed to `result` function in the end.
   * @param result Function returning the (nested) result expression using the collected `B` values.
   */
  protected def renderEach[A, B:ClassTag, R](as: IArray[A])(tpe: A => Type[?])
                                            (f: Quotes ?=> [X] => A => Type[X] ?=> (Quotes ?=> B => Expr[Any]) => Expr[Any])
                                            (result: Quotes ?=> IArray[B] => Expr[R])
                                            (using Quotes): Expr[R] =
    val bs = Array.ofDim[B](as.size)

    @tailrec def rec(ix: Int, statements: List[Statement], q: Quotes): Expr[R] =
      if (ix < as.size) {
        val a = as(ix)
        tpe(a) match
          case '[t] =>
            var nextQ: Quotes = null
            val expr = f(using q)[t](a) { b =>
              nextQ = summon[Quotes]
              bs(ix) = b
              '{ () }
            }
            val tree = expr.asTerm match
                case Inlined(_, _, Block(List(x), _)) => x
            rec(ix + 1, tree :: statements, nextQ)
      } else Block(statements.reverse, result(using q)(IArray.unsafeFromArray(bs)).asTerm).asExpr.asInstanceOf[Expr[R]]

    rec(0, Nil, summon[Quotes])

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  final def isBasicDefaultEncoder(encoder: Expr[Encoder[?]]): Boolean =
    encoder match
      case '{ Encoder.forString }  => true
      case '{ Encoder.forInt }     => true
      case '{ Encoder.forLong }    => true
      case '{ Encoder.forBoolean } => true
      case '{ Encoder.forDouble }  => true
      case '{ Encoder.forFloat }   => true
      case '{ Encoder.forChar }    => true
      case '{ Encoder.forByte }    => true
      case '{ Encoder.forShort }   => true
      case '{ Encoder.forNull }    => true
      case _                       => false

  final def isBasicDefaultDecoder(decoder: Expr[Decoder[?]]): Boolean =
    decoder match
      case '{ Decoder.forString }  => true
      case '{ Decoder.forInt }     => true
      case '{ Decoder.forLong }    => true
      case '{ Decoder.forBoolean } => true
      case '{ Decoder.forDouble }  => true
      case '{ Decoder.forFloat }   => true
      case '{ Decoder.forChar }    => true
      case '{ Decoder.forByte }    => true
      case '{ Decoder.forShort }   => true
      case '{ Decoder.forNull }    => true
      case _                       => false

  final def writeField[A: Type](w: Expr[Writer], value: Expr[A], encoder: Option[Expr[Encoder[?]]])(
      using Quotes): Expr[Writer] =
    encoder match
      case Some(x) => '{ $w.write($value)(using ${ x.asExprOf[Encoder[A]] }) }
      case None =>
        summon[Type[A]] match
          case '[String]  => '{ $w.writeString(${ value.asExprOf[String] }) }
          case '[Int]     => '{ $w.writeInt(${ value.asExprOf[Int] }) }
          case '[Long]    => '{ $w.writeLong(${ value.asExprOf[Long] }) }
          case '[Boolean] => '{ $w.writeBoolean(${ value.asExprOf[Boolean] }) }
          case '[Double]  => '{ $w.writeDouble(${ value.asExprOf[Double] }) }
          case '[Float]   => '{ $w.writeFloat(${ value.asExprOf[Float] }) }
          case '[Char]    => '{ $w.writeChar(${ value.asExprOf[Char] }) }
          case '[Byte]    => '{ $w.writeByte(${ value.asExprOf[Byte] }) }
          case '[Short]   => '{ $w.writeShort(${ value.asExprOf[Short] }) }
          case '[Null]    => '{ $w.writeNull() }
          case _          => fail(s"Cannot find given Encoder[${Type.show[A]}]")

  final def readField[A: Type](r: Expr[Reader], decoder: Option[Expr[Decoder[?]]])(using Quotes): Expr[A] =
    decoder match
      case Some(x) => '{ $r.read[A]()(using ${ x.asExprOf[Decoder[A]] }) }
      case None =>
        summon[Type[A]] match
          case '[String]  => '{ $r.readString() }.asExprOf[A]
          case '[Int]     => '{ $r.readInt() }.asExprOf[A]
          case '[Long]    => '{ $r.readLong() }.asExprOf[A]
          case '[Boolean] => '{ $r.readBoolean() }.asExprOf[A]
          case '[Double]  => '{ $r.readDouble() }.asExprOf[A]
          case '[Float]   => '{ $r.readFloat() }.asExprOf[A]
          case '[Char]    => '{ $r.readChar() }.asExprOf[A]
          case '[Byte]    => '{ $r.readByte() }.asExprOf[A]
          case '[Short]   => '{ $r.readShort() }.asExprOf[A]
          case '[Null]    => '{ $r.readNull() }.asExprOf[A]
          case _          => fail(s"Cannot find given Decoder[${Type.show[A]}]")

  final def typeArgs(tpe: TypeRepr): List[TypeRepr] = tpe match
    case AppliedType(_, typeArgs) => typeArgs.map(_.dealias)
    case _                        => Nil

  final def withVal[A: Type, B: Type](initialValue: Expr[A])(inner: Quotes ?=> Expr[A] => Expr[B])(
      using Quotes): Expr[B] =
    '{ val x: A = $initialValue; ${ inner('x) } }

  sealed abstract class Val {
    type V
    def tpe: Type[V]
    def get: Expr[V]
    final def as[A: Type]: Val { type V = A } =
      if (TypeRepr.of[A] <:< TypeRepr.of(using tpe)) this.asInstanceOf[Val { type V = A }]
      else {
        val msg = s"Val[${Type.show(using tpe)}] cannot be cast to Val[${Type.show[A]}]"
        printStackTracePrefix(msg)
        fail(msg)
      }
  }

  object Val {
    def of[A: Type](expr: Expr[A]): Val { type V = A } =
      new Val {
        type V = A
        val tpe = summon[Type[A]]
        val get = expr
      }
  }

  sealed abstract class Var {
    type V
    def tpe: Type[V]
    def get: Expr[V]
    def set(newValue: Expr[V])(using Quotes): Expr[Unit]
    final def as[A: Type]: Var { type V = A } =
      if (TypeRepr.of[A] =:= TypeRepr.of(using tpe)) this.asInstanceOf[Var { type V = A }]
      else {
        val msg = s"Var[${Type.show(using tpe)}] cannot be cast to Var[${Type.show[A]}]"
        printStackTracePrefix(msg)
        fail(msg)
      }
  }

  object Var {
    def of[A: Type](g: Expr[A], s: Quotes ?=> Expr[A] => Expr[Unit]): Var { type V = A } =
      new Var {
        type V = A
        def tpe                                              = summon[Type[A]]
        def get                                              = g
        def set(newValue: Expr[A])(using Quotes): Expr[Unit] = s(newValue)
      }
  }

  final def withVals[A: Type](array: IArray[Val])(next: Quotes ?=> IArray[Val] => Expr[A])(using Quotes): Expr[A] =
    renderEach[Val, Val, A](array)(_.tpe) {
      [X] => (v: Val) => tail => '{ val x: X = ${ v.as[X].get }; ${ tail(Val.of('x)) } }
    } { vals => next(vals) }

  final def withVars[A: Type](inits: IArray[Val])(next: Quotes ?=> IArray[Var] => Expr[A])(using Quotes): Expr[A] =
    renderEach[Val, Var, A](inits)(_.tpe) {
      [X] => (v: Val) => tail => '{ var x: X = ${ v.as[X].get } ; ${ tail(Var.of('x, newX => '{ x = $newX })) } }
    } { vals => next(vals) }

  final def withOptVals[A: Type](array: IArray[Option[Val]])(
      next: Quotes ?=> IArray[Option[Val]] => Expr[A])(using Quotes): Expr[A] =
    val arrayWithIndices = array.zipWithIndex.flatMap { case (x, i) => x.map(_ -> i) }
    renderEach[(Val, Int), (Val, Int), A](arrayWithIndices)(_._1.tpe) {
      [X] => (tup: (Val, Int)) => tail => '{ val x: X = ${ tup._1.as[X].get } ; ${ tail(Val.of('x) -> tup._2) } }
    } { valsWithIndices =>
      val result = Array.fill[Option[Val]](array.size)(None)
      valsWithIndices.foreach { case (v, i) => result(i) = Some(v) }
      next(IArray.unsafeFromArray(result))
    }

  final def companionApply(companion: Symbol, typeArgs: List[TypeRepr], args: List[Term]): Term =
    Select.overloaded(Ref(companion), "apply", typeArgs, args)

  final def select[A: Type](target: Expr[?], memberName: String): Expr[A] =
    Select.unique(target.asTerm, memberName).asExprOf[A]

  final def fail(msg: String): Nothing = report.errorAndAbort(msg, Position.ofMacroExpansion)

  final def printStackTracePrefix(msg: String): Unit = {
    val sw = new java.io.StringWriter
    val pw = new java.io.PrintWriter(sw)
    new RuntimeException("Stack Trace").printStackTrace(pw)
    val trace = sw.toString.split("\n").slice(2, 6).mkString("\n")
    println(s"$msg\n$trace")
  }
}
