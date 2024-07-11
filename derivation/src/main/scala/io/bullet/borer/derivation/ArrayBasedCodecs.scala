/*
 * Copyright (c) 2019-2024 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.derivation

import io.bullet.borer.*
import scala.deriving.*
import scala.compiletime.*
import scala.quoted.*

/**
 * Derivation macros for array-based encodings.
 */
object ArrayBasedCodecs extends DerivationApi {

  /**
   * Macro that creates an [[Encoder]] for [[T]] provided that
   * - [[T]] is a `case class`, `enum`, `sealed abstract class` or `sealed trait`
   * - [[Encoder]] instances for all members of [[T]] (if [[T]] is a `case class`)
   *   or all sub-types of [[T]] (if [[T]] is an ADT) are implicitly available
   *
   * Case classes are converted into an array of values, one value for each member.
   *
   * NOTE: If `T` is unary (i.e. only has a single member) then the member value is written in an unwrapped form,
   * i.e. without the array container.
   */
  inline def deriveEncoder[T]: Encoder[T] = ${ Macros.encoder[T] }

  /**
   * Macro that creates an [[Encoder]] for [[T]] and all direct and indirect sub-types of [[T]],
   * which are concrete, i.e. not abstract.
   * [[T]] must be an `enum`, `sealed abstract class` or `sealed trait`.
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
  inline def deriveAllEncoders[T]: Encoder[T] = ${ Macros.allEncoders[T] }

  /**
   * Macro that creates a [[Decoder]] for [[T]] provided that
   * - [[T]] is a `case class`, `enum`, `sealed abstract class` or `sealed trait`
   * - [[Decoder]] instances for all members of [[T]] (if [[T]] is a `case class`)
   *   or all sub-types of [[T]] (if [[T]] is an ADT) are implicitly available
   *
   * Case classes are created from an array of deserialized values, one value for each member.
   *
   * NOTE: If `T` is unary (i.e. only has a single member) then the member value is expected in an unwrapped form,
   * i.e. without the array container.
   */
  inline def deriveDecoder[T]: Decoder[T] = ${ Macros.decoder[T] }

  /**
   * Macro that creates a [[Decoder]] for [[T]] and all direct and indirect sub-types of [[T]],
   * which are concrete, i.e. not abstract.
   * [[T]] must be an `enum`, `sealed abstract class` or `sealed trait`.
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
  inline def deriveAllDecoders[T]: Decoder[T] = ${ Macros.allDecoders[T] }

  /**
   * Macro that creates an [[Encoder]] and [[Decoder]] pair for [[T]].
   * Convenience shortcut for `Codec(deriveEncoder[T], deriveDecoder[T])`.
   */
  inline def deriveCodec[T]: Codec[T] = Codec(deriveEncoder[T], deriveDecoder[T])

  /**
   * Macro that creates an [[Encoder]] and [[Decoder]] pair for [[T]] and all direct and indirect sub-types of [[T]].
   * Convenience shortcut for `Codec(deriveAllEncoders[T], deriveAllDecoders[T])`.
   */
  inline def deriveAllCodecs[T]: Codec[T] = Codec(deriveAllEncoders[T], deriveAllDecoders[T])

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  private[derivation] object Macros {

    def encoder[T: Type](using quotes: Quotes): Expr[Encoder[T]] =
      Derive[Encoder, T] {
        import quotes.reflect.*

        new Deriver[Encoder, T, quotes.type] {

          def deriveForCaseObject(moduleTermSymbol: Symbol): Expr[Encoder[T]] =
            '{ Encoder[T]((w, _) => w.writeEmptyArray()) }

          def deriveForCaseClass(
              tpe: TypeRepr,
              classSym: Symbol,
              companion: Symbol,
              fields: IArray[Field]): Expr[Encoder[T]] =

            def gen(w: Expr[Writer], x: Expr[T], nonBasicEncoders: IArray[Option[Val]])(using Quotes): Expr[Writer] = {
              val arity     = fields.size
              val writeOpen = if (arity == 1) w else '{ $w.writeArrayOpen(${ Expr(arity) }) }
              val writeOpenAndFields = fields.foldLeft(writeOpen) { (acc, field) =>
                field.theType match
                  case '[t] =>
                    val encoder = nonBasicEncoders(field.ownTypeIndex).map(_.as[Encoder[t]].get)
                    writeField[t](acc, select[t](x, field.name), encoder)
              }
              if (arity == 1) writeOpenAndFields else '{ $writeOpenAndFields.writeArrayClose() }
            }

            withOptVals {
              fields.map { field =>
                field.theType match
                  case '[t] if field.fieldType.isInstanceOf[OwnType] =>
                    Expr
                      .summon[Encoder[t]]
                      .orElse(fail(
                        s"Could not find given Encoder[${Type.show[t]}] for field `${field.name}` of case class `$theTname`"))
                      .filterNot(isBasicDefaultEncoder)
                      .map(x => Val.of[Encoder[t]]('{ $x.recursive }))
                  case _ => None
              }
            } { nonBasicEncoders =>
              '{ Encoder[T] { (w, x) => ${ gen('w, 'x, nonBasicEncoders) } } }
            }
          end deriveForCaseClass

          def deriveForSealedTrait(rootNode: AdtTypeNode): Expr[Encoder[T]] =
            '{
              new ArrayBasedAdtEncoder[T]:
                def write(w: Writer, value: T): Writer = {
                  val self = this // work around for compiler crash (AssertionError) during macro expansion
                  ${ deriveAdtEncoderBody(rootNode, 'self, 'w, 'value) }
                }
            }
        }
      }

    def decoder[T: Type](using quotes: Quotes): Expr[Decoder[T]] =
      Derive[Decoder, T] {
        import quotes.reflect.*

        new Deriver[Decoder, T, quotes.type]:

          def deriveForCaseObject(moduleTermSymbol: Symbol): Expr[Decoder[T]] =
            '{ Decoder[T](r => r.readArrayClose(r.readArrayOpen(0), ${ Ref(moduleTermSymbol).asExprOf[T] })) }

          def deriveForCaseClass(
              tpe: TypeRepr,
              classSym: Symbol,
              companion: Symbol,
              fields: IArray[Field]): Expr[Decoder[T]] =

            def gen(r: Expr[Reader], nonBasicDecoders: IArray[Option[Val]])(using Quotes): Expr[T] =
              def readObjectBlock(r: Expr[Reader])(using Quotes) =
                withVals {
                  fields.map { field =>
                    field.theType match
                      case '[t] =>
                        val decoder = nonBasicDecoders(field.ownTypeIndex).map(_.as[Decoder[t]].get)
                        Val.of[t](readField[t](r, decoder))
                  }
                } { exprs =>
                  companionApply(companion, typeArgs(tpe), exprs.toList.map(_.get.asTerm)).asExprOf[T]
                }

              def expected(s: String)(using Quotes) = Expr(s"$s for decoding an instance of `$theTname`")

              fields.size match
                case 0 =>
                  '{
                    if ($r.tryReadArrayHeader(0) || $r.tryReadArrayStart() && $r.tryReadBreak()) ${ readObjectBlock(r) }
                    else $r.unexpectedDataItem(${ expected("Empty array") })
                  }

                case 1 => readObjectBlock(r)

                case x =>
                  '{
                    def readObject(r: Reader) = ${ readObjectBlock('r) }
                    if ($r.tryReadArrayStart()) {
                      val result = readObject($r)
                      if ($r.tryReadBreak()) result
                      else
                        $r.unexpectedDataItem(${ expected(s"Array with $x elements") }, "at least one extra element")
                    } else if ($r.tryReadArrayHeader(${ Expr(x) })) readObject($r)
                    else $r.unexpectedDataItem(${ expected(s"Array-Start or Array-Header($x)") })
                  }
            end gen

            withOptVals {
              fields.map { field =>
                field.theType match
                  case '[t] if field.fieldType.isInstanceOf[OwnType] =>
                    Expr
                      .summon[Decoder[t]]
                      .orElse(fail(
                        s"Could not find given Decoder[${Type.show[t]}] for field `${field.name}` of case class `$theTname`"))
                      .filterNot(isBasicDefaultDecoder)
                      .map(x => Val.of[Decoder[t]]('{ $x.recursive }))
                  case _ => None
              }
            } { nonBasicDecoders =>
              '{ Decoder[T](r => ${ gen('r, nonBasicDecoders) }) }
            }

          end deriveForCaseClass

          def deriveForSealedTrait(rootNode: AdtTypeNode): Expr[Decoder[T]] =
            val methodBody = sealedTraitMethodBody(rootNode)

            def read0(self: Expr[DerivedAdtDecoder[T]], r: Expr[Reader])(using Quotes) =
              methodBody.derive(
                self,
                r,
                identity,
                {
                  case x: Long   => '{ $r.tryReadLongCompare(${ Expr(x) }) }
                  case x: String => '{ $r.tryReadStringCompare(${ Expr(x) }) }
                })

            def read1(self: Expr[DerivedAdtDecoder[T]], r: Expr[Reader], typeId: Expr[Long])(using Quotes) =
              import java.lang.Long.compare as longCompare
              methodBody.derive(
                self,
                r,
                _.filter(_._1.isInstanceOf[Long]),
                tid => '{ longCompare($typeId, ${ Expr(tid.asInstanceOf[Long]) }) })

            def read2(self: Expr[DerivedAdtDecoder[T]], r: Expr[Reader], typeId: Expr[String])(using Quotes) =
              methodBody.derive(
                self,
                r,
                _.filter(_._1.isInstanceOf[String]),
                tid => '{ $typeId.compareTo(${ Expr(tid.asInstanceOf[String]) }) })

            def readWithEnvelope(self: Expr[DerivedAdtDecoder[T]], r: Expr[Reader])(using Quotes): Expr[T] =
              val res0: Expr[T] = '{ $r.readArrayClose($r.readArrayOpen(2), ${ read0(self, r) }) }
              val res1: Expr[T] =
                if (rootNode.allSubs.exists(x => x.isEnumSingletonCase && x.key.isInstanceOf[String])) {
                  val readStringSingleton = methodBody.derive(
                    self,
                    r,
                    _.filter((tid, n) => tid.isInstanceOf[String] && n.isEnumSingletonCase),
                    tid => '{ $r.tryReadStringCompare(${ Expr(tid.asInstanceOf[String]) }) })
                  '{ if ($r.hasString) $readStringSingleton else $res0 }
                } else res0
              if (rootNode.allSubs.exists(x => x.isEnumSingletonCase && x.key.isInstanceOf[Long])) {
                val readLongSingleton = methodBody.derive(
                  self,
                  r,
                  _.filter((tid, n) => tid.isInstanceOf[Long] && n.isEnumSingletonCase),
                  tid => '{ $r.tryReadLongCompare(${ Expr(tid.asInstanceOf[Long]) }) })
                '{ if ($r.hasLong) $readLongSingleton else $res1 }
              } else res1

            '{
              new ArrayBasedAdtDecoder[T]:

                def read(r: Reader): T =
                  val self = this // work around for compiler crash (AssertionError) during macro expansion
                  ${ readWithEnvelope('self, 'r) }

                def read(r: Reader, typeId: Long): T =
                  val self = this // work around for compiler crash (AssertionError) during macro expansion
                  ${ read1('self, 'r, 'typeId) }

                def read(r: Reader, typeId: String): T =
                  val self = this // work around for compiler crash (AssertionError) during macro expansion
                  ${ read2('self, 'r, 'typeId) }

                def failExpectedTypeId(r: Reader) =
                  r.unexpectedDataItem(${ Expr(s"type id key for subtype of `$theTname`") })
            }

          end deriveForSealedTrait
      }

    def allEncoders[T: Type](using Quotes): Expr[Encoder[T]] =
      Derive.deriveAll[Encoder, T]("allEncoders", "deriveEncoder") {
        new Derive.MacroCall[Encoder] {
          def apply[A: Type](using Quotes) = '{ deriveEncoder[A] }
        }
      }

    def allDecoders[T: Type](using Quotes): Expr[Decoder[T]] =
      Derive.deriveAll[Decoder, T]("allDecoders", "deriveDecoder") {
        new Derive.MacroCall[Decoder] {
          def apply[A: Type](using Quotes) = '{ deriveDecoder[A] }
        }
      }
  }

  abstract class ArrayBasedAdtEncoder[T] extends DerivedAdtEncoder[T] {

    final def writeAdtValue[A](w: Writer, typeId: Long, value: A)(using encoder: Encoder[A]): Writer =
      encoder match {
        case enc: AdtEncoder[A] => enc.write(w, value)
        case enc                => enc.write(w.writeArrayOpen(2).writeLong(typeId), value).writeArrayClose()
      }

    final def writeAdtValue[A](w: Writer, typeId: String, value: A)(using encoder: Encoder[A]): Writer =
      encoder match {
        case enc: AdtEncoder[A] => enc.write(w, value)
        case enc                => enc.write(w.writeArrayOpen(2).writeString(typeId), value).writeArrayClose()
      }

    final def enumCasesAsProduct: Boolean = false
  }

  abstract class ArrayBasedAdtDecoder[T] extends DerivedAdtDecoder[T]
}
