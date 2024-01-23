/*
 * Copyright (c) 2019-2023 Mathias Doenitz
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
object MapBasedCodecs extends DerivationApi {

  /**
   * Macro that creates an [[Encoder]] for [[T]] provided that
   * - [[T]] is a `case class`, `enum`, `sealed abstract class` or `sealed trait`
   * - [[Encoder]] instances for all members of [[T]] (if [[T]] is a `case class`)
   *   or all sub-types of [[T]] (if [[T]] is an ADT) are implicitly available
   *
   * Case classes are converted into a map of values, one key-value pair for each member.
   * The key for each member is a `String` holding the member's name.
   * This can be customized with the [[key]] annotation.
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
   * Case classes are created from a map of values, one key-value pair for each member.
   * The key for each member is a `String` holding the member's name.
   * This can be customized with the [[key]] annotation.
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
            '{ Encoder[T]((w, _) => w.writeEmptyMap()) }

          def deriveForCaseClass(
              tpe: TypeRepr,
              classSym: Symbol,
              companion: Symbol,
              fields: IArray[Field]): Expr[Encoder[T]] =

            val fieldEncoders   = fields.map(field => field.theType match { case '[t] => Expr.summon[Encoder[t]] })
            val basicFieldFlags = fields.map(field => fieldEncoders(field.index).exists(isBasicDefaultEncoder))
            val nonBasicFields  = fields.map(field => Option.when(!basicFieldFlags(field.index))(field))

            withVal('{ summonInline[DerivationConfig] }) { derivationConfig =>
              withOptVals {
                nonBasicFields.map {
                  _.flatMap { field =>
                    field.theType match
                      case '[t] if field.fieldType.isInstanceOf[OwnType] =>
                        Some {
                          Val.of[Encoder[t]] {
                            val fieldEnc = fieldEncoders(field.index).map(_.asExprOf[Encoder[t]]).getOrElse {
                              fail(s"Could not find given Encoder[${Type.show[t]}] " +
                                s"for field `${field.name}` of case class `$theTname`")
                            }
                            field.defaultValue[t] match
                              case Some(x) => '{ $fieldEnc.withDefaultValue($x).recursive }
                              case None => '{ $fieldEnc.recursive }
                          }
                        }
                      case _ => None
                  }
                }
              } { fieldEncDefs =>
                withOptVals {
                  nonBasicFields.map {
                    _.flatMap(field => field.theType match { case '[t] => field.defaultValue[t].map(Val.of[t](_)) })
                  }
                } { nonBasicDefaultValues =>

                  def gen(
                      w: Expr[Writer],
                      value: Expr[T],
                      count: Expr[Int],
                      setCount: Quotes ?=> Expr[Int] => Expr[Unit])(using Quotes): Expr[Writer] =
                    withVals {
                      fields.map { field =>
                        field.theType match
                          case '[t] =>
                            Val.of[Boolean] {
                              val fieldValue = select[t](value, field.name)
                              val decCount = setCount('{ $count - 1 })

                              def defaultVal(expr: Option[Expr[t]])(using Quotes): Expr[Boolean] =
                                expr match
                                  case Some(x) =>
                                    '{
                                      $derivationConfig.encodeCaseClassMemberDefaultValues
                                        || $fieldValue != $x
                                        || {
                                        $decCount; false
                                      }
                                    }
                                  case None => Expr(true)

                              if (basicFieldFlags(field.index)) {
                                defaultVal(field.defaultValue[t])
                              } else {
                                '{
                                  val enc = ${ fieldEncDefs(field.ownTypeIndex).get.as[Encoder[t]].get }.unwrap
                                  if (enc.isInstanceOf[Encoder.PossiblyWithoutOutput[t]])
                                    enc.asInstanceOf[Encoder.PossiblyWithoutOutput[t]].producesOutputFor($fieldValue) || {
                                      $decCount;
                                      false
                                    }
                                  else ${ defaultVal(nonBasicDefaultValues(field.index).map(_.get.asExprOf[t])) }
                                }
                              }
                            }
                      }
                    } { fieldOutputFlags =>

                      def writeEntriesBody(w: Expr[Writer])(using Quotes): Expr[Unit] =
                        fields.iterator
                          .map { field =>
                            field.theType match
                              case '[t] =>
                                val writeKey = field.key match
                                  case x: Long   => '{ $w.writeLong(${ Expr(x) }) }
                                  case x: String => '{ $w.writeString(${ Expr(x) }) }
                                val fieldValue = select[t](value, field.name)
                                val encoder    = fieldEncDefs(field.ownTypeIndex).map(_.get.asExprOf[Encoder[t]])
                                val flag       = fieldOutputFlags(field.index).get.asExprOf[Boolean]
                                '{ if ($flag) ${ writeField(writeKey, fieldValue, encoder) } }
                          }
                          .reduceLeftOption((a, b) => '{ $a; $b })
                          .getOrElse('{ () })

                      '{
                        def writeEntries(w: Writer): Writer = {
                          ${ writeEntriesBody('w) }
                          w
                        }

                        if ($w.writingCbor) writeEntries($w.writeMapHeader($count))
                        else writeEntries($w.writeMapStart()).writeBreak()
                      }
                    }
                  end gen

                  '{
                    Encoder[T] { (w, value) =>
                      var count = ${ Expr(fields.size) }
                      ${ gen('w, 'value, 'count, x => '{ count = $x }) }
                    }
                  }
                }
              }
            }
          end deriveForCaseClass

          def deriveForSealedTrait(rootNode: AdtTypeNode): Expr[Encoder[T]] =
            '{
              new MapBasedAdtEncoder[T](summonInline[AdtEncodingStrategy]):
                final protected def typeName = ${ Expr(rootNode.name) }
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
            '{
              Decoder[T] { r =>
                if (r.readMapOpen(0)) while (!r.tryReadBreak()) r.skipElement()
                ${ Ref(moduleTermSymbol).asExprOf[T] }
              }
            }

          def deriveForCaseClass(
              tpe: TypeRepr,
              classSym: Symbol,
              companion: Symbol,
              fields: IArray[Field]): Expr[Decoder[T]] =

            val arity         = fields.size
            val keysAndFields = new Array[(Long | String, Field)](arity)
            fields.foreach(x => keysAndFields(x.index) = x.key -> x)
            val keysAndFieldsSorted = keysAndFields.clone()
            sortAndVerifyNoCollisions(keysAndFieldsSorted) { case (k, a, b) =>
              fail(
                s"@key collision: parameters `${a.name}` and `${b.name}` " +
                  s"of case class `$theTname` share the same type id `$k`")
            }
            val fieldKeyNames = Expr.ofList(keysAndFields.iterator.map(t => Expr(t._1.toString)).toList)

            def gen(r: Expr[Reader], nonBasicDecoders: IArray[Option[Val]])(using Quotes): Expr[T] =

              def failDup(key: Long | String) = key match
                case x: Long   => '{ helpers.failDuplicateMapKey($r, ${ Expr(x) }, ${ Expr(theTname) }) }
                case x: String => '{ helpers.failDuplicateMapKey($r, ${ Expr(x) }, ${ Expr(theTname) }) }

              def readObjectBody(
                  remaining: Expr[Int],
                  setRemaining: Quotes ?=> Expr[Int] => Expr[Unit],
                  isMaskBitSet: Quotes ?=> Field => Expr[Boolean],
                  setMaskBit: Quotes ?=> Field => Expr[Unit],
                  isMaskComplete: Expr[Boolean],
                  construct: Quotes ?=> Expr[T] => Expr[T])(using Quotes) =

                def tryReadKey(key: Long | String)(using Quotes): Expr[Boolean] =
                  key match
                    case x: Long   => '{ $r.tryReadLong(${ Expr(x) }) }
                    case x: String => '{ $r.tryReadString(${ Expr(x) }) }
                def tryReadKeyCompare(key: Long | String)(using Quotes): Expr[Int] =
                  key match
                    case x: Long   => '{ $r.tryReadLongCompare(${ Expr(x) }) }
                    case x: String => '{ $r.tryReadStringCompare(${ Expr(x) }) }
                def defaultOrNull[A: Type](field: Field)(using Quotes): Expr[A] =
                  field.defaultValue[A].getOrElse('{ null.asInstanceOf[A] })

                withVars {
                  IArray.unsafeFromArray {
                    keysAndFields.map { (key, field) =>
                      field.theType match
                        case '[t] =>
                          Val.of[t] {
                            '{
                              if ($remaining != 0 && ${ tryReadKey(key) }) {
                                ${ setMaskBit(field) }
                                ${ setRemaining('{ $remaining - 1 }) }
                                ${ readField[t](r, nonBasicDecoders(field.ownTypeIndex).map(_.as[Decoder[t]].get)) }
                              } else ${ defaultOrNull[t](field) }
                            }
                          }
                    }
                  }
                } { fieldVars =>
                  def readFields(start: Int, end: Int)(using Quotes): Expr[Unit] =
                    if (start < end) {
                      val mid          = (start + end) >> 1
                      val (key, field) = keysAndFieldsSorted(mid)
                      val readFieldAndSetFieldVar = field.theType match
                        case '[t] =>
                          val decoder = nonBasicDecoders(field.ownTypeIndex).map(_.as[Decoder[t]].get)
                          fieldVars(field.index).as[t].set(readField[t](r, decoder))
                      val onMatch =
                        '{
                          if (${ isMaskBitSet(field) }) ${ failDup(key) }
                          $readFieldAndSetFieldVar
                          ${ setMaskBit(field) }
                        }
                      if (start < mid)
                        '{
                          def read(): Unit = {
                            val cmp = ${ tryReadKeyCompare(key) }
                            if (cmp < 0) ${ readFields(start, mid) }
                            else if (cmp > 0) ${ readFields(mid + 1, end) }
                            else $onMatch
                          }
                          read()
                        }
                      else '{ if (${ tryReadKeyCompare(key) } == 0) $onMatch else $r.skipTwoElements() }
                    } else '{ $r.skipTwoElements() }
                  end readFields

                  val fieldVarTerms = fieldVars.toList.map(_.get.asTerm)

                  '{
                    while ($remaining > 0 || $remaining < 0 && ! $r.tryReadBreak()) {
                      if (! $isMaskComplete) ${ readFields(0, arity) }
                      else $r.skipTwoElements()
                      ${ setRemaining('{ $remaining - 1 }) }
                    }
                    ${ construct(companionApply(companion, typeArgs(tpe), fieldVarTerms).asExprOf[T]) }
                  }
                }
              end readObjectBody

              def readObjectBody32(count: Expr[Int])(using Quotes): Expr[T] =
                val reqMask: Int = fields.foldLeft(0) { (acc, field) =>
                  if (field.hasDefaultValue) acc | (1 << field.index) else acc
                }

                '{
                  var rem: Int  = $count
                  var mask: Int = 0
                  ${
                    readObjectBody(
                      remaining = 'rem,
                      setRemaining = x => '{ rem = $x },
                      isMaskBitSet = field => '{ (mask & ${ Expr(1 << field.index) }) != 0 },
                      setMaskBit = field => '{ mask |= ${ Expr(1 << field.index) } },
                      isMaskComplete = '{ mask == ${ Expr((1 << arity) - 1) } },
                      construct = { compApply =>
                        '{
                          val testMask    = mask | ${ Expr(reqMask | (-1 << arity)) }
                          def failMissing = helpers.failMissing($r, ${ Expr(theTname) }, testMask, $fieldKeyNames)
                          if (testMask == -1) $compApply else failMissing
                        }
                      }
                    )
                  }
                }
              end readObjectBody32

              def readObjectBody64(count: Expr[Int])(using Quotes) =
                val reqMask: Long = fields.foldLeft(0L) { (acc, field) =>
                  if (field.hasDefaultValue) acc | (1L << field.index) else acc
                }

                '{
                  var rem: Int   = $count
                  var mask: Long = 0L
                  ${
                    readObjectBody(
                      remaining = 'rem,
                      setRemaining = x => '{ rem = $x },
                      isMaskBitSet = field => '{ (mask & ${ Expr(1L << field.index) }) != 0 },
                      setMaskBit = field => '{ mask |= ${ Expr(1L << field.index) } },
                      isMaskComplete = '{ mask == ${ Expr((1L << arity) - 1) } },
                      construct = { compApply =>
                        '{
                          val testMask    = mask | ${ Expr(reqMask | (-1L << arity)) }
                          def failMissing = helpers.failMissing($r, ${ Expr(theTname) }, testMask, $fieldKeyNames)
                          if (testMask == -1L) $compApply else failMissing
                        }
                      }
                    )
                  }
                }
              end readObjectBody64

              def readObjectBody128(count: Expr[Int])(using Quotes) =
                def reqMask(fieldIter: Iterator[Field]): Long =
                  fieldIter.foldLeft(0L) { (acc, field) =>
                    if (field.hasDefaultValue) acc | (1L << field.index) else acc
                  }

                '{
                  var rem: Int    = $count
                  var mask0: Long = 0L
                  var mask1: Long = 0L
                  ${
                    readObjectBody(
                      remaining = 'rem,
                      setRemaining = x => '{ rem = $x },
                      isMaskBitSet = field =>
                        '{ (${ if (field.index < 64) 'mask0 else 'mask1 } & ${ Expr(1L << field.index) }) != 0 },
                      setMaskBit = field =>
                        val bit = Expr(1L << field.index)
                        if (field.index < 64) '{ mask0 |= $bit }
                        else '{ mask1 |= $bit }
                      ,
                      isMaskComplete = '{ mask0 == -1 && mask1 == ${ Expr((1L << (arity - 64)) - 1) } },
                      construct = { compApply =>
                        '{
                          val testMask0 = mask0 | ${ Expr(reqMask(fields.iterator.take(64))) }
                          val testMask1 = mask1 | ${ Expr(reqMask(fields.iterator.drop(64)) | (-1L << (arity - 64))) }
                          def failMissing =
                            helpers.failMissing($r, ${ Expr(theTname) }, testMask0, testMask1, $fieldKeyNames)
                          if ((testMask0 & testMask1) == -1L) $compApply else failMissing
                        }
                      }
                    )
                  }
                }
              end readObjectBody128

              '{
                def readObject(count: Int): T = ${
                  if (fields.nonEmpty) {
                    if (arity <= 32) readObjectBody32('count)
                    else if (arity <= 64) readObjectBody64('count)
                    else if (arity <= 128) readObjectBody128('count)
                    else fail("Case classes mit > 128 fields are not supported")
                  } else
                    '{
                      if (count < 0) while (! $r.tryReadBreak()) $r.skipElement()
                      ${ companionApply(companion, typeArgs(tpe), Nil).asExprOf[T] }
                    }
                }

                if ($r.tryReadMapStart()) readObject(-1)
                else if ($r.hasMapHeader) {
                  val mapLength = $r.readMapHeader()
                  if (mapLength > Int.MaxValue) $r.overflow("Maps with more than 2^31 entries are not supported")
                  readObject(mapLength.toInt)
                } else
                  $r.unexpectedDataItem(${
                    Expr(s"Map Start or Map Header for decoding an instance of type `$theTname`")
                  })
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
                      .map { fieldDec =>
                        Val.of[Decoder[t]] {
                          field.defaultValue[t] match
                            case Some(x) => '{ $fieldDec.withDefaultValue($x).recursive }
                            case None => '{ $fieldDec.recursive }
                        }
                      }
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
                _.filter((_, n) => !n.isEnumSingletonCase),
                {
                  case x: Long   => '{ $r.tryReadLongCompare(${ Expr(x) }) }
                  case x: String => '{ $r.tryReadStringCompare(${ Expr(x) }) }
                })

            def read1(self: Expr[DerivedAdtDecoder[T]], r: Expr[Reader], typeId: Expr[Long])(using Quotes) =
              import java.lang.Long.compare as longCompare
              methodBody.derive(
                self,
                r,
                _.filter((tid, n) => tid.isInstanceOf[Long] && !n.isEnumSingletonCase),
                tid => '{ longCompare($typeId, ${ Expr(tid.asInstanceOf[Long]) }) })

            def read2(self: Expr[DerivedAdtDecoder[T]], r: Expr[Reader], typeId: Expr[String])(using Quotes) =
              methodBody.derive(
                self,
                r,
                _.filter((tid, n) => tid.isInstanceOf[String] && !n.isEnumSingletonCase),
                tid => '{ $typeId.compareTo(${ Expr(tid.asInstanceOf[String]) }) })

            def readWithEnvelope(
                self: Expr[DerivedAdtDecoder[T]],
                r: Expr[Reader],
                strategy: Expr[AdtEncodingStrategy])(using Quotes): Expr[T] =
              val rootName = Expr(rootNode.name)
              val res0: Expr[T] =
                '{
                  val opening = $strategy.readAdtEnvelopeOpen($r, $rootName)
                  val result  = ${ read0(self, r) }
                  $strategy.readAdtEnvelopeClose($r, opening, $rootName)
                  result
                }
              val res1: Expr[T] =
                if (rootNode.isEnum && rootNode.subs.exists(x => x.isEnumSingletonCase && x.key.isInstanceOf[String])) {
                  val readStringSingleton = methodBody.derive(
                    self,
                    r,
                    _.filter((tid, n) => tid.isInstanceOf[String] && n.isEnumSingletonCase),
                    tid => '{ $r.tryReadStringCompare(${ Expr(tid.asInstanceOf[String]) }) })
                  '{ if ($r.hasString) $readStringSingleton else $res0 }
                } else res0
              if (rootNode.isEnum && rootNode.subs.exists(x => x.isEnumSingletonCase && x.key.isInstanceOf[Long])) {
                val readLongSingleton = methodBody.derive(
                  self,
                  r,
                  _.filter((tid, n) => tid.isInstanceOf[Long] && n.isEnumSingletonCase),
                  tid => '{ $r.tryReadLongCompare(${ Expr(tid.asInstanceOf[Long]) }) })
                '{ if ($r.hasLong) $readLongSingleton else $res1 }
              } else res1

            '{
              new MapBasedAdtDecoder[T]:
                private val strategy = summonInline[AdtEncodingStrategy]

                def read(r: Reader): T =
                  val self = this // work around for compiler crash (AssertionError) during macro expansion
                  ${ readWithEnvelope('self, 'r, 'strategy) }

                def read(r: Reader, typeId: Long): T =
                  val self = this // work around for compiler crash (AssertionError) during macro expansion
                  ${ read1('self, 'r, 'typeId) }

                def read(r: Reader, typeId: String): T =
                  val self = this // work around for compiler crash (AssertionError) during macro expansion
                  ${ read2('self, 'r, 'typeId) }

                def failExpectedTypeId(r: Reader) =
                  r.unexpectedDataItem(${ Expr(s"type id key for subtype of `$theTname`") })
            }
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

  abstract class MapBasedAdtEncoder[T](strategy: AdtEncodingStrategy) extends DerivedAdtEncoder[T] {

    protected def typeName: String

    final def writeAdtValue[A](w: Writer, typeId: Long, value: A)(using encoder: Encoder[A]): Writer =
      encoder match {
        case enc: AdtEncoder[A] => enc.write(w, value)
        case enc =>
          strategy.writeAdtEnvelopeOpen(w, typeName)
          enc.write(w.writeLong(typeId), value)
          strategy.writeAdtEnvelopeClose(w, typeName)
      }

    final def writeAdtValue[A](w: Writer, typeId: String, value: A)(using encoder: Encoder[A]): Writer =
      encoder match {
        case enc: AdtEncoder[A] => enc.write(w, value)
        case enc =>
          strategy.writeAdtEnvelopeOpen(w, typeName)
          enc.write(w.writeString(typeId), value)
          strategy.writeAdtEnvelopeClose(w, typeName)
      }
  }

  abstract class MapBasedAdtDecoder[T] extends DerivedAdtDecoder[T]
}
