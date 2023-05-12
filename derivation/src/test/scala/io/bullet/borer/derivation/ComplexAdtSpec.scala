/*
 * Copyright (c) 2019-2022 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.derivation

import io.bullet.borer.*

class ComplexAdtSpec extends AbstractBorerSpec:

  def encode[T: Encoder](value: T): String   = toHexString(Cbor.encode(value).toByteArray)
  def decode[T: Decoder](encoded: String): T = Cbor.decode(hexBytes(encoded)).to[T].value

  import ComplexAdtSpec.*
  import CompilerAst.*

  given Codec[Exp] = {
    import MapBasedCodecs._
    given Codec[ExprType]    = deriveCodec
    given Codec[TableColDef] = deriveCodec
    deriveAllCodecs[Exp]
  }

  test("Complex ADT") {
    val ast: Exp = InsertDef(
      List(ColDef("DUMMY", Obj(Ident(List("DUMMY")), null, null, null, false), ExprType("java.lang.Integer"))),
      List(TableDef("DUMMY", Obj(TableObj(Ident(List("DUMMY"))), null, null, null, false))),
      Insert(null, null, List(), Values(List(Arr(List(BigDecimalConst(0))))), None, None))

    val encoding = encode(ast)
    roundTrip(encoding, ast)
  }

object ComplexAdtSpec {

  sealed trait Exp

  sealed trait DMLExp extends Exp {
    def table: Ident

    def alias: String

    def cols: List[Col]

    def filter: Arr

    def vals: Exp

    def returning: Option[Cols]

    def db: Option[String]
  }

  sealed trait Const extends Exp {
    def value: Any
  }

  case class IntConst(value: Int) extends Const

  case class StringConst(value: String) extends Const

  case class BigDecimalConst(value: BigDecimal) extends Const

  case class BooleanConst(value: Boolean) extends Const

  case class Sql(sql: String) extends Exp

  case class Ident(ident: List[String]) extends Exp

  case class Variable(variable: String, members: List[String] = Nil, opt: Boolean) extends Exp

  case class Id(name: String) extends Exp

  case class IdRef(name: String) extends Exp

  case class Res(rNr: Int, col: Exp) extends Exp

  case class Cast(exp: Exp, typ: String) extends Exp

  case class UnOp(operation: String, operand: Exp) extends Exp

  case class ChildQuery(query: Exp, db: Option[String]) extends Exp

  case class Fun(
      name: String,
      parameters: List[Exp],
      distinct: Boolean,
      aggregateOrder: Option[Ord],
      aggregateWhere: Option[Exp]
  ) extends Exp

  case class TableColDef(name: String, typ: Option[String])

  case class FunAsTable(fun: Fun, cols: Option[List[TableColDef]], withOrdinality: Boolean) extends Exp

  case class In(lop: Exp, rop: List[Exp], not: Boolean) extends Exp

  case class BinOp(op: String, lop: Exp, rop: Exp) extends Exp

  case class TerOp(lop: Exp, op1: String, mop: Exp, op2: String, rop: Exp) extends Exp

  case class Join(default: Boolean, expr: Exp = null, noJoin: Boolean) extends Exp

  case class Obj(obj: Exp, alias: String = null, join: Join = null, outerJoin: String = null, nullable: Boolean = false)
      extends Exp

  case class Col(col: Exp, alias: String = null) extends Exp

  case class Cols(distinct: Boolean, cols: List[Col]) extends Exp

  case class Grp(cols: List[Exp], having: Exp = null) extends Exp

  case class OrdCol(nullsFirst: Exp = null, exp: Exp, nullsLast: Exp = null) extends Exp

  case class Ord(cols: List[OrdCol]) extends Exp

  case class Query(
      tables: List[Obj],
      filter: Filters,
      cols: Cols = null,
      group: Grp = null,
      order: Ord = null,
      offset: Exp = null,
      limit: Exp = null)
      extends Exp

  case class WithTable(name: String, cols: List[String], recursive: Boolean, table: Exp) extends Exp

  case class With(tables: List[WithTable], query: Exp) extends Exp

  case class Values(values: List[Arr]) extends Exp

  case class Insert(
      table: Ident = null,
      alias: String = null,
      cols: List[Col],
      vals: Exp = null,
      returning: Option[Cols],
      db: Option[String])
      extends DMLExp {
    override def filter = null
  }

  case class Update(
      table: Ident = null,
      alias: String = null,
      filter: Arr = null,
      cols: List[Col],
      vals: Exp = null,
      returning: Option[Cols],
      db: Option[String])
      extends DMLExp

  case class ValuesFromSelect(select: Query) extends Exp

  case class Delete(
      table: Ident = null,
      alias: String = null,
      filter: Arr,
      using: Exp = null,
      returning: Option[Cols],
      db: Option[String])
      extends DMLExp {
    override def cols = null

    override def vals = using
  }

  case class Arr(elements: List[Exp]) extends Exp

  case class Filters(filters: List[Arr]) extends Exp

  case object All extends Exp

  case class IdentAll(ident: Ident) extends Exp

  sealed trait Null extends Exp

  case object Null extends Null

  case object NullUpdate extends Null

  case class Braces(expr: Exp) extends Exp

  object CompilerAst {
    sealed trait CompilerExp extends Exp

    case class ExprType(name: String = null)

    sealed trait TypedExp extends CompilerExp {
      def exp: Exp

      def typ: ExprType
    }

    case class TableDef(name: String, exp: Obj) extends CompilerExp

    case class TableObj(obj: Exp) extends CompilerExp

    case class TableAlias(obj: Exp) extends CompilerExp

    case class ColDef(name: String = null, col: Exp, typ: ExprType) extends TypedExp {
      def exp: ColDef = this
    }

    case class ChildDef(exp: Exp, db: Option[String]) extends TypedExp {
      val typ: ExprType = ExprType(this.getClass.getName)
    }

    case class FunDef(name: String, exp: Fun, typ: ExprType, procedure: Fun) extends TypedExp

    case class FunAsTableDef(exp: FunDef, cols: Option[List[TableColDef]], withOrdinality: Boolean) extends CompilerExp

    case class RecursiveDef(exp: Exp) extends TypedExp {
      val typ: ExprType = ExprType(this.getClass.getName)
    }

    case class PrimitiveExp(exp: Exp) extends CompilerExp

    case class PrimitiveDef(exp: Exp, typ: ExprType) extends TypedExp

    sealed trait RowDefBase extends TypedExp {
      def cols: List[ColDef]

      val typ: ExprType = ExprType(this.getClass.getName)
    }

    sealed trait SQLDefBase extends RowDefBase {
      def tables: List[TableDef]
    }

    sealed trait DMLDefBase extends SQLDefBase {
      def db: Option[String]
    }

    sealed trait SelectDefBase extends SQLDefBase

    case class SelectDef(cols: List[ColDef], tables: List[TableDef], exp: Query) extends SelectDefBase

    case class BinSelectDef(leftOperand: SelectDefBase, rightOperand: SelectDefBase, exp: BinOp) extends SelectDefBase {
      def cols = Nil

      def tables = Nil
    }

    case class BracesSelectDef(exp: SelectDefBase) extends SelectDefBase {
      def cols = Nil

      def tables = Nil
    }

    case class WithTableDef(
        cols: List[ColDef],
        tables: List[TableDef],
        recursive: Boolean,
        exp: SQLDefBase
    ) extends SelectDefBase

    case class ValuesFromSelectDef(exp: SelectDefBase) extends SelectDefBase {
      def cols = Nil

      def tables = Nil
    }

    case class FunSelectDef(cols: List[ColDef], tables: List[TableDef], exp: FunDef) extends SelectDefBase

    case class InsertDef(cols: List[ColDef], tables: List[TableDef], exp: Insert) extends DMLDefBase {
      def db = None
    }

    case class UpdateDef(cols: List[ColDef], tables: List[TableDef], exp: Update) extends DMLDefBase {
      def db = None
    }

    case class DeleteDef(tables: List[TableDef], exp: Delete) extends DMLDefBase {
      def cols = Nil

      def db = None
    }

    case class ReturningDMLDef(cols: List[ColDef], tables: List[TableDef], exp: DMLDefBase) extends SelectDefBase

    sealed trait WithQuery extends SQLDefBase {
      def exp: SQLDefBase

      def withTables: List[WithTableDef]
    }

    sealed trait WithSelectBase extends SelectDefBase with WithQuery {
      def exp: SelectDefBase
    }

    sealed trait WithDMLQuery extends DMLDefBase with WithQuery {
      def exp: DMLDefBase

      override def db: Option[String] = None
    }

    case class WithSelectDef(exp: SelectDefBase, withTables: List[WithTableDef]) extends WithSelectBase {
      def cols = exp.cols

      def tables = exp.tables
    }

    case class WithDMLDef(exp: DMLDefBase, withTables: List[WithTableDef]) extends WithDMLQuery {
      def cols = exp.cols

      def tables = exp.tables
    }

    case class ArrayDef(cols: List[ColDef]) extends RowDefBase {
      def exp = this
    }
  }

}
