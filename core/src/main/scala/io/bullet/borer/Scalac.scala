/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import scala.reflect.macros.blackbox
import scala.util.control.NonFatal
import scala.util.matching.Regex

object Scalac {

  sealed trait TypeCheck {
    def assertErrorMsgMatches(string: String): Unit
    def assertErrorMsgMatches(regex: Regex): Unit
  }

  object TypeCheck {
    final case class Result(code: String, tpe: String) extends TypeCheck {
      def assertErrorMsgMatches(string: String): Unit = assertErrorMsgMatches(null: Regex)

      def assertErrorMsgMatches(regex: Regex): Unit =
        sys.error(s"Code Fragment compiled without error to an expression of type `$tpe`:\n\n$code")
    }
    final case class Error(msg: String) extends TypeCheck {
      def assertErrorMsgMatches(string: String): Unit = assert(msg == string, string)
      def assertErrorMsgMatches(regex: Regex): Unit   = assert(regex.findAllIn(msg).hasNext, regex)
      private def assert(value: Boolean, expected: Any): Unit =
        if (!value) sys.error(s"Expected compiler error matching [$expected] but got [$msg]")
    }
  }

  /**
    * Type-checks the given code fragment and returns either the resulting code (as a String)
    * or the compiler error message.
    */
  def typecheck(codeFragment: String): TypeCheck = macro Macro.typecheck

  private object Macro {

    def typecheck(c: blackbox.Context)(codeFragment: c.Tree): c.Tree = {
      import c.universe._

      val fragment = codeFragment match {
        case Literal(Constant(x: String)) => x
        case _                            => c.abort(c.enclosingPosition, "`codeFragment` argument must be a literal string")
      }

      try {
        val name0 = TermName(c.freshName)
        val name1 = TermName(c.freshName)
        c.typecheck(c.parse(s"object $name0 { val $name1 = { $fragment } }")) match {
          case ModuleDef(_, _, Template(_, _, List(_, valDef: ValDef, defDef: DefDef))) =>
            val tpe = defDef.symbol.asMethod.returnType.toString
            q"_root_.io.bullet.borer.Scalac.TypeCheck.Result(${showCode(valDef.rhs)}, $tpe)"
          case x => c.abort(c.enclosingPosition, s"Unexpected scalac result:\n\n${showCode(x)}")
        }
      } catch {
        case NonFatal(e) => q"_root_.io.bullet.borer.Scalac.TypeCheck.Error(${e.getMessage})"
      }
    }
  }
}
