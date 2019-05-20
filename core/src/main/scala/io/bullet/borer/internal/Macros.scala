/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.internal

import scala.reflect.macros.{blackbox, ParseException, TypecheckException}
import java.util.regex.Pattern

object Macros {

  def encoderForUnaryCaseClass[T: c.WeakTypeTag](c: blackbox.Context): c.Tree = {
    import c.universe._
    val tpe    = weakTypeOf[T].dealias
    val fields = getCaseClassFields(c)(tpe)
    if (fields.lengthCompare(1) == 0) encoder(c)(tpe, fields)
    else c.abort(c.enclosingPosition, s"`$tpe` is not a unary case class")
  }

  def encoderForCaseClass[T: c.WeakTypeTag](c: blackbox.Context): c.Tree = {
    import c.universe._
    val tpe    = weakTypeOf[T].dealias
    val fields = getCaseClassFields(c)(tpe)
    encoder(c)(tpe, fields)
  }

  private def encoder[T: c.WeakTypeTag](
      c: blackbox.Context)(tpe: c.Type, fields: List[c.universe.MethodSymbol]): c.Tree = {
    import c.universe._
    val arity     = fields.size
    val stringTpe = typeOf[String]
    val writeOpen = if (arity == 1) q"w" else q"w.writeArrayOpen($arity)"
    val writeOpenAndFields = fields.foldLeft(writeOpen) { (acc, field) =>
      val access = q"x.${field.name}"
      field.typeSignatureIn(tpe).resultType match {
        case x if x =:= stringTpe              => q"$acc.writeString($access)"
        case x if x =:= definitions.IntTpe     => q"$acc.writeInt($access)"
        case x if x =:= definitions.LongTpe    => q"$acc.writeLong($access)"
        case x if x =:= definitions.BooleanTpe => q"$acc.writeBoolean($access)"
        case x if x =:= definitions.DoubleTpe  => q"$acc.writeDouble($access)"
        case x if x =:= definitions.FloatTpe   => q"$acc.writeFloat($access)"
        case x if x =:= definitions.CharTpe    => q"$acc.writeChar($access)"
        case x if x =:= definitions.ByteTpe    => q"$acc.writeByte($access)"
        case x if x =:= definitions.ShortTpe   => q"$acc.writeShort($access)"
        case _                                 => q"$acc.write($access)"
      }
    }
    val writeOpenFieldsAndClose = if (arity == 1) writeOpenAndFields else q"$writeOpenAndFields.writeArrayClose()"
    q"""_root_.io.bullet.borer.Encoder((w, x) => $writeOpenFieldsAndClose)"""
  }

  def decoderForUnaryCaseClass[T: c.WeakTypeTag](c: blackbox.Context): c.Tree = {
    import c.universe._
    val tpe    = weakTypeOf[T].dealias
    val fields = getCaseClassFields(c)(tpe)
    if (fields.lengthCompare(1) == 0) decoder(c)(tpe, fields)
    else c.abort(c.enclosingPosition, s"`$tpe` is not a unary case class")
  }

  def decoderForCaseClass[T: c.WeakTypeTag](c: blackbox.Context): c.Tree = {
    import c.universe._
    val tpe    = weakTypeOf[T].dealias
    val fields = getCaseClassFields(c)(tpe)
    decoder(c)(tpe, fields)
  }

  def decoder[T: c.WeakTypeTag](c: blackbox.Context)(tpe: c.Type, fields: List[c.universe.MethodSymbol]): c.Tree = {
    import c.universe._

    val typeSymbol = tpe.typeSymbol
    val companion  = typeSymbol.companion
    val apply =
      if (companion == NoSymbol) {
        if (typeSymbol.isClass && typeSymbol.asClass.isCaseClass) {
          q"${TermName(typeSymbol.name.decodedName.toString)}.apply"
        } else c.abort(c.enclosingPosition, s"`$tpe` is not a case class")
      } else q"$companion.apply"

    val arity     = fields.size
    val stringTpe = typeOf[String]
    val readFields = fields.map { field =>
      field.typeSignatureIn(tpe).resultType match {
        case x if x =:= stringTpe              => q"r.readString()"
        case x if x =:= definitions.IntTpe     => q"r.readInt()"
        case x if x =:= definitions.LongTpe    => q"r.readLong()"
        case x if x =:= definitions.BooleanTpe => q"r.readBoolean()"
        case x if x =:= definitions.DoubleTpe  => q"r.readDouble()"
        case x if x =:= definitions.FloatTpe   => q"r.readFloat()"
        case x if x =:= definitions.CharTpe    => q"r.readChar()"
        case x if x =:= definitions.ByteTpe    => q"r.readByte()"
        case x if x =:= definitions.ShortTpe   => q"r.readShort()"
        case x                                 => q"r.read[$x]()"
      }
    }
    val readObject = q"$apply(..$readFields)"
    val readObjectWithWrapping =
      if (arity == 1) readObject else q"r.readArrayClose(r.readArrayOpen($arity), $readObject)"
    q"""_root_.io.bullet.borer.Decoder(r => $readObjectWithWrapping)"""
  }

  def codecForCaseClass[T: c.WeakTypeTag](c: blackbox.Context): c.Tree = {
    import c.universe._
    val tpe = weakTypeOf[T]
    q"""_root_.io.bullet.borer.Codec[$tpe](
          _root_.io.bullet.borer.Encoder.forCaseClass[$tpe],
          _root_.io.bullet.borer.Decoder.forCaseClass[$tpe])"""
  }

  def codecForUnaryCaseClass[T: c.WeakTypeTag](c: blackbox.Context): c.Tree = {
    import c.universe._
    val tpe = weakTypeOf[T]
    q"""_root_.io.bullet.borer.Codec[$tpe](
          _root_.io.bullet.borer.Encoder.forUnaryCaseClass[$tpe],
          _root_.io.bullet.borer.Decoder.forUnaryCaseClass[$tpe])"""
  }

  private def getCaseClassFields(c: blackbox.Context)(tpe: c.Type): List[c.universe.MethodSymbol] = {
    import c.universe._
    tpe.decls.collect { case m: MethodSymbol if m.isCaseAccessor => m.asMethod }.toList
  }

  /**
    * A utility which ensures that a code fragment does not typecheck.
    *
    * Original Credit: Stefan Zeiger (@StefanZeiger)
    */
  def assertCompileError0(c: blackbox.Context)(codeFragment: c.Tree): c.Tree = assertCompileError(c)(codeFragment, null)

  def assertCompileError(c: blackbox.Context)(codeFragment: c.Tree, errorMsgRegex: c.Tree): c.Tree = {
    import c.universe._

    val fragment = codeFragment match {
      case Literal(Constant(x: String)) => x
      case _                            => c.abort(c.enclosingPosition, "`codeFragment` argument must be a literal string")
    }
    val (expectedErrorMsgPattern, expected) = errorMsgRegex match {
      case null => None -> "Code Fragment compiled without any errors"
      case Literal(Constant(x: String)) =>
        Some(Pattern.compile(x, Pattern.CASE_INSENSITIVE | Pattern.DOTALL)) -> s"Expected compiler error matching [$x]"
    }

    try {
      val dummy0 = TermName(c.freshName)
      val dummy1 = TermName(c.freshName)
      c.typecheck(c.parse(s"object $dummy0 { val $dummy1 = { $fragment } }"))
      c.error(c.enclosingPosition, expected)
    } catch {
      case e: TypecheckException =>
        expectedErrorMsgPattern
          .filterNot(_.matcher(e.getMessage).matches)
          .foreach(_ => c.error(c.enclosingPosition, s"$expected but got [${e.getMessage}]"))

      case e: ParseException =>
        c.error(c.enclosingPosition, s"Parsing error:\n${e.getMessage}")
    }
    q"()"
  }
}
