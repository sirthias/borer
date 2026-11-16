/*
 * Copyright (c) 2019-2026 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.derivation

import java.lang.reflect.InvocationTargetException

import scala.quoted.*

/**
 * Compile-time evaluation of the [[DerivationConfig]] available at a macro call site.
 *
 * The config is passed to the derivation macros via a `using inline` parameter. Since the config must be provided as
 * an `inline given` (or an explicit, static argument) the tree of this parameter contains the (inlined) definition
 * of the config value.
 * (Using a `using inline` parameter rather than `Expr.summon` makes sure that the compiler properly registers the
 * usage of the given, which is important for the unused-import checks.) This tree is then interpreted by a tiny interpreter, which supports
 * - literals, `None`, `Some(...)` and [[DerivationConfig.Naming]] cases
 * - (inlined) blocks and local `val` definitions
 * - method calls (incl. default argument getters) on [[DerivationConfig]], [[DerivationConfig.FlatAdtEncoding]]
 *   and their companions (incl. `apply` and `copy`), which are executed via reflection at compile time.
 *
 * Since all `with...` builder methods on [[DerivationConfig]] are `inline` they are already expanded into
 * `copy` calls in the tree and therefore don't require any special treatment.
 */
private[derivation] object StaticDerivationConfig:

  def evaluate(configExpr: Expr[DerivationConfig])(using q: Quotes): DerivationConfig =
    import q.reflect.*

    def fail(msg: String): Nothing = report.errorAndAbort(msg, Position.ofMacroExpansion)

    def failNonStatic(msg: String): Nothing =
      fail(
        s"$msg. A `DerivationConfig` must be provided as an `inline given` with a statically known value, " +
          "e.g. `inline given DerivationConfig = DerivationConfig().withFlatAdtEncoding()`.")

    val configSym = TypeRepr.of[DerivationConfig].typeSymbol
    val namingSym = TypeRepr.of[DerivationConfig.Naming].typeSymbol
    val noneSym   = Symbol.requiredModule("scala.None")

    val modules: Map[Symbol, AnyRef] = Map(
      configSym.companionModule                                                -> DerivationConfig,
      TypeRepr.of[DerivationConfig.FlatAdtEncoding].typeSymbol.companionModule -> DerivationConfig.FlatAdtEncoding,
      Symbol.requiredModule("scala.Some")                                      -> Some)

    val allowedReceiverClasses: Set[Class[?]] =
      modules.valuesIterator.map(_.getClass).toSet[Class[?]] +
        classOf[DerivationConfig] + classOf[DerivationConfig.FlatAdtEncoding]

    def unsupported(t: Tree): Nothing =
      failNonStatic(s"Cannot statically evaluate the given `DerivationConfig` (unsupported expression `${t.show}`)")

    def isNamingCase(sym: Symbol): Boolean =
      sym.flags.is(Flags.Enum) && sym.owner == namingSym.companionModule.moduleClass

    def invoke(tree: Tree, receiver: Any, name: String, args: List[Any]): Any =
      if (!allowedReceiverClasses.contains(receiver.getClass)) unsupported(tree)
      receiver.getClass.getMethods.filter(m => m.getName == name && m.getParameterCount == args.size) match
        case Array(method) =>
          try method.invoke(receiver, args.map(_.asInstanceOf[AnyRef])*)
          catch
            case e: InvocationTargetException =>
              fail(s"Invalid `DerivationConfig`: ${e.getCause.getMessage}")
        case _ => unsupported(tree)

    def bind(env: Map[Symbol, Any], stats: List[Tree]): Map[Symbol, Any] =
      stats.foldLeft(env) {
        case (e, vd @ ValDef(_, _, Some(rhs))) => e.updated(vd.symbol, eval(rhs, e))
        case (_, x)                            => unsupported(x)
      }

    def eval(t: Term, env: Map[Symbol, Any]): Any =
      t match
        case Inlined(_, bindings, x)            => eval(x, bind(env, bindings))
        case Block(stats, x)                    => eval(x, bind(env, stats))
        case Typed(x, _)                        => eval(x, env)
        case NamedArg(_, x)                     => eval(x, env)
        case Literal(c)                         => c.value
        case x: Ident if env.contains(x.symbol) => env(x.symbol)
        case x if x.symbol == noneSym           => None
        case x if isNamingCase(x.symbol)        => DerivationConfig.Naming.valueOf(x.symbol.name)
        case x if modules.contains(x.symbol)    => modules(x.symbol)
        case x @ (_: Ident | _: Select) if x.symbol.flags.is(Flags.Given) =>
          failNonStatic(s"The given `DerivationConfig` found (`${x.show}`) is not an `inline given`")
        case x if x.symbol.flags.is(Flags.Module)          => unsupported(x)
        case Apply(TypeApply(Select(qual, name), _), args) => invoke(t, eval(qual, env), name, args.map(eval(_, env)))
        case Apply(Select(qual, name), args)               => invoke(t, eval(qual, env), name, args.map(eval(_, env)))
        case Select(qual: Ident, _) if !env.contains(qual.symbol) && !modules.contains(qual.symbol) => unsupported(t)
        case Select(qual, name) => invoke(t, eval(qual, env), name, Nil)
        case x                  => unsupported(x)

    eval(configExpr.asTerm, Map.empty).asInstanceOf[DerivationConfig]
