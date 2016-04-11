package tcompiler.utils

import scala.language.experimental.macros
import scala.reflect.macros.Context

/*
A macro to produce a TreeSet of all instances of a sealed trait.
Based on Travis Brown's work:
http://stackoverflow.com/questions/13671734/iteration-over-a-sealed-trait-in-scala
*/
object EnumerationMacros {
  def sealedInstancesOf[A]: Set[A] = macro sealedInstancesOf_impl[A]

  def sealedInstancesOf_impl[A: c.WeakTypeTag](c: Context) = {
    import c.universe._

    val symbol = weakTypeOf[A].typeSymbol

    if (!symbol.isClass) c.abort(
      c.enclosingPosition,
      "Can only enumerate values of a sealed trait or class."
    ) else if (!symbol.asClass.isSealed) c.abort(
      c.enclosingPosition,
      "Can only enumerate values of a sealed trait or class."
    ) else {
      val siblingSubclasses: List[Symbol] = scala.util.Try {
        val enclosingModule = c.enclosingClass.asInstanceOf[ModuleDef]
        enclosingModule.impl.body.filter { x =>
          scala.util.Try(x.symbol.asModule.moduleClass.asClass.baseClasses.contains(symbol))
            .getOrElse(false)
        }.map(_.symbol)
      } getOrElse {
        Nil
      }

      val children = symbol.asClass.knownDirectSubclasses.toList ::: siblingSubclasses
      if (!children.forall(x => x.isModuleClass || x.isModule)) c.abort(
        c.enclosingPosition,
        "All children must be objects."
      ) else c.Expr[Set[A]] {
        def sourceModuleRef(sym: Symbol) = Ident(
          if (sym.isModule) sym else
            sym.asInstanceOf[
              scala.reflect.internal.Symbols#Symbol
              ].sourceModule.asInstanceOf[Symbol]
        )

        Apply(
          Select(
            reify(Set).tree,
            newTermName("apply")
          ),
          children.map(sourceModuleRef(_))
        )
      }
    }
  }
}
