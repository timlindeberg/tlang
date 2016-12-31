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

    def getSymbol(sym: c.universe.Symbol) =
      sym.asInstanceOf[scala.reflect.internal.Symbols#Symbol].sourceModule.asInstanceOf[Symbol]

    val symbol = weakTypeOf[A].typeSymbol

    if (!(symbol.isClass && symbol.asClass.isSealed))
      c.abort(c.enclosingPosition, "Can only enumerate values of a sealed trait or class.")

    val children = symbol.asClass.knownDirectSubclasses.toList

    if (!children.forall(_.isModuleClass))
      c.abort(c.enclosingPosition, "All children must be objects.")

    c.Expr[Set[A]] {
      Apply(
        Select(reify(Set).tree, TermName("apply")),
        children.map(sym => Ident(getSymbol(sym)))
      )
    }
  }


}
