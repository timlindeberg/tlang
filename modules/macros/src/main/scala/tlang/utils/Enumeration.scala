package tlang
package utils

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

/*
 * A macro to produce a List of all instances of a sealed trait.
 * Based on Travis Brown's work:
 * http://stackoverflow.com/questions/13671734/iteration-over-a-sealed-trait-in-scala
*/
object Enumeration {

  def instancesOf[A]: List[A] = macro instancesOf_impl[A]

  def instancesOf_impl[A: c.WeakTypeTag](c: whitebox.Context): c.Expr[List[A]] = {
    import c.universe._

    def getSymbol(sym: c.universe.Symbol) =
      sym.asInstanceOf[scala.reflect.internal.Symbols#Symbol].sourceModule.asInstanceOf[Symbol]

    val symbol: c.universe.Symbol = weakTypeOf[A].typeSymbol

    if (!symbol.isClass || !symbol.asClass.isSealed)
      c.abort(c.enclosingPosition, "Can only enumerate values of a sealed trait or class.")

    val children = symbol.asClass.knownDirectSubclasses.filter(_.isModuleClass).toList

    c.Expr[List[A]] {
      Apply(
        Select(reify(List).tree, TermName("apply")),
        children.map(sym => Ident(getSymbol(sym)))
      )
    }
  }
}
