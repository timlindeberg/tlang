package koolc
package analyzer

import utils._
import ast.Trees._
import Symbols._

object NameAnalysis extends Pipeline[Program, Program] {

  class NameAnalyser(ctx: Context) {
    import ctx.reporter._

    def addTo[T](map: Map[String, T], symbol: T, id: Identifier): Map[String, T] = {
      if (map.contains(id.value)) {
        error("Something already defined!", id)
      }
      map + (id.value -> symbol)
    }

    def addSymbols(t: Tree, s: GlobalScope): Unit = t match {
      case Program(main, classes) => {
        addSymbols(main, s)
        classes.foreach(addSymbols(_, s))
      }
      case MainObject(id, stats) => s.mainClass = new ClassSymbol(id.value)
      case ClassDecl(id, parent, vars, methods) => {
        var newSymbol = new ClassSymbol(id.value)
        s.classes = addTo(s.classes, newSymbol, id)
        vars.foreach(addSymbols(_, newSymbol))
        methods.foreach(addSymbols(_, newSymbol))
      }
      case _ => error("LOL")
    }

    def addSymbols(t: Tree, s: ClassSymbol): Unit = t match {
      case VarDecl(tpe, id) => s.members = addTo(s.members, new VariableSymbol(id.value), id)
      case MethodDecl(retType, id, args, vars, stats, retExpr) => {
        var newSymbol = new MethodSymbol(id.value, s)
        s.methods = addTo(s.methods, newSymbol, id)
        args.foreach(addSymbols(_, newSymbol))
        vars.foreach(addSymbols(_, newSymbol))
      }
      case _ => error("LOL")
    }

    def addSymbols(t: Tree, s: MethodSymbol): Unit = t match {
      case VarDecl(tpe, id) => s.members = addTo(s.members, new VariableSymbol(id.value), id)
      case Formal(tpe, id)  => s.params = addTo(s.params, new VariableSymbol(id.value), id)
      case _                => error("LOL")
    }

  }
  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._
    var g = new GlobalScope
    new NameAnalyser(ctx).addSymbols(prog, g)

    //def analyze(list: List[Tree]): Unit = list.foreach(analyze(_))
    // Step 1: Collect symbols in declarations

    // Step 2: Attach symbols to identifiers (except method calls) in method bodies

    // (Step 3:) Print tree with symbol ids for debugging

    // Make sure you check for all constraints

    prog
  }

}
