package koolc
package analyzer

import utils._
import ast.Trees._
import Symbols._
import org.omg.CosNaming.NamingContextExtPackage.InvalidAddressHelper

object NameAnalysis extends Pipeline[Program, Program] {

  class NameAnalyser(ctx: Context, prog: Program, g: GlobalScope) {
    import ctx.reporter._
    object Adder {
      def apply(): Unit = addSymbols(prog, g)
      private def addSymbols(t: Tree, s: GlobalScope): Unit = t match {
        case Program(main, classes) => {
          addSymbols(main, s)
          classes.foreach(addSymbols(_, s))
        }
        case x @ MainObject(id, stats) => {
          s.mainClass = new ClassSymbol(id.value).setPos(id)
          x.setSymbol(s.mainClass)
          id.setSymbol(s.mainClass)
        }
        case x @ ClassDecl(id, parent, vars, methods) => {
          val newSymbol = new ClassSymbol(id.value).setPos(id)
          s.classes = addTo(s.classes, newSymbol, id, x)
          vars.foreach(addSymbols(_, newSymbol))
          methods.foreach(addSymbols(_, newSymbol))
        }
        case _ => error("LOL")
      }

      private def addSymbols(t: Tree, s: ClassSymbol): Unit = t match {
        case x @ VarDecl(tpe, id) => {
          val newSymbol = new VariableSymbol(id.value).setPos(id)
          s.members = addTo(s.members, newSymbol, id, x)
        }
        case x @ MethodDecl(retType, id, args, vars, stats, retExpr) => {
          val newSymbol = new MethodSymbol(id.value, s).setPos(id)
          s.methods = addTo(s.methods, newSymbol, id, x)
          args.foreach(addSymbols(_, newSymbol))
          vars.foreach(addSymbols(_, newSymbol))
          newSymbol.argList = newSymbol.params.values.toList
        }
        case _ => error("LOL")
      }

      private def addSymbols(t: Tree, s: MethodSymbol): Unit = t match {
        case x @ VarDecl(tpe, id) => {
          val newSymbol = new VariableSymbol(id.value).setPos(id)
          s.members = addTo(s.members, newSymbol, id, x)
        }
        case x @ Formal(tpe, id) => {
          val newSymbol = new VariableSymbol(id.value).setPos(id)
          s.params = addTo(s.params, newSymbol, id, x)
        }
        case _ => error("LOL")
      }

      private def addTo[T <: Symbol](map: Map[String, T], symbol: T, id: Identifier, x: Symbolic[T]): Map[String, T] = {
        if (map.contains(id.value)) {
          val oldSymbol = map(id.value)
          error("Symbol \"" + id.value + "\" already defined at " + oldSymbol.line + ":" + oldSymbol.col, id)
        }
        id.setSymbol(symbol)
        x.setSymbol(symbol)
        map + (id.value -> symbol)
      }
    }
    object Binder {
      def apply(): Unit = bind(prog)
      private def bind(t: Option[Tree]): Unit = if (t.isDefined) bind(t.get)

      private def bind(list: List[Tree]): Unit = list.foreach(bind)

      private def bind(t: Tree): Unit = t match {
        case Program(main, classes) =>
          bind(main); bind(classes)
        case x @ MainObject(id, stats) => bind(x.getSymbol, stats)
        case classDecl @ ClassDecl(id, parent, vars, methods) => {
          setParent(id, parent, classDecl)

          val p = classDecl.getSymbol.parent
          if (p.isDefined) {
            vars.foreach(variable => {
              p.get.lookupVar(variable.id) match {
                case Some(x) => error("Field already defined in parent class: " + variable.getSymbol.name, variable)
                case None    =>
              }
            })
          }
          bind(vars)
          bind(methods)
        }
        case x @ VarDecl(tpe, id) => setType(tpe)
        case x @ MethodDecl(retType, id, args, vars, stats, retExpr) => {
          val parent = x.getSymbol.classSymbol.parent
          if (parent.isDefined) {
            parent.get.lookupMethod(id) match {
              case Some(parentMethod) => if (parentMethod.argList.size != x.getSymbol.argList.size)
                error("Method already declared in super class: " + id.value, id)
              case None =>
            }
          }
          setType(retType)
          bind(args)
          bind(vars)
          bind(x.getSymbol, stats, retExpr)
        }
        case Formal(tpe, id) => setType(tpe)
      }

      private def bind(s: Symbol, list: Any*): Unit = {
        list.foreach(_ match {
          case x: List[Tree]   => x.foreach(bind(s, _))
          case x: Tree         => bind(s, x)
          case x: Option[Tree] => if (x.isDefined) bind(s, x.get)
          case _               => throw new UnsupportedOperationException
        })
      }

      private def bind(s: Symbol, t: Tree): Unit = t match {
        // Statements
        case Block(stats)       => bind(s, stats)
        case If(expr, thn, els) => bind(s, expr, thn, els)
        case While(expr, stat)  => bind(s, expr, stat)
        case Println(expr)      => bind(s, expr)
        case Assign(id, expr) => {
          setVariable(id, s)
          bind(s, expr)
        }
        case ArrayAssign(id, index, expr) => {
          setVariable(id, s)
          bind(s, index, expr)
        }
        // Expressions
        case And(lhs, rhs)               => bind(s, lhs, rhs)
        case Or(lhs, rhs)                => bind(s, lhs, rhs)
        case Plus(lhs, rhs)              => bind(s, lhs, rhs)
        case Minus(lhs, rhs)             => bind(s, lhs, rhs)
        case Times(lhs, rhs)             => bind(s, lhs, rhs)
        case Div(lhs, rhs)               => bind(s, lhs, rhs)
        case LessThan(lhs, rhs)          => bind(s, lhs, rhs)
        case Equals(lhs, rhs)            => bind(s, lhs, rhs)
        case ArrayRead(arr, index)       => bind(s, arr, index)
        case ArrayLength(arr)            => bind(s, arr)
        case MethodCall(obj, meth, args) => bind(s, obj, args)
        case id @ Identifier(value)      => setVariable(id, s)
        case x @ This() => {
          s match {
            case classSymbol: ClassSymbol   => x.setSymbol(g.mainClass)
            case methodSymbol: MethodSymbol => x.setSymbol(methodSymbol.classSymbol)
          }
        }
        case NewIntArray(size) => bind(s, size)
        case New(tpe)          => setType(tpe)
        case Not(expr)         => bind(s, expr)
        case _                 =>
      }

      private def setClassSymbol(id: Identifier, errorStr: String): Unit = {
        g.lookupClass(id.value) match {
          case Some(x) => id.setSymbol(x)
          case None    => error(errorStr + id.value, id)
        }
      }

      private def setType(tpe: TypeTree): Unit = {
        if (tpe.isInstanceOf[Identifier]) {
          val id = tpe.asInstanceOf[Identifier]
          setClassSymbol(id, "Class not declared:")
        }
      }

      private def setVariable(id: Identifier, s: Symbol): Unit = {
        s match {
          case classSymbol: ClassSymbol => error("Variable not declared: " + id.value, id)
          case methodSymbol: MethodSymbol => {
            methodSymbol.lookupVar(id) match {
              case Some(symbol) => id.setSymbol(symbol)
              case None         => error("Variable not declared: " + id.value, id)
            }
          }
        }
      }

      private def setParent(id: Identifier, parent: Option[Identifier], classDecl: ClassDecl): Unit = {
        if (parent.isDefined) {
          val p = parent.get
          g.lookupClass(p.value) match {
            case Some(parentSymbol) => {
              p.setSymbol(parentSymbol)
              classDecl.getSymbol.parent = Some(parentSymbol)
            }
            case None => error("Parent class is not defined: " + p.value, p)
          }
        }
      }
    }
    def addSymbols(): Unit = Adder()
    def bindIdentifiers(): Unit = Binder()

    def checkForCycles(): Unit = {
      g.classes.foreach { x =>
        var classSymbol: Option[ClassSymbol] = Some(x._2)
        var set: Set[ClassSymbol] = Set()
        while (classSymbol.isDefined) {
          val c = classSymbol.get
          if (set.contains(c)) {
            error("The transitive closure of the \"extends\" relation must be irreflexive: " + c.name, c)
            return
          }
          set += c
          classSymbol = c.parent
        }

      }
    }
  }

  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._
    var nameAnalyzer = new NameAnalyser(ctx, prog, new GlobalScope)
    nameAnalyzer.addSymbols()
    nameAnalyzer.bindIdentifiers()
    nameAnalyzer.checkForCycles()

    prog
  }

}
