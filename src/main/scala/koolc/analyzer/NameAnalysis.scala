package koolc
package analyzer

import utils._
import ast.Trees._
import Symbols._
import Types._

object NameAnalysis extends Pipeline[Program, Program] {

  class NameAnalyser(ctx: Context, prog: Program, g: GlobalScope) {
    import ctx.reporter._

    var variableUsage: Map[VariableSymbol, Boolean] = Map()

    object SymbolAdder {
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
        case x @ ClassDecl(id @ Identifier(name), parent, vars, methods) => {
          val newSymbol = new ClassSymbol(name).setPos(id)
          s.classes = addTo(s.classes, newSymbol, id, x)
          vars.foreach(addSymbols(_, newSymbol))
          methods.foreach(addSymbols(_, newSymbol))

          if (name == s.mainClass.name) {
            error("Class \'" + name + "\' has the same name as the main object.", id)
          }
        }
        case _ => throw new UnsupportedOperationException
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
        }
        case _ => throw new UnsupportedOperationException
      }

      private def addSymbols(t: Tree, s: MethodSymbol): Unit = t match {
        case x @ VarDecl(tpe, id) => {
          val newSymbol = new VariableSymbol(id.value).setPos(id)
          if (s.params.contains(id.value)) {
            val oldSymbol = s.params(id.value)
            error("Local variable \'" + id.value + "\' shadows method parameter defined at " + oldSymbol.line + ":" + oldSymbol.col, id)
          }
          s.members = addTo(s.members, newSymbol, id, x)
        }
        case x @ Formal(tpe, id) => {
          val newSymbol = new VariableSymbol(id.value).setPos(id)
          val used = true
          s.params = addTo(s.params, newSymbol, id, x, used)
          s.argList ++= List(newSymbol)

        }
        case _ => throw new UnsupportedOperationException
      }

      private def addTo[T <: Symbol](map: Map[String, T], symbol: T, id: Identifier, x: Symbolic[T], used: Boolean = false): Map[String, T] = {
        if (map.contains(id.value)) {
          val oldSymbol = map(id.value)
          error("Variable \'" + id.value + "\' is already defined at " + oldSymbol.line + ":" + oldSymbol.col, id)
        }
        id.setSymbol(symbol)
        x.setSymbol(symbol)
        if (symbol.isInstanceOf[VariableSymbol])
          variableUsage += symbol.asInstanceOf[VariableSymbol] -> used

        map + (id.value -> symbol)
      }
    }
    object SymbolBinder {
      def apply(): Unit = bind(prog)
      private def bind(t: Option[Tree]): Unit = if (t.isDefined) bind(t.get)

      private def bind(list: List[Tree]): Unit = list.foreach(bind)

      private def bind(t: Tree): Unit = t match {
        case Program(main, classes) =>
          bind(main); bind(classes)
        case main @ MainObject(id, stats) => bind(main.getSymbol, stats)
        case classDecl @ ClassDecl(id, parent, vars, methods) => {
          setParent(id, parent, classDecl)
          val sym = classDecl.getSymbol
          sym.setType(TObject(sym))
          val p = classDecl.getSymbol.parent
          if (p.isDefined) {
            vars.foreach(variable => {
              val v = p.get.lookupVar(variable.id)
              if (v.isDefined)
                error("Field \'" + variable.getSymbol.name + "\' already defined in super class: ", variable)
            })
          }
          bind(vars)
          bind(methods)
        }
        case VarDecl(tpe, id) => setType(tpe, id)
        case methDecl @ MethodDecl(retType, id, args, vars, stats, retExpr) => {
          setType(retType)
          methDecl.getSymbol.setType(retType.getType)

          bind(args)
          bind(vars)
          bind(methDecl.getSymbol, stats, retExpr)
        }
        case Formal(tpe, id) => setType(tpe, id)
        case _               => throw new UnsupportedOperationException
      }

      private def bind(s: Symbol, list: Any*): Unit = {
        list.foreach(_ match {
          case x: List[_]   => x.foreach(bind(s, _))
          case x: Tree      => bind(s, x)
          case x: Option[_] => if (x.isDefined) bind(s, x.get)
          case _            => throw new UnsupportedOperationException
        })
      }

      private def bind(s: Symbol, t: Tree): Unit = t match {
        // Statements
        case Block(stats)       => bind(s, stats)
        case If(expr, thn, els) => bind(s, expr, thn, els)
        case While(expr, stat)  => bind(s, expr, stat)
        case Println(expr)      => bind(s, expr)
        case Assign(id, expr) =>
          setVariable(id, s)
          bind(s, expr)
        case ArrayAssign(id, index, expr) =>
          setVariable(id, s)
          bind(s, index, expr)
        // Expressions
        case And(lhs, rhs)         => bind(s, lhs, rhs)
        case Or(lhs, rhs)          => bind(s, lhs, rhs)
        case Plus(lhs, rhs)        => bind(s, lhs, rhs)
        case Minus(lhs, rhs)       => bind(s, lhs, rhs)
        case Times(lhs, rhs)       => bind(s, lhs, rhs)
        case Div(lhs, rhs)         => bind(s, lhs, rhs)
        case LessThan(lhs, rhs)    => bind(s, lhs, rhs)
        case Equals(lhs, rhs)      => bind(s, lhs, rhs)
        case ArrayRead(arr, index) => bind(s, arr, index)
        case ArrayLength(arr)      => bind(s, arr)
        case MethodCall(obj, meth, args) =>

          bind(s, obj, args)
        case id @ Identifier(value) => setVariable(id, s)
        case thisSym @ This() =>
          s match {
            case classSymbol: ClassSymbol   => thisSym.setSymbol(g.mainClass)
            case methodSymbol: MethodSymbol => thisSym.setSymbol(methodSymbol.classSymbol)
            case _                          => throw new UnsupportedOperationException
          }
        case NewIntArray(size) => bind(s, size)
        case New(tpe)          => setType(tpe)
        case Not(expr)         => bind(s, expr)
        case _                 => // Do nothing
      }

      private def setType(tpe: TypeTree, id: Identifier): Unit = {
        def set(t: Type): Unit = {
          id.setType(t)
          tpe.setType(t)
        }
        tpe match {
          case tpeId @ Identifier(value) =>
            g.lookupClass(tpeId) match {
              case Some(classSymbol) =>
                tpeId.setSymbol(classSymbol)
                set(TObject(classSymbol))
              case None => error("Type \'" + id.value + "\' was not declared:", id)
            }
          case BooleanType()  => set(TBool)
          case IntType()      => set(TInt)
          case IntArrayType() => set(TIntArray)
          case StringType()   => set(TString)
        }
      }

      private def setType(tpe: TypeTree): Unit = {
        tpe match {
          case tpeId @ Identifier(value) =>
            g.lookupClass(tpeId) match {
              case Some(classSymbol) =>
                tpeId.setSymbol(classSymbol)
                tpeId.setType(TObject(classSymbol))
              case None => error("Type \'" + tpeId.value + "\' was not declared:", tpeId)
            }
          case BooleanType()  => tpe.setType(TBool)
          case IntType()      => tpe.setType(TInt)
          case IntArrayType() => tpe.setType(TIntArray)
          case StringType()   => tpe.setType(TString)
        }
      }

      private def setVariable(id: Identifier, s: Symbol): Unit = {
        def errorMsg(name: String) = "Variable \'" + name + "\' was not declared: "
        s match {
          case methodSymbol: MethodSymbol =>
            methodSymbol.lookupVar(id) match {
              case Some(symbol) =>
                id.setSymbol(symbol)
                variableUsage += symbol.asInstanceOf[VariableSymbol] -> true
              case None => error(errorMsg(id.value), id)
            }
          case _ => throw new UnsupportedOperationException
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
            case None => error("Parent class \'" + p.value + "\' is not declared: ", p)
          }
        }
      }
    }

    def addSymbols(): this.type = { SymbolAdder(); this }
    def bindIdentifiers(): this.type = { SymbolBinder(); this }

    def checkInheritanceCycles(): this.type = {

      def inheritanceList(set: Set[ClassSymbol], c: ClassSymbol): String =
        (if (set.size >= 2) set.map(_.name).mkString(" <: ")
        else c.name) + " <: " + c.name

      g.classes.foreach { x =>
        var classSymbol: Option[ClassSymbol] = Some(x._2)
        var set: Set[ClassSymbol] = Set()
        while (classSymbol.isDefined) {
          val c = classSymbol.get
          if (set.contains(c)) {
            error("A cycle was found in the inheritence graph: " + inheritanceList(set, c), c)
            return this
          }
          set += c
          classSymbol = c.parent
        }
      }
      this
    }

    def checkOverrideConstraints(): this.type = {
      prog.classes.foreach { klass =>
        klass.methods.foreach { meth =>
          val parent = meth.getSymbol.classSymbol.parent
          if (parent.isDefined) {
            val parentMeth = parent.get.lookupMethod(meth.id)
            if (parentMeth.isDefined) {
              val (methSymbol, parentMethSymbol) = (meth.getSymbol, parentMeth.get)
              val (list1, list2) = (methSymbol.argList, parentMethSymbol.argList)
              val sameSize = list1.size == list2.size
              val sameArgs = list1.zip(list2).forall { case (x, y) => x.getType == y.getType }
              val sameRetType = methSymbol.getType == parentMethSymbol.getType
              if (sameSize && sameArgs && sameRetType)
                methSymbol.overridden = Some(parentMethSymbol)
              else
                error("Method \'" + meth.id + "\' is already declared in super class: ", meth.id)
            }
          }
        }
      }
      this
    }

    def checkVariableUsage(): this.type = {
      variableUsage map {
        case (variable, used) =>
          if (!used) warning("Variable \'" + variable.name + "\' declared but is never used:", variable)
      }
      this
    }

    def error(msg: String, tree: Positioned) = {
      if (tree.isInstanceOf[Identifier])
        tree.asInstanceOf[Identifier].setSymbol(new ErrorSymbol)

      ctx.reporter.error(msg, tree)
    }
  }

  def run(ctx: Context)(prog: Program): Program = {
    var nameAnalyzer = new NameAnalyser(ctx, prog, new GlobalScope)
    nameAnalyzer.addSymbols.bindIdentifiers
    nameAnalyzer.checkInheritanceCycles.checkVariableUsage.checkOverrideConstraints
    prog
  }

}
