package koolc
package analyzer

import koolc.ast.Trees
import utils._
import ast.Trees._
import Symbols._
import Types._

object NameAnalysis extends Pipeline[Program, Program] {

  def run(ctx: Context)(prog: Program): Program = {
    val nameAnalyzer = new NameAnalyser(ctx, prog, new GlobalScope)
    nameAnalyzer.addSymbols.bindIdentifiers
    nameAnalyzer.checkInheritanceCycles.checkVariableUsage.checkOverrideConstraints
    prog
  }

  class NameAnalyser(ctx: Context, prog: Program, g: GlobalScope) {

    import ctx.reporter._

    var variableUsage: Map[VariableSymbol, Boolean] = Map()

    def addSymbols(): this.type = {
      SymbolAdder();
      this
    }

    def bindIdentifiers(): this.type = {
      SymbolBinder();
      this
    }

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

    def error(msg: String, tree: Positioned) = {
      if (tree.isInstanceOf[Identifier])
        tree.asInstanceOf[Identifier].setSymbol(new ErrorSymbol)

      ctx.reporter.error(msg, tree)
    }

    def checkOverrideConstraints(): this.type = {
      prog.classes.foreach { klass =>
        klass.methods.foreach(_ match {
          case meth: MethodDecl =>
            val parent = meth.getSymbol.classSymbol.parent
            if (parent.isDefined) {
              val parentMeth = parent.get.lookupMethod(meth.id.value)
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
          case constructor: ConstructorDecl => // TODO
        }
        )

      }
      this
    }

    def checkVariableUsage(): this.type = {
      variableUsage foreach {
        case (variable, used) =>
          if (!used) warning("Variable \'" + variable.name + "\' declared but is never used:", variable)
      }
      this
    }

    object SymbolAdder {
      def apply(): Unit = addSymbols(prog, g)

      private def addSymbols(t: Tree, globalScope: GlobalScope): Unit = t match {
        case Program(main, classes) => {
          addSymbols(main, globalScope)
          classes.foreach(addSymbols(_, globalScope))
        }
        case mainObject@MainObject(id, stats) => {
          globalScope.mainClass = new ClassSymbol(id.value).setPos(id)
          mainObject.setSymbol(globalScope.mainClass)
          id.setSymbol(globalScope.mainClass)
        }
        case classDecl@ClassDecl(id@TypeIdentifier(name, types), parent, vars, methods) => {
          val newSymbol = new ClassSymbol(name).setPos(id)
          ensureIdentiferNotDefined(globalScope.classes, id.value, id)
          id.setSymbol(newSymbol)
          classDecl.setSymbol(newSymbol)
          globalScope.classes += (id.value -> newSymbol)
          vars.foreach(addSymbols(_, newSymbol))
          methods.foreach(addSymbols(_, newSymbol))

          if (name == globalScope.mainClass.name) {
            error("Class \'" + name + "\' has the same name as the main object.", id)
          }
        }
        case _ => throw new UnsupportedOperationException
      }

      private def addSymbols(t: Tree, s: ClassSymbol): Unit = t match {
        case varDecl@VarDecl(tpe, id) => {
          val newSymbol = new VariableSymbol(id.value).setPos(id)
          ensureIdentiferNotDefined(s.members, id.value, id)
          id.setSymbol(newSymbol)
          varDecl.setSymbol(newSymbol)
          variableUsage += newSymbol -> true
          s.members += (id.value -> newSymbol)
        }
        case methodDecl@MethodDecl(retType, id, args, vars, stats, retExpr) => {
          val newSymbol = new MethodSymbol(id.value, s).setPos(id)
          ensureIdentiferNotDefined(s.methods, id.value, id)
          id.setSymbol(newSymbol)
          methodDecl.setSymbol(newSymbol)
          s.methods += (id.value -> newSymbol)
          args.foreach(addSymbols(_, newSymbol))
          vars.foreach(addSymbols(_, newSymbol))
        }
        case constructorDecl@ConstructorDecl(id, args, vars, stats) => {
          val newSymbol = new MethodSymbol(id.value, s).setPos(id)
          newSymbol.setType(TObject(s))

          ensureIdentiferNotDefined(s.methods, id.value, id)

          id.setSymbol(newSymbol)
          constructorDecl.setSymbol(newSymbol)

          s.methods += (id.value -> newSymbol)
          args.foreach(addSymbols(_, newSymbol))
          vars.foreach(addSymbols(_, newSymbol))
          if(id.value != s.name) {
            error("Invalid constructor name: \'" + id.value + "\'. Needs to have the same name as the class it's declared in: \'" + s.name + "\'.", id)
          }
        }
        case _ => throw new UnsupportedOperationException
      }

      private def addSymbols(t: Tree, s: MethodSymbol): Unit = t match {
        case varDecl@VarDecl(tpe, id) => {
          val newSymbol = new VariableSymbol(id.value).setPos(id)
          ensureIdentiferNotDefined(s.members, id.value, id)
          if (s.params.contains(id.value)) {
            val oldSymbol = s.params(id.value)
            error("Local variable \'" + id.value + "\' shadows method parameter defined at " + oldSymbol.line + ":" + oldSymbol.col, id)
          }
          id.setSymbol(newSymbol)
          varDecl.setSymbol(newSymbol)
          variableUsage += newSymbol -> true
          s.members += (id.value -> newSymbol)
        }
        case formal@Formal(tpe, id) => {
          val newSymbol = new VariableSymbol(id.value).setPos(id)
          ensureIdentiferNotDefined(s.params, id.value, id)
          id.setSymbol(newSymbol)
          formal.setSymbol(newSymbol)
          s.params += (id.value -> newSymbol)
          s.argList ++= List(newSymbol)
        }
        case _ => throw new UnsupportedOperationException
      }

      private def ensureIdentiferNotDefined[T <: Symbol](map: Map[String, T], id: String, pos: Positioned): Unit = {
        if (map.contains(id)) {
          val oldSymbol = map(id)
          error("Variable \'" + id + "\' is already defined at " + oldSymbol.line + ":" + oldSymbol.col, pos)
        }
      }

    }

    object SymbolBinder {
      def apply(): Unit = bind(prog)

      private def bind(list: List[Tree]): Unit = list.foreach(bind)

      private def bind(t: Tree): Unit = t match {
        case Program(main, classes) =>
          bind(main); bind(classes)
        case main@MainObject(id, stats) => bind(main.getSymbol, stats)
        case classDecl@ClassDecl(id, parent, vars, methods) => {
          setParent(id, parent, classDecl)
          val sym = classDecl.getSymbol
          sym.setType(TObject(sym))
          val p = classDecl.getSymbol.parent
          if (p.isDefined) {
            vars.foreach(variable => {
              val v = p.get.lookupVar(variable.id.value)
              if (v.isDefined)
                error("Field \'" + variable.getSymbol.name + "\' already defined in super class: ", variable)
            })
          }
          bind(vars)
          bind(methods)
        }
        case VarDecl(tpe, id) => setType(tpe, id)
        case methDecl@MethodDecl(retType, id, args, vars, stats, retExpr) => {
          setType(retType)

          methDecl.getSymbol.setType(retType.getType)

          bind(args)
          bind(vars)
          bind(methDecl.getSymbol, stats, retExpr)
        }
        case constructorDecl@ConstructorDecl(id, args, vars, stats) => {
          bind(args)
          bind(vars)
          bind(constructorDecl.getSymbol, stats)
        }
        case Formal(tpe, id) => setType(tpe, id)
        case _ => throw new UnsupportedOperationException
      }

      private def bind(s: Symbol, list: Any*): Unit = {
        list.foreach(_ match {
          case x: List[_] => x.foreach(bind(s, _))
          case x: Tree => bind(s, x)
          case x: Option[_] => if (x.isDefined) bind(s, x.get)
          case _ => throw new UnsupportedOperationException
        })
      }

      private def bind(s: Symbol, t: Tree): Unit = t match {
          // Statements
          case Block(stats) => bind(s, stats)
          case If(expr, thn, els) => bind(s, expr, thn, els)
          case While(expr, stat) => bind(s, expr, stat)
          case Println(expr) => bind(s, expr)
          case Assign(id, expr) =>
            setVariable(id, s)
            bind(s, expr)
          case ArrayAssign(id, index, expr) =>
            setVariable(id, s)
            bind(s, index, expr)
          // Expressions
          case MathExpr(lhs, rhs) => bind(s, lhs, rhs)
          case Comparison(lhs, rhs) => bind(s, lhs, rhs)
          case ArrayRead(arr, index) => bind(s, arr, index)
          case ArrayLength(arr) => bind(s, arr)
          case MethodCall(obj, meth, args) => bind(s, obj, args)
          case id@Identifier(value) => setVariable(id, s)
          case id@TypeIdentifier(value, _) => setVariable2(id, s)
          case thisSym@This() =>
            s match {
              case classSymbol: ClassSymbol => thisSym.setSymbol(g.mainClass)
              case methodSymbol: MethodSymbol => thisSym.setSymbol(methodSymbol.classSymbol)
              case _ => throw new UnsupportedOperationException
            }
          case NewIntArray(size) => bind(s, size)
          case New(tpe, args) =>
            setType(tpe)
            bind(s, args)
          case Not(expr) => bind(s, expr)
          case _ => // Do nothing
        }


      private def setType(tpe: TypeTree, id: Identifier): Unit = {
        def set(t: Type): Unit = {
          id.setType(t)
          tpe.setType(t)
        }
        tpe match {
          case tpeId@TypeIdentifier(typeName, _) =>
            g.lookupClass(typeName) match {
              case Some(classSymbol) =>
                tpeId.setSymbol(classSymbol)
                set(TObject(classSymbol))
              case None =>
                tpeId.setSymbol(new ErrorSymbol)
                error("Type \'" + tpeId.value + "\' was not declared:", tpeId)
            }
          case BooleanType() => set(TBool)
          case IntType() => set(TInt)
          case IntArrayType() => set(TIntArray)
          case StringType() => set(TString)
        }
      }

      private def setType(tpe: TypeTree): Unit = {
        tpe match {
          case tpeId@TypeIdentifier(typeName, _) =>
            g.lookupClass(typeName) match {
              case Some(classSymbol) =>
                tpeId.setSymbol(classSymbol)
                tpeId.setType(TObject(classSymbol))
              case None =>
                tpeId.setSymbol(new ErrorSymbol)
                error("Type \'" + tpeId.value + "\' was not declared:", tpeId)
            }
          case BooleanType() => tpe.setType(TBool)
          case IntType() => tpe.setType(TInt)
          case IntArrayType() => tpe.setType(TIntArray)
          case StringType() => tpe.setType(TString)
          case UnitType() => tpe.setType(TUnit)
        }
      }

      private def setVariable(id: Identifier, s: Symbol): Unit = {
        def errorMsg(name: String) = "Variable \'" + name + "\' was not declared: "
        s match {
          case methodSymbol: MethodSymbol =>
            methodSymbol.lookupVar(id.value) match {
              case Some(symbol) =>
                id.setSymbol(symbol)
                variableUsage += symbol.asInstanceOf[VariableSymbol] -> true
              case None => error(errorMsg(id.value), id)
            }
          case _ => throw new UnsupportedOperationException
        }
      }

      private def setVariable2(id: TypeIdentifier, s: Symbol): Unit = {
        def errorMsg(name: String) = "Variable \'" + name + "\' was not declared: "
        s match {
          case methodSymbol: MethodSymbol =>
            methodSymbol.lookupVar(id.value) match {
              case Some(symbol) =>
                id.setSymbol(symbol)
                variableUsage += symbol.asInstanceOf[VariableSymbol] -> true
              case None => error(errorMsg(id.value), id)
            }
          case _ => throw new UnsupportedOperationException
        }
      }

      private def setParent(id: TypeIdentifier, parent: Option[TypeIdentifier], classDecl: ClassDecl): Unit = {
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

  }

}
