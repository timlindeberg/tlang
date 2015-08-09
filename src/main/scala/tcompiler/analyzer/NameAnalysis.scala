package tcompiler
package analyzer

import tcompiler.analyzer.Symbols._
import tcompiler.analyzer.Types._
import tcompiler.ast.TreeGroups.{IncrementDecrement, UnaryOperatorDecl, BinaryOperatorDecl}
import tcompiler.ast.Trees.{ClassDecl, _}
import tcompiler.ast.{Printer, Trees}
import tcompiler.utils._

object NameAnalysis extends Pipeline[Program, Program] {

  def run(ctx: Context)(prog: Program): Program = {
    val nameAnalyzer = new NameAnalyser(ctx, prog)
    nameAnalyzer.addSymbols()
    nameAnalyzer.bindIdentifiers()
    nameAnalyzer.checkInheritanceCycles()
    nameAnalyzer.checkVariableUsage()
    nameAnalyzer.checkOverrideAndOverLoadConstraints()
    prog
  }

}

class NameAnalyser(ctx: Context, prog: Program) {

  import ctx.reporter._

  private var variableUsage: Map[VariableSymbol, Boolean] = Map()

  val globalScope = new GlobalScope

  def addSymbols(): Unit = addSymbols(prog, globalScope)

  def bindIdentifiers(): Unit = bind(prog)

  def checkInheritanceCycles(): Unit = {
    globalScope.classes.foreach { x =>
      var classSymbol: Option[ClassSymbol] = Some(x._2)
      var set: Set[ClassSymbol] = Set()
      while (classSymbol.isDefined) {
        val c = classSymbol.get
        if (set.contains(c)) {
          ErrorInheritanceCycle(set, c, c)
          return
        }
        set += c
        classSymbol = c.parent
      }
    }
  }

  def checkOverrideAndOverLoadConstraints() = {
    prog.classes.foreach { klass =>
      klass.methods.foreach {
        case meth: MethodDecl             =>
          val methSymbol = meth.getSymbol
          val methName = meth.id.value
          val argTypes = meth.args.map(_.tpe.getType)
          methSymbol.classSymbol.parent match {
            case Some(parent) =>
              parent.lookupMethod(methName, argTypes) match {
                case Some(parentMethSymbol) =>
                  methSymbol.overridden = Some(parentMethSymbol)
                case None                   =>
              }
            case None         =>
          }
        case constructor: ConstructorDecl => // TODO
        case operator: OperatorDecl       =>
          val operatorSymbol = operator.getSymbol
          val argTypes = operator.args.map(_.tpe.getType)
          operatorSymbol.classSymbol.parent match {
            case Some(parent) =>
              parent.lookupOperator(operator.operatorType, argTypes) match {
                case Some(parentMethSymbol) =>
                  ErrorOverloadOperator(operator)
                case None                   =>
              }
            case None         =>
          }
      }
    }
  }

  def checkVariableUsage() = {
    variableUsage foreach {
      case (variable, used) =>
        if (!used)
          WarningUnused(variable.name, variable)
    }
  }

  private def addSymbols(t: Tree, globalScope: GlobalScope): Unit = t match {
    case Program(_, _, main, classes)                                                    =>
      main match {
        case Some(methDecl) =>
          globalScope.mainClass = new ClassSymbol(methDecl.id.value).setPos(methDecl.id)
          addSymbols(methDecl, globalScope.mainClass)
          methDecl.id.setType(TUnit)
        case _              =>
      }
      classes.foreach(addSymbols(_, globalScope))
    case classDecl @ ClassDecl(id @ ClassIdentifier(name, types), parent, vars, methods) =>
      if(name == globalScope.mainClass.name)
        ErrorSameNameAsMain(name, id)

      val newSymbol = new ClassSymbol(name).setPos(id)
      ensureIdentiferNotDefined(globalScope.classes, id.value, id)
      id.setSymbol(newSymbol)
      classDecl.setSymbol(newSymbol)
      globalScope.classes += (id.value -> newSymbol)
      vars.foreach(addSymbols(_, newSymbol))
      methods.foreach(addSymbols(_, newSymbol))
    case _                                                                               => throw new UnsupportedOperationException
  }

  private def addSymbols(t: Tree, classSymbol: ClassSymbol): Unit = t match {
    case varDecl @ VarDecl(tpe, id, init, _)                                          =>
      val newSymbol = new VariableSymbol(id.value, varDecl.modifiers, Some(classSymbol)).setPos(id)
      ensureIdentiferNotDefined(classSymbol.members, id.value, id)
      id.setSymbol(newSymbol)
      varDecl.setSymbol(newSymbol)
      variableUsage += newSymbol -> true
      classSymbol.members += (id.value -> newSymbol)
    case methodDecl @ MethodDecl(retType, id, args, vars, stats, _)                   =>
      val newSymbol = new MethodSymbol(id.value, classSymbol, methodDecl.modifiers).setPos(id)
      id.setSymbol(newSymbol)
      methodDecl.setSymbol(newSymbol)

      args.foreach(addSymbols(_, newSymbol))
      vars.foreach(addSymbols(_, newSymbol))
    case constructorDecl @ ConstructorDecl(id, args, vars, stats, _)                  =>
      val newSymbol = new MethodSymbol(id.value, classSymbol, constructorDecl.modifiers).setPos(id)
      newSymbol.setType(TUnit)

      id.setSymbol(newSymbol)
      constructorDecl.setSymbol(newSymbol)

      args.foreach(addSymbols(_, newSymbol))
      vars.foreach(addSymbols(_, newSymbol))
    case operatorDecl @ OperatorDecl(operatorType, retType, args, vars, stats, _, id) =>
      val newSymbol = new OperatorSymbol(operatorType, classSymbol, operatorDecl.modifiers)
      id.setSymbol(newSymbol)
      operatorDecl.setSymbol(newSymbol)

      args.foreach(addSymbols(_, newSymbol))
      vars.foreach(addSymbols(_, newSymbol))
  }

  private def addSymbols(t: Tree, methSymbol: MethodSymbol): Unit = t match {
    case varDecl @ VarDecl(tpe, id, init, _) =>
      val newSymbol = new VariableSymbol(id.value, varDecl.modifiers).setPos(id)
      ensureIdentiferNotDefined(methSymbol.members, id.value, id)
      if (methSymbol.params.contains(id.value)) {
        val oldSymbol = methSymbol.params(id.value)
        ErrorLocalVarShadowsParamater(id.value, oldSymbol.line, id)
      }
      id.setSymbol(newSymbol)
      varDecl.setSymbol(newSymbol)
      variableUsage += newSymbol -> true
      methSymbol.members += (id.value -> newSymbol)
    case formal @ Formal(tpe, id)            =>
      val newSymbol = new VariableSymbol(id.value).setPos(id)
      ensureIdentiferNotDefined(methSymbol.params, id.value, id)
      id.setSymbol(newSymbol)
      formal.setSymbol(newSymbol)
      methSymbol.params += (id.value -> newSymbol)
      methSymbol.argList ++= List(newSymbol)
    case _                                   => throw new UnsupportedOperationException
  }

  private def ensureIdentiferNotDefined[T <: Symbol](map: Map[String, T], id: String, pos: Positioned): Unit = {
    if (map.contains(id)) {
      val oldSymbol = map(id)
      ErrorVariableAlreadyDefined(id, oldSymbol.line, pos)
    }
  }

  private def bind(t: Tree): Unit = t match {
    case Program(_, _, main, classes)                                               =>
      main match {
        case Some(mainMethod) => bind(mainMethod)
        case _                =>
      }
      classes.foreach(bind)
    case classDecl @ ClassDecl(id, parent, vars, methods)                           =>
      setParent(id, parent, classDecl)
      val sym = classDecl.getSymbol
      sym.setType(TObject(sym))
      val p = classDecl.getSymbol.parent
      if (p.isDefined) {
        vars.foreach(varDecl => {
          val v = p.get.lookupVar(varDecl.id.value)
          if (v.isDefined)
            ErrorFieldDefinedInSuperClass(varDecl.getSymbol.name, varDecl)
        })
      }
      vars.foreach { case varDecl @ VarDecl(tpe, varId, init, _) =>
        setType(tpe, varId)
        init match {
          case Some(expr) => bind(expr, classDecl.getSymbol, varDecl.isStatic)
          case None       =>
        }
      }
      methods.foreach(bind)
    case methDecl @ MethodDecl(retType, _, args, vars, stat, _)                     =>
      setType(retType)

      methDecl.getSymbol.setType(retType.getType)

      args.foreach(bind)
      bindVars(vars, methDecl.getSymbol, isStatic = false)

      ensureMethodNotDefined(methDecl)

      bind(stat, methDecl.getSymbol, methDecl.isStatic)
    case constructorDecl @ ConstructorDecl(_, args, vars, stat, _)                  =>
      args.foreach(bind)
      bindVars(vars, constructorDecl.getSymbol, isStatic = false)

      ensureMethodNotDefined(constructorDecl)
      bind(stat, constructorDecl.getSymbol, isStatic = false)
    case operatorDecl @ OperatorDecl(operatorType, retType, args, vars, stat, _, _) =>
      setType(retType)

      operatorDecl.getSymbol.setType(retType.getType)
      operatorType.setType(retType.getType)
      operatorType match {
        case IncrementDecrement(id) => id.setSymbol(new VariableSymbol("")).setType(retType.getType)
        case _                      =>
      }
      args.foreach(bind)
      bindVars(vars, operatorDecl.getSymbol, operatorDecl.isStatic)

      ensureOperatorNotDefined(operatorDecl)

      bind(stat, operatorDecl.getSymbol, operatorDecl.isStatic)
    case Formal(tpe, id)                                                            => setType(tpe, id)
    case _                                                                          => throw new UnsupportedOperationException
  }

  private def bindVars(vars: List[VarDecl], symbol: MethodSymbol, isStatic: Boolean): Unit =
    vars.foreach { case VarDecl(tpe, id, init, _) =>
      setType(tpe, id)
      init match {
        case Some(expr) => bind(expr, symbol, isStatic)
        case None       =>
      }
    }

  private def bind(t: Tree, s: Symbol, isStatic: Boolean): Unit =
    Trees.traverse(t, (parent, current) => Some(current) collect {
      case m @ MethodCall(obj, meth, args) =>
        bind(obj, s, isStatic)
        args.foreach(bind(_, s, isStatic))
      case Instance(expr, id)              =>
        bind(expr, s, isStatic)
        globalScope.lookupClass(id.value) match {
          case Some(classSymbol) =>
            id.setSymbol(classSymbol)
            id.setType(TObject(classSymbol))
          case None              => ErrorTypeNotDeclared(id.value, id)
        }
      case FieldRead(obj, id)              =>
        bind(obj, s, isStatic)
      case FieldAssign(obj, id, expr)      =>
        bind(obj, s, isStatic)
        bind(expr, s, isStatic)
      case id: Identifier                  => parent match {
        case _: MethodCall  =>
        case _: Instance    =>
        case _: FieldRead   =>
        case _: FieldAssign =>
        case _              => setVariable(id, s, isStatic)
      }
      case id: ClassIdentifier             => setType(id)
      case NewArray(tpe, size)             => setType(tpe)
      case thisSym: This                   =>
        s match {
          case classSymbol: ClassSymbol   => thisSym.setSymbol(globalScope.mainClass)
          case methodSymbol: MethodSymbol => thisSym.setSymbol(methodSymbol.classSymbol)
          case _                          => throw new UnsupportedOperationException
        }
    })

  private def ensureMethodNotDefined(meth: FuncTree): Unit = {
    val name = meth.id.value
    val argTypes = meth.args.map(_.tpe.getType)
    meth.getSymbol.classSymbol.lookupMethod(name, argTypes, recursive = false) match {
      case Some(oldMeth) =>
        ErrorMethodAlreadyDefined(meth.signature, oldMeth.line, meth)
      case None          =>
        meth.getSymbol.classSymbol.addMethod(meth.getSymbol)
    }
  }

  private def ensureOperatorNotDefined(operator: OperatorDecl): Unit = {
    val operatorType = operator.operatorType
    val argTypes = operator.args.map(_.tpe.getType)

    operatorType match {
      case UnaryOperatorDecl(expr) if argTypes.size != 1      =>
        ErrorOperatorWrongNumArguments(Printer(operatorType), 1, argTypes.size, operator)
      case BinaryOperatorDecl(lhs, rhs) if argTypes.size != 2 =>
        ErrorOperatorWrongNumArguments(Printer(operatorType), 2, argTypes.size, operator)
      case _                                                  =>
    }

    operator.getSymbol.classSymbol.lookupOperator(operatorType, argTypes, recursive = true) match {
      case Some(oldOperator) =>
        ErrorOperatorAlreadyDefined(Trees.operatorString(operatorType, argTypes), oldOperator.line, operator)
      case None              =>
        operator.getSymbol.classSymbol.addOperator(operator.getSymbol.asInstanceOf[OperatorSymbol])
    }
  }

  private def setType(tpe: TypeTree, id: Identifier): Unit = {
    def set(t: Type): Unit = {
      id.setType(t)
      tpe.setType(t)
    }
    tpe match {
      case tpeId @ ClassIdentifier(typeName, _) =>
        globalScope.lookupClass(typeName) match {
          case Some(classSymbol) =>
            tpeId.setSymbol(classSymbol)
            set(TObject(classSymbol))
          case None              =>
            ErrorTypeNotDeclared(tpeId.value, tpeId)
        }
      case BooleanType()                        => set(TBool)
      case IntType()                            => set(TInt)
      case LongType()                           => set(TLong)
      case FloatType()                          => set(TFloat)
      case DoubleType()                         => set(TDouble)
      case CharType()                           => set(TChar)
      case StringType()                         => set(TString)
      case UnitType()                           => set(TUnit)
      case ArrayType(arrayTpe)                  =>
        setType(arrayTpe)
        set(TArray(arrayTpe.getType))
    }
  }

  private def setType(tpe: TypeTree): Unit = {
    tpe match {
      case tpeId @ ClassIdentifier(typeName, _) =>
        globalScope.lookupClass(typeName) match {
          case Some(classSymbol) =>
            tpeId.setSymbol(classSymbol)
            tpeId.setType(TObject(classSymbol))
          case None              =>
            ErrorTypeNotDeclared(tpeId.value, tpeId)
            ErrorTypeNotDeclared(tpeId.value, tpeId)
        }
      case BooleanType()                        => tpe.setType(TBool)
      case IntType()                            => tpe.setType(TInt)
      case LongType()                           => tpe.setType(TLong)
      case FloatType()                          => tpe.setType(TFloat)
      case DoubleType()                         => tpe.setType(TDouble)
      case CharType()                           => tpe.setType(TChar)
      case StringType()                         => tpe.setType(TString)
      case UnitType()                           => tpe.setType(TUnit)
      case ArrayType(arrayTpe)                  =>
        setType(arrayTpe)
        tpe.setType(TArray(arrayTpe.getType))
    }
  }

  private def setVariable(id: Identifier, s: Symbol, isStatic: Boolean): Unit = {
    val name = id.value
    s match {
      case methodSymbol: MethodSymbol =>
        globalScope.lookupClass(name) match {
          case Some(symbol) => id.setSymbol(symbol)
          case None         => methodSymbol.lookupLocalVar(name) match {
            case Some(symbol) =>
              id.setSymbol(symbol)
              variableUsage += symbol.asInstanceOf[VariableSymbol] -> true
            case None         => methodSymbol.lookupField(name) match {
              case Some(symbol) =>
                if (isStatic && !symbol.isStatic)
                  ErrorAccessNonStaticFromStatic(id.value, id)
                id.setSymbol(symbol)
                variableUsage += symbol.asInstanceOf[VariableSymbol] -> true
              case None         => ErrorCantResolveSymbol(id.value, id)
            }
          }
        }

      case classSymbol: ClassSymbol =>
        classSymbol.lookupVar(id.value) match {
          case Some(symbol) =>
            id.setSymbol(symbol)
            variableUsage += symbol.asInstanceOf[VariableSymbol] -> true
          case None         => ErrorCantResolveSymbol(id.value, id)
        }
    }
  }

  private def setParent(id: ClassIdentifier, parent: Option[ClassIdentifier], classDecl: ClassDecl): Unit =
    parent match {
      case Some(parentId) =>
        globalScope.lookupClass(parentId.value) match {
          case Some(parentSymbol) =>
            parentId.setSymbol(parentSymbol)
            classDecl.getSymbol.parent = Some(parentSymbol)
          case None               =>
            ErrorParentNotDeclared(parentId.value, parentId)
        }
      case _              =>
    }


  private def error(msg: String, tree: Positioned) = {
    tree match {
      case id: Identifier      => id.setSymbol(new ErrorSymbol)
      case id: ClassIdentifier => id.setSymbol(new ErrorSymbol)
      case _                   =>
    }

    ctx.reporter.error(msg, tree)
  }

  //---------------------------------------------------------------------------------------
  //  Error messages
  //---------------------------------------------------------------------------------------


  private def ErrorInheritanceCycle(set: Set[ClassSymbol], c: ClassSymbol, pos: Positioned) = {
    val inheritanceList =
      (if (set.size >= 2) set.map(_.name).mkString(" <: ")
      else c.name) + " <: " + c.name
    error(s"A cycle was found in the inheritence graph: $inheritanceList", pos)
  }

  private def ErrorOverloadOperator(pos: Positioned) =
    error("Overloaded operators cannot be overriden.", pos)

  private def ErrorSameNameAsMain(name: String, pos: Positioned) =
    error(s"Class '$name' has the same name as the main object.", pos)

  private def ErrorLocalVarShadowsParamater(name: String, line: Int, pos: Positioned) =
    error(s"Local variable '$name' shadows method parameter defined at line $line.", pos)

  private def ErrorVariableAlreadyDefined(name: String, line: Int, pos: Positioned) =
    error(s"Variable '$name' is already defined at line $line.", pos)

  private def ErrorFieldDefinedInSuperClass(name: String, pos: Positioned) =
    error(s"Field '$name' is already defined in super class.", pos)

  private def ErrorTypeNotDeclared(name: String, pos: Positioned) =
    error(s"Type '$name' was not declared.", pos)

  private def ErrorMethodAlreadyDefined(methodSignature: String, line: Int, pos: Positioned) =
    error(s"Method '$methodSignature' is already defined at line $line.", pos)

  private def ErrorOperatorWrongNumArguments(operator: String, expected: Int, found: Int, pos: Positioned) =
    error(s"Operator '$operator' has wrong number of arguments. Expected $expected argument(s), found $found.", pos)

  private def ErrorOperatorAlreadyDefined(operator: String, line: Int, pos: Positioned) =
    error(s"Operator '$operator' is already defined at line $line.", pos)

  private def ErrorCantResolveSymbol(name: String, pos: Positioned) =
    error(s"Can not resolve symbol '$name'.", pos)

  private def ErrorAccessNonStaticFromStatic(name: String, pos: Positioned) =
    error(s"Non-static field '$name' cannot be accessed from a static function.", pos)

  private def ErrorParentNotDeclared(name: String, pos: Positioned) =
    error(s"Parent class '$name' was not declared. ", pos)

  private def WarningUnused(name: String, pos: Positioned) =
    warning(s"Variable '$name' is declared but never used:", pos)

}
