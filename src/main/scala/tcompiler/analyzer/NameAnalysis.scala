package tcompiler
package analyzer

import tcompiler.analyzer.Symbols._
import tcompiler.analyzer.Types._
import tcompiler.ast.TreeGroups.{PrintStatement, IncrementDecrement, UnaryOperatorDecl, BinaryOperatorDecl}
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
    def checkInheritanceCycles(c: Option[ClassSymbol], set: Set[ClassSymbol]): Unit = c match {
      case Some(classSymbol) =>
        if (set.contains(classSymbol))
          ErrorInheritanceCycle(set, classSymbol, classSymbol)
        else
          checkInheritanceCycles(classSymbol.parent, set + classSymbol)
      case None              =>
    }

    globalScope.classes.foreach { case (_, classSymbol) => checkInheritanceCycles(Some(classSymbol), Set()) }
  }

  def checkOverrideAndOverLoadConstraints() =
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
      if (name == globalScope.mainClass.name)
        ErrorSameNameAsMain(name, id)

      val newSymbol = new ClassSymbol(name).setPos(id)
      ensureIdentiferNotDefined(globalScope.classes, id.value, id)
      id.setSymbol(newSymbol)
      classDecl.setSymbol(newSymbol)
      globalScope.classes += (id.value -> newSymbol)
      vars.foreach(addSymbols(_, newSymbol))
      methods.foreach(addSymbols(_, newSymbol))
  }

  private def addSymbols(t: Tree, classSymbol: ClassSymbol): Unit = t match {
    case varDecl @ VarDecl(tpe, id, init, _)                                    =>
      val newSymbol = new VariableSymbol(id.value, varDecl.modifiers, Some(classSymbol)).setPos(id)
      ensureIdentiferNotDefined(classSymbol.members, id.value, id)
      id.setSymbol(newSymbol)
      varDecl.setSymbol(newSymbol)
      variableUsage += newSymbol -> true
      classSymbol.members += (id.value -> newSymbol)
    case methodDecl @ MethodDecl(retType, id, args, stats, _)                   =>
      val newSymbol = new MethodSymbol(id.value, classSymbol, methodDecl.modifiers).setPos(id)
      id.setSymbol(newSymbol)
      methodDecl.setSymbol(newSymbol)

      args.foreach(addSymbols(_, newSymbol))
    case constructorDecl @ ConstructorDecl(id, args, stats, _)                  =>
      val newSymbol = new MethodSymbol(id.value, classSymbol, constructorDecl.modifiers).setPos(id)
      newSymbol.setType(TUnit)

      id.setSymbol(newSymbol)
      constructorDecl.setSymbol(newSymbol)

      args.foreach(addSymbols(_, newSymbol))
    case operatorDecl @ OperatorDecl(operatorType, retType, args, stats, _, id) =>
      val newSymbol = new OperatorSymbol(operatorType, classSymbol, operatorDecl.modifiers)
      id.setSymbol(newSymbol)
      operatorDecl.setSymbol(newSymbol)

      args.foreach(addSymbols(_, newSymbol))
  }

  private def addSymbols(t: Tree, methSymbol: MethodSymbol): Unit = t match {
    case formal @ Formal(tpe, id) =>
      val newSymbol = new VariableSymbol(id.value).setPos(id)
      ensureIdentiferNotDefined(methSymbol.params, id.value, id)
      id.setSymbol(newSymbol)
      formal.setSymbol(newSymbol)
      methSymbol.params += (id.value -> newSymbol)
      methSymbol.argList ++= List(newSymbol)
  }


  private def bind(tree: Tree): Unit = tree match {
    case Program(_, _, main, classes)                                         =>
      main match {
        case Some(mainMethod) => bind(mainMethod)
        case _                =>
      }
      classes.foreach(bind)
    case classDecl @ ClassDecl(id, parent, vars, methods)                     =>
      setParentSymbol(id, parent, classDecl)
      val sym = classDecl.getSymbol
      sym.setType(TObject(sym))

      checkConflictingFieldsInParent(vars, classDecl.getSymbol.parent)
      bindFields(classDecl)
      methods.foreach(bind)
    case methDecl @ MethodDecl(retType, _, args, stat, _)                     =>
      setType(retType)

      methDecl.getSymbol.setType(retType.getType)

      bindArguments(args)
      ensureMethodNotDefined(methDecl)

      new StatementBinder(methDecl.getSymbol, methDecl.isStatic).bindStatement(stat)
    case constructorDecl @ ConstructorDecl(_, args, stat, _)                  =>

      bindArguments(args)
      ensureMethodNotDefined(constructorDecl)

      new StatementBinder(constructorDecl.getSymbol, false).bindStatement(stat)
    case operatorDecl @ OperatorDecl(operatorType, retType, args, stat, _, _) =>
      setType(retType)

      operatorDecl.getSymbol.setType(retType.getType)
      operatorType.setType(retType.getType)
      bindArguments(args)

      ensureOperatorNotDefined(operatorDecl)

      new StatementBinder(operatorDecl.getSymbol, operatorDecl.isStatic).bindStatement(stat)
  }


  private def bindArguments(args: List[Formal]) =
    args.foreach { case Formal(typeTree, id) =>
      val tpe = setType(typeTree)
      id.setType(tpe)
    }

  private def bindFields(classDecl: ClassDecl) =
    classDecl.vars.foreach { case varDecl @ VarDecl(typeTree, varId, init, _) =>
      val tpe = setType(typeTree)
      varId.setType(tpe)
      init match {
        case Some(expr) => new StatementBinder(classDecl.getSymbol, varDecl.isStatic).bindExpr(expr)
        case None       =>
      }
    }

  private def checkConflictingFieldsInParent(vars: List[VarDecl], parent: Option[ClassSymbol]) =
    parent match {
      case Some(parentSymbol) =>
        vars.foreach { varDecl =>
          parentSymbol.lookupVar(varDecl.id.value) match {
            case Some(_) => ErrorFieldDefinedInSuperClass(varDecl.getSymbol.name, varDecl)
            case None    =>
          }
        }
      case None               =>
    }

  private def ensureIdentiferNotDefined[T <: Symbol](map: Map[String, T], id: String, pos: Positioned): Unit = {
    if (map.contains(id)) {
      val oldSymbol = map(id)
      ErrorVariableAlreadyDefined(id, oldSymbol.line, pos)
    }
  }

  private def ensureMethodNotDefined(meth: FuncTree): Unit = {
    val name = meth.id.value
    val argTypes = meth.args.map(_.tpe.getType)
    meth.getSymbol.classSymbol.lookupMethod(name, argTypes, recursive = false) match {
      case Some(oldMeth) => ErrorMethodAlreadyDefined(meth.signature, oldMeth.line, meth)
      case None          => meth.getSymbol.classSymbol.addMethod(meth.getSymbol)
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

  private def setType(tpe: TypeTree): Type = {
    tpe match {
      case BooleanType()                        => tpe.setType(TBool)
      case IntType()                            => tpe.setType(TInt)
      case LongType()                           => tpe.setType(TLong)
      case FloatType()                          => tpe.setType(TFloat)
      case DoubleType()                         => tpe.setType(TDouble)
      case CharType()                           => tpe.setType(TChar)
      case StringType()                         => tpe.setType(TString)
      case UnitType()                           => tpe.setType(TUnit)
      case tpeId @ ClassIdentifier(typeName, _) =>
        globalScope.lookupClass(typeName) match {
          case Some(classSymbol) =>
            tpeId.setSymbol(classSymbol)
            tpeId.setType(TObject(classSymbol))
          case None              =>
            ErrorTypeNotDeclared(tpeId.value, tpeId)
        }
      case ArrayType(arrayTpe)                  =>
        setType(arrayTpe)
        tpe.setType(TArray(arrayTpe.getType))
    }
    tpe.getType
  }


  private def setParentSymbol(id: ClassIdentifier, parent: Option[ClassIdentifier], classDecl: ClassDecl): Unit =
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


  private class StatementBinder(scope: Symbol, isStaticContext: Boolean) {

    class VariableIdentifier(val symbol: VariableSymbol, val scopeLevel: Int)

    def bindStatement(tree: Tree): Unit = bind(tree, Map(), 0)

    private def bind(statement: Tree, localVars: Map[String, VariableIdentifier], scopeLevel: Int): Map[String, VariableIdentifier] =
      statement match {
        case Block(stats)                                     =>
          stats.foldLeft(localVars)((currentLocalVars, nextStatement) => bind(nextStatement, currentLocalVars, scopeLevel + 1))
          localVars
        case varDecl @ VarDecl(typeTree, id, init, modifiers) =>
          val newSymbol = new VariableSymbol(id.value, modifiers).setPos(id)
          val tpe = setType(typeTree)
          id.setSymbol(newSymbol)
          varDecl.setSymbol(newSymbol)
          id.setType(tpe)

          variableUsage += newSymbol -> false

          init collect { case expr => bind(expr, localVars, scopeLevel) }

          localVars.get(id.value) match {
            case Some(varId) =>
              if (varId.scopeLevel == scopeLevel)
                ErrorVariableAlreadyDefined(id.value, varId.symbol.line, id)
            case _           =>
          }

          localVars + (id.value -> new VariableIdentifier(newSymbol, scopeLevel))
        case For(init, condition, post, stat)                 =>
          val newVars = init.foldLeft(localVars)((currentLocalVars, nextStatement) => bind(nextStatement, currentLocalVars, scopeLevel + 1))
          bind(condition, newVars, scopeLevel)
          post.foreach(bind(_, newVars, scopeLevel))
          bind(stat, newVars, scopeLevel)
          localVars
        case If(expr, thn, els)                               =>
          bind(expr, localVars, scopeLevel)
          bind(thn, localVars, scopeLevel)
          els collect { case e => bind(e, localVars, scopeLevel) }
          localVars
        case While(expr, stat)                                =>
          bind(expr, localVars, scopeLevel)
          bind(stat, localVars, scopeLevel)
          localVars
        case PrintStatement(expr)                             =>
          bind(expr, localVars, scopeLevel)
          localVars
        case Error(expr)                                      =>
          bind(expr, localVars, scopeLevel)
          localVars
        case Return(expr)                                     =>
          expr collect { case e => bind(e, localVars, scopeLevel) }
          localVars
        case expr: ExprTree                                        =>
          bindExpr(expr, localVars, scopeLevel)
          localVars
      }

    def bindExpr(tree: ExprTree): Unit = bindExpr(tree, Map(), 0)

    private def bindExpr(tree: ExprTree, localVars: Map[String, VariableIdentifier], scopeLevel: Int): Unit =
      Trees.traverse(tree, (parent, current) => Some(current) collect {
        case MethodCall(obj, _, args)  =>
          bind(obj, localVars, scopeLevel)
          args.foreach(bind(_, localVars, scopeLevel))
        case Instance(expr, id)        =>
          bind(expr, localVars, scopeLevel)
          globalScope.lookupClass(id.value) match {
            case Some(classSymbol) =>
              id.setSymbol(classSymbol)
              id.setType(TObject(classSymbol))
            case None              => ErrorTypeNotDeclared(id.value, id)
          }
        case FieldRead(obj, _)         =>
          bind(obj, localVars, scopeLevel)
        case FieldAssign(obj, _, expr) =>
          bind(obj, localVars, scopeLevel)
          bind(expr, localVars, scopeLevel)
        case id: Identifier            => parent match {
          case _: MethodCall  =>
          case _: Instance    =>
          case _: FieldRead   =>
          case _: FieldAssign =>
          case _              => setIdentiferSymbol(id, localVars)
        }
        case typeTree: TypeTree        => setType(typeTree)
        case NewArray(tpe, size)       => setType(tpe)
        case thisSymbol: This          =>
          scope match {
            case methodSymbol: MethodSymbol => thisSymbol.setSymbol(methodSymbol.classSymbol)
          }
      })

    private def setIdentiferSymbol(id: Identifier, localVars: Map[String, VariableIdentifier]): Unit = {
      val symbol = getSymbolForIdentifier(id, localVars)
      id.setSymbol(symbol)
      symbol match {
        case v: VariableSymbol => variableUsage += v -> true
        case _                 =>
      }
    }

    private def getSymbolForIdentifier(id: Identifier, localVars: Map[String, VariableIdentifier]): Symbol = {
      val name = id.value

      def lookupClass() = globalScope.lookupClass(name)
      def lookupLocalVar() = localVars.get(name).map(_.symbol)
      def lookupArgument(methodSymbol: MethodSymbol) = methodSymbol.lookupArgument(name)
      def lookupField(methodSymbol: MethodSymbol) = {
        val m = methodSymbol.lookupField(name)
        m collect {
          case sym if isStaticContext && !sym.isStatic =>
            ErrorAccessNonStaticFromStatic(id.value, id)
        }
        m
      }
      scope match {
        case methodSymbol: MethodSymbol => // We're binding symbols inside a method
          lookupClass().getOrElse(
            lookupLocalVar().getOrElse(
              lookupArgument(methodSymbol).getOrElse(
                lookupField(methodSymbol).getOrElse(
                  ErrorCantResolveSymbol(name, id))
              ))
          )
        case classSymbol: ClassSymbol   => // We're binding symbols inside a class (fields)
          classSymbol.lookupVar(name).getOrElse(ErrorCantResolveSymbol(name, id))
      }
    }
  }


  private def error(msg: String, tree: Positioned) = {
    tree match {
      case id: Identifier      => id.setSymbol(new ErrorSymbol)
      case id: ClassIdentifier => id.setSymbol(new ErrorSymbol)
      case _                   =>
    }

    ctx.reporter.error(msg, tree)
    new ErrorSymbol()
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
