package tcompiler
package analyzer

import tcompiler.analyzer.Symbols._
import tcompiler.analyzer.Types._
import tcompiler.ast.TreeTraverser
import tcompiler.ast.Trees._
import tcompiler.utils.Extensions._
import tcompiler.utils._

object NameAnalysis extends Pipeline[List[Program], List[Program]] {

  var globalScope: GlobalScope = null

  def run(ctx: Context)(progs: List[Program]): List[Program] = {
    globalScope = new GlobalScope



    // Add all symbols first so each program instance can access
    // all symbols in binding
    val analyzers = progs map { prog =>
      val nameAnalyzer = new NameAnalyser(ctx, prog)
      nameAnalyzer.addSymbols()
      nameAnalyzer
    }

    analyzers foreach { nameAnalyzer =>
      nameAnalyzer.bindIdentifiers()
      nameAnalyzer.checkInheritanceCycles()
      nameAnalyzer.checkVariableUsage()
      nameAnalyzer.checkVariableReassignments()
      nameAnalyzer.checkValidParenting()
    }
    progs
  }

}

class NameAnalyser(override var ctx: Context, prog: Program) extends NameAnalysisErrors {

  import NameAnalysis._

  private var variableUsage        = Map[VariableSymbol, Boolean]()
  private var variableReassignment = Map[VariableSymbol, Boolean]()


  def addSymbols(): Unit = prog.classes.foreach(addSymbols)

  def bindIdentifiers(): Unit = prog.classes.foreach(bind)

  def checkInheritanceCycles(): Unit = {

    var classesFoundInCycle = Set[ClassSymbol]()
    def checkInheritanceCycles(classSymbol: ClassSymbol, set: Set[ClassSymbol]): Unit = {
      if (classesFoundInCycle.contains(classSymbol))
        return

      if (set.contains(classSymbol)) {
        classesFoundInCycle ++= set
        ErrorInheritanceCycle(set, classSymbol, classSymbol)
      } else {
        val newSet = set + classSymbol
        classSymbol.parents.foreach(checkInheritanceCycles(_, newSet))
      }
    }

    globalScope.classes.foreach { case (_, classSymbol) => checkInheritanceCycles(classSymbol, Set[ClassSymbol]()) }
  }


  def checkVariableUsage() = {
    variableUsage.foreach {
      case (variable, used) =>
        if (!used)
          WarningUnusedVar(variable)
    }
  }

  def checkVariableReassignments() = {
    variableReassignment.foreach {
      case (variable, reassigned) =>
        if (!reassigned)
          WarningCouldBeVal(variable.name, variable)
    }
  }

  def checkValidParenting(): Unit =
    prog.classes.foreach { classDecl =>

      val nonTraits = classDecl.parents.filter(!_.getSymbol.isAbstract)
      if (nonTraits.size > 1) {
        ErrorExtendMultipleClasses(nonTraits(1))
        return
      }

      val nonTraitsAfterFirst = classDecl.parents.drop(1).filter(!_.getSymbol.isAbstract)
      if (nonTraitsAfterFirst.nonEmpty)
        ErrorNonFirstArgumentIsClass(nonTraits.head)
    }


  /*-------------------------------- Adding symbols --------------------------------*/

  private def addSymbols(classDecl: ClassDecl): Unit = classDecl match {
    case ClassDecl(id@ClassIdentifier(name, _), _, vars, methods, isAbstract) =>
      val fullName = prog.getPackageName(name)

      val newSymbol = new ClassSymbol(fullName, isAbstract)
      prog.importMap(name) = fullName
      newSymbol.writtenName = name
      newSymbol.setPos(id)
      ensureClassNotDefined(id)
      id.setSymbol(newSymbol)
      classDecl.setSymbol(newSymbol)
      globalScope.classes += (fullName -> newSymbol)
      vars.foreach(addSymbols(_, newSymbol))
      methods.foreach(addSymbols(_, newSymbol))
  }

  private def addSymbols(t: Tree, classSymbol: ClassSymbol): Unit = t match {
    case varDecl@VarDecl(tpe, id, init, modifiers)                                   =>
      val newSymbol = new VariableSymbol(id.value, Field, modifiers, Some(classSymbol)).setPos(varDecl)
      ensureIdentiferNotDefined(classSymbol.fields, id.value, varDecl)
      id.setSymbol(newSymbol)
      varDecl.setSymbol(newSymbol)

      val isStaticFinal = modifiers.contains(Static()) && modifiers.contains(Final())
      if (classSymbol.isAbstract && !isStaticFinal)
        ErrorNonStaticFinalFieldInTrait(varDecl)

      // Check usage for private fields
      varDecl.accessability match {
        case Private() => variableUsage += newSymbol -> false
        case _         => variableUsage += newSymbol -> true
      }

      classSymbol.addField(newSymbol)
    case methodDecl@MethodDecl(retType, id, args, stat, modifiers)                   =>
      val newSymbol = new MethodSymbol(id.value, classSymbol, stat, modifiers).setPos(methodDecl)
      id.setSymbol(newSymbol)
      methodDecl.setSymbol(newSymbol)
      args.foreach(addSymbols(_, newSymbol))

      if (!classSymbol.isAbstract && stat.isEmpty)
        ErrorClassUnimplementedMethod(methodDecl)

      if (classSymbol.isAbstract && retType.isEmpty && stat.isEmpty)
        ErrorUnimplementedMethodNoReturnType(newSymbol.signature, methodDecl)
    case constructorDecl@ConstructorDecl(_, id, args, stat, modifiers)               =>
      // TODO: Make sure constructors arent declared as abstract
      val newSymbol = new MethodSymbol(id.value, classSymbol, stat, modifiers).setPos(constructorDecl)
      newSymbol.setType(TUnit)

      id.setSymbol(newSymbol)
      constructorDecl.setSymbol(newSymbol)

      args.foreach(addSymbols(_, newSymbol))
    case operatorDecl@OperatorDecl(operatorType, retType, args, stat, modifiers, id) =>
      val newSymbol = new OperatorSymbol(operatorType, classSymbol, stat, modifiers).setPos(operatorDecl)
      id.setSymbol(newSymbol)
      operatorDecl.setSymbol(newSymbol)

      args.foreach(addSymbols(_, newSymbol))

      if (stat.isEmpty)
        ErrorAbstractOperator(operatorDecl)
  }

  private def addSymbols(t: Tree, methSymbol: MethodSymbol): Unit = t match {
    case formal@Formal(_, id) =>
      val modifiers: Set[Modifier] = Set(Private(), Final())
      val newSymbol = new VariableSymbol(id.value, Argument, modifiers).setPos(id)
      ensureIdentiferNotDefined(methSymbol.params, id.value, id)
      id.setSymbol(newSymbol)
      formal.setSymbol(newSymbol)

      methSymbol.params += (id.value -> newSymbol)
      methSymbol.argList ++= List(newSymbol)

      // Don't put out warning when args is unused since it's implicitly defined
      // or if the method is abstract
      if (methSymbol.isMainMethod || methSymbol.isAbstract)
        variableUsage += newSymbol -> true
      else
        variableUsage += newSymbol -> false
  }

  private def ensureClassNotDefined(id: ClassIdentifier): Unit = {
    val name = id.value
    val fullName = prog.getFullName(name)
    globalScope.classes.get(fullName) match {
      case Some(old) => ErrorClassAlreadyDefined(name, old.line, id)
      case None =>
    }
  }

  private def ensureIdentiferNotDefined[T <: Symbol](map: Map[String, T], id: String, pos: Positioned): Unit = {
    if (map.contains(id)) {
      val oldSymbol = map(id)
      ErrorVariableAlreadyDefined(id, oldSymbol.line, pos)
    }
  }


  /*-------------------------------- Binding symbols --------------------------------*/


  private def bind(tree: Tree): Unit = tree match {
    case classDecl@ClassDecl(id, parents, vars, methods, _)                 =>
      setParentSymbol(id, parents, classDecl)
      val sym = classDecl.getSymbol
      sym.setType(TObject(sym))

      bindFields(classDecl)
      methods.foreach(bind)
    case methDecl@MethodDecl(retType, _, args, stat, _)                     =>
      retType.ifDefined { tpe =>
        setType(tpe)
        methDecl.getSymbol.setType(tpe.getType)
      }

      bindArguments(args)
      ensureMethodNotDefined(methDecl)

      stat.ifDefined(new StatementBinder(methDecl.getSymbol, methDecl.isStatic).bindStatement(_))
    case constructorDecl@ConstructorDecl(_, _, args, stat, _)               =>

      bindArguments(args)
      ensureMethodNotDefined(constructorDecl)

      stat.ifDefined(new StatementBinder(constructorDecl.getSymbol, false).bindStatement(_))
    case operatorDecl@OperatorDecl(operatorType, retType, args, stat, _, _) =>
      retType.ifDefined { tpe =>
        val t = setType(tpe)
        operatorDecl.getSymbol.setType(t)
        operatorType.setType(t)
      }

      //operatorType.setType(retType.getType)
      bindArguments(args)

      ensureOperatorNotDefined(operatorDecl)

      val isStaticOperator = operatorDecl.modifiers.contains(Static())
      val argTypes = args.map(_.getSymbol.getType)
      val argClassSymbols = argTypes.collect { case TObject(c) => c }
      val classSymbol = operatorDecl.getSymbol.classSymbol

      // Ensure that operator pertains to the class defined in
      if (isStaticOperator && !argClassSymbols.contains(classSymbol))
        ErrorOperatorWrongTypes(operatorType, argTypes, classSymbol.name, operatorDecl)

      stat.ifDefined(new StatementBinder(operatorDecl.getSymbol, operatorDecl.isStatic).bindStatement(_))
  }


  private def bindArguments(args: List[Formal]) =
    args.foreach { case Formal(typeTree, id) =>
      val tpe = setType(typeTree)
      id.setType(tpe)
    }

  private def bindFields(classDecl: ClassDecl) =
    classDecl.fields.foreach { case varDecl@VarDecl(typeTree, varId, init, _) =>
      typeTree match {
        case Some(t) =>
          val tpe = setType(t)
          varId.setType(tpe)
        case None    =>
      }

      init.ifDefined(new StatementBinder(classDecl.getSymbol, varDecl.isStatic).bindExpr(_))
    }

  private class StatementBinder(scope: Symbol, isStaticContext: Boolean) {

    class VariableIdentifier(val symbol: VariableSymbol, val scopeLevel: Int)

    def bindStatement(tree: Tree): Unit = bind(tree, Map(), 0)

    private def bind(statement: Tree,
      localVars: Map[String, VariableIdentifier],
      scopeLevel: Int,
      canBreakContinue: Boolean = false): Map[String, VariableIdentifier] =
      statement match {
        case Block(stats)                                   =>
          stats.dropRight(1).foreach {
            case UselessStatement(expr) => WarningUselessStatement(expr)
            case _                      =>
          }
          stats.foldLeft(localVars)((currentLocalVars, nextStatement) => bind(nextStatement, currentLocalVars, scopeLevel + 1, canBreakContinue))
          localVars
        case varDecl@VarDecl(typeTree, id, init, modifiers) =>
          val newSymbol = new VariableSymbol(id.value, LocalVar, modifiers).setPos(varDecl)
          id.setSymbol(newSymbol)
          varDecl.setSymbol(newSymbol)

          typeTree ifDefined { t =>
            val tpe = setType(t)
            id.setType(tpe)
          }

          variableUsage += newSymbol -> false
          val isFinal = modifiers.contains(Final())
          if (!isFinal)
            variableReassignment += newSymbol -> false

          init ifDefined {expr => bind(expr, localVars, scopeLevel, canBreakContinue)}

          localVars.get(id.value) ifDefined { varId =>
            if(varId.scopeLevel == scopeLevel)
              ErrorVariableAlreadyDefined(id.value, varId.symbol.line, varDecl)
          }

          localVars + (id.value -> new VariableIdentifier(newSymbol, scopeLevel))
        case For(init, condition, post, stat)               =>
          val newVars = init.foldLeft(localVars)((currentLocalVars, nextStatement) => bind(nextStatement, currentLocalVars, scopeLevel + 1))
          bind(condition, newVars, scopeLevel)
          post.foreach(bind(_, newVars, scopeLevel, canBreakContinue))
          bind(stat, newVars, scopeLevel + 1, canBreakContinue = true)
          localVars
        case Foreach(varDecl, container, stat)              =>
          val newVars = bind(varDecl, localVars, scopeLevel)
          bind(container, localVars, scopeLevel)
          bind(stat, newVars, scopeLevel + 1, canBreakContinue = true)
          localVars
        case If(expr, thn, els)                             =>
          bind(expr, localVars, scopeLevel)
          bind(thn, localVars, scopeLevel, canBreakContinue)
          els collect { case e => bind(e, localVars, scopeLevel, canBreakContinue) }
          localVars
        case While(expr, stat)                              =>
          bind(expr, localVars, scopeLevel)
          bind(stat, localVars, scopeLevel, canBreakContinue = true)
          localVars
        case PrintStatTree(expr)                            =>
          bind(expr, localVars, scopeLevel)
          localVars
        case Error(expr)                                    =>
          bind(expr, localVars, scopeLevel)
          localVars
        case Return(expr)                                   =>
          expr collect { case e => bind(e, localVars, scopeLevel) }
          localVars
        case _: Break | _: Continue                         =>
          if (!canBreakContinue)
            ErrorBreakContinueOutsideLoop(statement, statement)
          localVars
        case expr: ExprTree                                 =>
          bindExpr(expr, localVars, scopeLevel)
          localVars
      }

    def bindExpr(tree: ExprTree): Unit = bindExpr(tree, Map(), 0)


    private def bindExpr(startingTree: ExprTree, localVars: Map[String, VariableIdentifier], scopeLevel: Int): Unit = {
      val traverser = new TreeTraverser {

        override def traverse(t: Tree) = t match {
          case mc@MethodCall(obj, methodName, args) =>
            // Don't traverse methodName identifier now since we don't
            // know the type of 'obj' before typechecking.
            // Therefore we can't know what class methodName belongs to
            obj match {
              case _: Empty =>
                // Replace empty with class name or this
                val classSymbol = scope match {
                  case m: MethodSymbol => m.classSymbol
                  case c: ClassSymbol  => c
                  case _               => ???
                }
                val obj = if (isStaticContext) Identifier(classSymbol.name) else This()
                mc.obj = obj.setSymbol(classSymbol)
              case _        => traverse(obj)
            }
            args foreach traverse
          case FieldAssign(obj, id, expr)           =>
            // Same as with method call, we can't traverse 'id' until typechecking stage
            traverse(obj, expr)
          case FieldAccess(obj, id)             =>
            // Same as with method call, we can't traverse 'id' until typechecking stage
            traverse(obj)
          case Instance(expr, id)                   =>
            traverse(expr)
            globalScope.lookupClass(prog, id.value) match {
              case Some(classSymbol) =>
                id.setSymbol(classSymbol)
                id.setType(TObject(classSymbol))
              case None              => ErrorUnknownType(id.value, id)
            }
          case Assign(id, expr)                     =>
            traverse(id, expr)
            checkReassignment(id, pos = t)
            setVariableUsed(id)
            id.getSymbol match {
              case v: VariableSymbol => variableReassignment += v -> true
              case _                 => ??? // Assignment can only be to a variable
            }
          case IncrementDecrementTree(expr)         =>
            traverse(expr)
            expr match {
              case id: Identifier =>
                checkReassignment(id, pos = t)
                setVariableUsed(id)
              case _              =>
            }
          case thisTree: This                       =>
            scope match {
              case methodSymbol: MethodSymbol =>
                if (isStaticContext)
                  ErrorThisInStaticContext(thisTree)
                thisTree.setSymbol(methodSymbol.classSymbol)
              case _: ClassSymbol             => ErrorThisInStaticContext(thisTree)
              case _                          => ???
            }
          case superSymbol@Super(specifier)         =>
            scope match {
              case methodSymbol: MethodSymbol =>
                if (isStaticContext)
                  ErrorSuperInStaticContext(superSymbol)
                val parents = methodSymbol.classSymbol.parents

                specifier match {
                  case Some(spec)              =>
                    parents.find(_.name == spec.value) match {
                      case Some(p) =>
                        superSymbol.setSymbol(p)
                      case None    =>
                        ErrorSuperSpecifierDoesNotExist(spec.value, methodSymbol.classSymbol.name, spec)
                    }
                  case _                       =>
                    superSymbol.setSymbol(methodSymbol.classSymbol)
                  // Set symbol to this and let the typechecker decide later.
                }
              case _: ClassSymbol             => ErrorSuperInStaticContext(superSymbol)
              case _                          => ???
            }
          case tpe: TypeTree                        => setType(tpe)
          case id: Identifier                       => setIdentiferSymbol(id, localVars)
          case _                                    => super.traverse(t)
        }
      }
      traverser.traverse(startingTree)
    }

    private def setVariableUsed(id: Identifier) =
      id.getSymbol match {
        case v: VariableSymbol => variableReassignment += v -> true
        case _                 => ??? // Should only be called for identifier with variable symbols
      }

    private def checkReassignment(id: Identifier, pos: Positioned) = {
      val varSymbol = id.getSymbol.asInstanceOf[VariableSymbol]
      if (varSymbol.modifiers.contains(Final()))
        ErrorReassignmentToVal(id.value, pos)
    }

    private def setIdentiferSymbol(id: Identifier, symbol: Symbol) = {
      id.setSymbol(symbol)
      symbol match {
        case v: VariableSymbol => variableUsage += v -> true
        case _                 =>
      }
    }

    private def setIdentiferSymbol(id: Identifier, localVars: Map[String, VariableIdentifier]): Unit = {
      lookupSymbol(id, localVars) match {
        case Some(symbol) => setIdentiferSymbol(id, symbol)
        case None         => ErrorCantResolveSymbol(id.value, id)
      }
    }

    private def lookupSymbol(id: Identifier, localVars: Map[String, VariableIdentifier]): Option[Symbol] = {
      val name = id.value

      def lookupLocalVar() = localVars.get(name).map(_.symbol)
      def lookupArgument(methodSymbol: MethodSymbol) = methodSymbol.lookupArgument(name)
      def lookupClass() = globalScope.lookupClass(prog, name)
      def lookupField(methodSymbol: MethodSymbol) = {
        val m = methodSymbol.lookupField(name)
        m collect {
          case sym if isStaticContext && !sym.isStatic =>
            ErrorAccessNonStaticFromStatic(id.value, id)
        }
        m
      }
      scope match {
        case methodSymbol: MethodSymbol => // Binding symbols inside a method
            lookupLocalVar().orElse(
              lookupArgument(methodSymbol).orElse(
                lookupField(methodSymbol).orElse(
                  lookupClass()
                  )))
        case classSymbol: ClassSymbol   => // Binding symbols inside a class (fields)
          classSymbol.lookupVar(name)
        case _                          => ???
      }
    }
  }

  private def ensureMethodNotDefined(meth: FuncTree): Unit = {
    // This is done in the binding stage since we are then gauranteed that
    // all types have been added to the global scope
    val name = meth.id.value
    val argTypes = meth.args.map(_.tpe.getType)
    val classSymbol = meth.getSymbol.classSymbol
    classSymbol.lookupMethod(name, argTypes, recursive = false) match {
      case Some(oldMeth) => ErrorMethodAlreadyDefined(meth.getSymbol.signature, oldMeth.line, meth)
      case None          => classSymbol.addMethod(meth.getSymbol)
    }
  }

  private def ensureOperatorNotDefined(operator: OperatorDecl): Unit = {
    // This is done in the binding stage since we are then gauranteed that
    // all types have been added to the global scope
    val operatorType = operator.operatorType
    val argTypes = operator.args.map(_.tpe.getType)


    val classSymbol = operator.getSymbol.classSymbol
    classSymbol.lookupOperator(operatorType, argTypes, recursive = false) match {
      case Some(oldOperator) =>
        val op = operator.getSymbol.asInstanceOf[OperatorSymbol]

        ErrorOperatorAlreadyDefined(op.operatorString, oldOperator.line, operator)
      case None              =>
        classSymbol.lookupOperator(operatorType, argTypes, recursive = true) match {
          case Some(oldOperator) =>
            ErrorOverrideOperator(operator)
          case None              =>
            val op = operator.getSymbol.asInstanceOf[OperatorSymbol]
            operator.getSymbol.classSymbol.addOperator(op)
        }
    }
  }

  private def setType(tpe: TypeTree): Type = {
    tpe match {
      case BooleanType()                  => tpe.setType(TBool)
      case IntType()                      => tpe.setType(TInt)
      case LongType()                     => tpe.setType(TLong)
      case FloatType()                    => tpe.setType(TFloat)
      case DoubleType()                   => tpe.setType(TDouble)
      case CharType()                     => tpe.setType(TChar)
      case StringType()                   => tpe.setType(TString)
      case UnitType()                     => tpe.setType(TUnit)
      case tpeId@ClassIdentifier(name, _) =>
        globalScope.lookupClass(prog, name) match {
          case Some(classSymbol) =>
            tpeId.setSymbol(classSymbol)
            tpeId.setType(TObject(classSymbol))
          case None              =>
            ErrorUnknownType(name, tpeId)
        }
      case ArrayType(arrayTpe)            =>
        setType(arrayTpe)
        tpe.setType(TArray(arrayTpe.getType))
    }
    tpe.getType
  }

  private def setParentSymbol(id: ClassIdentifier, parents: List[ClassIdentifier], classDecl: ClassDecl): Unit = {
    parents.foreach { parentId =>
      globalScope.lookupClass(prog, parentId.value) match {
        case Some(parentSymbol) =>
          parentId.setSymbol(parentSymbol)
          // This takes O(n), shouldnt be a big problem though
          classDecl.getSymbol.parents = classDecl.getSymbol.parents :+ parentSymbol
        case None               =>
          ErrorParentNotDeclared(parentId.value, parentId)
      }
    }
  }
}
