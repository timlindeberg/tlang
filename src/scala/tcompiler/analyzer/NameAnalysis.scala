package tcompiler
package analyzer

import tcompiler.analyzer.Symbols._
import tcompiler.analyzer.Types._
import tcompiler.ast.TreeTraverser
import tcompiler.ast.Trees._
import tcompiler.imports.ClassSymbolLocator
import tcompiler.utils.Extensions._
import tcompiler.utils._

object NameAnalysis extends Pipeline[List[CompilationUnit], List[CompilationUnit]] {

  var globalScope: GlobalScope = null

  def run(ctx: Context)(cus: List[CompilationUnit]): List[CompilationUnit] = {
    globalScope = new GlobalScope

    // Add all symbols first so each program instance can access
    // all symbols in binding
    val analyzers = cus map { cu =>
      val nameAnalyzer = new NameAnalyser(ctx, cu)
      nameAnalyzer.addSymbols()
      nameAnalyzer.importExtensionClasses()
      nameAnalyzer
    }

    analyzers foreach { nameAnalyzer =>
      nameAnalyzer.bindIdentifiers()
      nameAnalyzer.checkInheritanceCycles()
      nameAnalyzer.checkVariableUsage()
      nameAnalyzer.checkVariableReassignments()
      nameAnalyzer.checkValidParenting()
    }

    cus
  }

}

class NameAnalyser(override var ctx: Context, cu: CompilationUnit) extends NameAnalysisErrors {

  import NameAnalysis._

  override var importMap            = cu.importMap
  private  var variableUsage        = Map[VariableSymbol, Boolean]()
  private  var variableReassignment = Map[VariableSymbol, Boolean]()


  def addSymbols(): Unit = cu.classes.foreach(addSymbols)
  def importExtensionClasses() = {
    cu.importMap.extensionImports foreach { extImport =>
      ClassSymbolLocator.findExtensionSymbol(extImport.fullName) match {
        case Some(e) =>
          globalScope.extensions += (e.name -> e)
        case None    => // NOO
      }
    }
  }

  def bindIdentifiers(): Unit = cu.classes.foreach(bind)

  def checkInheritanceCycles(): Unit = {

    var classesFoundInCycle = Set[ClassSymbol]()
    def checkInheritanceCycles(classSymbol: ClassSymbol, set: Set[ClassSymbol]): Unit = {
      if (classesFoundInCycle.contains(classSymbol))
        return

      if (set.contains(classSymbol)) {
        classesFoundInCycle ++= set
        ErrorInheritanceCycle(set, classSymbol, classSymbol)
        return
      }

      val newSet = set + classSymbol
      classSymbol.parents.foreach(checkInheritanceCycles(_, newSet))
    }

    globalScope.classes.foreach { case (_, classSymbol) => checkInheritanceCycles(classSymbol, Set[ClassSymbol]()) }
  }


  def checkVariableUsage() =
    variableUsage.foreach {
      case (variable, used) =>
        if (!used)
          WarningUnusedVar(variable)
    }

  def checkVariableReassignments() =
    variableReassignment.foreach {
      case (variable, reassigned) =>
        if (!reassigned)
          WarningCouldBeVal(variable.name, variable)
    }

  def checkValidParenting(): Unit =
    cu.classes.foreach { classDecl =>

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

  private def addSymbols(classDecl: ClassDeclTree): Unit = {
    val id = classDecl.id
    ensureClassNotDefined(id)
    val sym = classDecl match {
      case _: ExtensionDecl =>
        val name = id.name.replaceAll("::", ".")
        val fullName = (cu.pack.address :+ ExtensionDecl.seperator :+ name).mkString(".")
        val newSymbol = new ExtensionClassSymbol(fullName)
        globalScope.extensions += (fullName -> newSymbol)
        newSymbol
      case _                =>
        val fullName = (cu.pack.address :+ id.name).mkString(".")
        val newSymbol = new ClassSymbol(fullName, classDecl.isAbstract)
        globalScope.classes += (fullName -> newSymbol)
        newSymbol
    }

    sym.setPos(id)
    id.setSymbol(sym)
    classDecl.setSymbol(sym)
    classDecl.fields foreach { f => addSymbols(f, sym) }
    classDecl.methods foreach { m => addSymbols(m, sym) }
  }

  private def addSymbols(varDecl: VarDecl, classSymbol: ClassSymbol): Unit = {
    val id = varDecl.id
    val newSymbol = new FieldSymbol(id.name, varDecl.modifiers, classSymbol).setPos(varDecl)
    ensureIdentifierNotDefined(classSymbol.fields, id.name, varDecl)
    id.setSymbol(newSymbol)
    varDecl.setSymbol(newSymbol)

    val isStaticFinal = newSymbol.isStatic && newSymbol.isFinal
    if (classSymbol.isAbstract && !isStaticFinal)
      ErrorNonStaticFinalFieldInTrait(varDecl)

    // Check usage for private fields
    varDecl.accessability match {
      case Private() => variableUsage += newSymbol -> false
      case _         => variableUsage += newSymbol -> true
    }

    classSymbol.addField(newSymbol)
  }

  private def addSymbols(funcTree: MethodDeclTree, classSymbol: ClassSymbol): Unit = {
    val id = funcTree.id
    val name = id.name
    val sym = funcTree match {
      case methDecl@MethodDecl(retType, _, _, stat, modifiers)      =>
        val sym = new MethodSymbol(name, classSymbol, stat, modifiers)

        if (!classSymbol.isAbstract && stat.isEmpty)
          ErrorClassUnimplementedMethod(methDecl)

        if (classSymbol.isAbstract && retType.isEmpty && stat.isEmpty)
          ErrorUnimplementedMethodNoReturnType(sym.signature, methDecl)
        sym
      case conDecl@ConstructorDecl(_, _, _, stat, modifiers)        =>
        val sym = new MethodSymbol(name, classSymbol, stat, modifiers).setType(TUnit)

        // TODO: Make sure constructors aren't declared as abstract
        if (stat.isEmpty)
          ErrorAbstractOperator(conDecl)
        sym
      case opDecl@OperatorDecl(operatorType, _, _, stat, modifiers) =>
        val newSymbol = new OperatorSymbol(operatorType, classSymbol, stat, modifiers)

        if (stat.isEmpty)
          ErrorAbstractOperator(opDecl)
        newSymbol
    }

    sym.setPos(funcTree)
    id.setSymbol(sym)
    funcTree.setSymbol(sym)
    funcTree.args foreach { arg => addSymbols(arg, sym) }
  }

  private def addSymbols(formal: Formal, methSymbol: MethodSymbol): Unit = {
    val id = formal.id
    val modifiers: Set[Modifier] = Set(Private(), Final())
    val newSymbol = new VariableSymbol(id.name, modifiers).setPos(id)
    ensureIdentifierNotDefined(methSymbol.args, id.name, id)
    id.setSymbol(newSymbol)
    formal.setSymbol(newSymbol)

    methSymbol.args += (id.name -> newSymbol)
    methSymbol.argList ++= List(newSymbol)

    // Don't put out warning when args is unused since it's implicitly defined
    // or if the method is abstract
    if (methSymbol.isMainMethod || methSymbol.isAbstract)
      variableUsage += newSymbol -> true
    else
      variableUsage += newSymbol -> false
  }

  private def ensureClassNotDefined(id: ClassID): Unit = {
    val name = id.name
    val fullName = importMap.getFullName(name)
    fullName match {
      case "kool.lang.Object" =>
      case "kool.lang.String" =>
      case _                  =>
        globalScope.classes.get(fullName) ifDefined { old =>
          ErrorClassAlreadyDefined(name, old.line, id)
        }
    }
  }

  private def ensureIdentifierNotDefined[T <: Symbol](map: Map[String, T], id: String, pos: Positioned): Unit = {
    if (map.contains(id)) {
      val oldSymbol = map(id)
      ErrorVariableAlreadyDefined(id, oldSymbol.line, pos)
    }
  }


  /*-------------------------------- Binding symbols --------------------------------*/


  private def bind(tree: Tree): Unit = tree match {
    case classDecl@ClassDeclTree(id, parents, vars, methods)             =>
      classDecl.ifInstanceOf[ExtensionDecl] { extension =>
        val name = id.name
        val extensionSym = extension.getSymbol.asInstanceOf[ExtensionClassSymbol]
        globalScope.lookupClass(importMap, name) match {
          case Some(c) =>
            extensionSym.originalClassSymbol = Some(c)
            c.extensionClasses ::= extensionSym
          case None    => ErrorCantResolveSymbol(name, id)
        }
      }

      setParentSymbol(classDecl)
      bindFields(classDecl)
      methods foreach bind
    case methDecl@MethodDecl(retType, _, args, stat, _)                  =>
      retType ifDefined { tpe =>
        setType(tpe)
        methDecl.getSymbol.setType(tpe.getType)
      }

      bindArguments(args)
      ensureMethodNotDefined(methDecl)
      val sym = methDecl.getSymbol
      stat ifDefined {new StatementBinder(sym, methDecl.isStatic).bindStatement(_)}
    case constructorDecl@ConstructorDecl(_, _, args, stat, _)            =>

      bindArguments(args)
      ensureMethodNotDefined(constructorDecl)

      val sym = constructorDecl.getSymbol
      stat ifDefined {new StatementBinder(sym, false).bindStatement(_)}
    case operatorDecl@OperatorDecl(operatorType, retType, args, stat, _) =>
      retType ifDefined { tpe =>
        val t = setType(tpe)
        operatorDecl.getSymbol.setType(t)
        operatorType.setType(t)
      }

      bindArguments(args)

      ensureOperatorNotDefined(operatorDecl)

      val sym = operatorDecl.getSymbol
      val isStaticOperator = operatorDecl.isStatic
      val argTypes = args.map(_.getSymbol.getType)
      val argClassSymbols = argTypes.collect { case TObject(c) => c }
      val classSymbol = sym.classSymbol

      // Ensure that operator pertains to the class defined in and that
      // types are not nullable
      val nullableTypes = (retType ++ args.map(_.tpe)).filterType[NullableType]

      // We don't want to report OperatorWrongTypes if types are nullable
      if (nullableTypes.nonEmpty)
        nullableTypes.foreach(tpe => ErrorNullableInOperator(tpe))
      else if (isStaticOperator) {
        val sym = classSymbol match {
          case e: ExtensionClassSymbol => e.originalClassSymbol.get
          case _                       => classSymbol
        }

        if (!argClassSymbols.contains(sym))
          ErrorOperatorWrongTypes(operatorType, argTypes, classSymbol, sym.name, operatorDecl)
      }

      stat ifDefined {new StatementBinder(sym, isStaticOperator).bindStatement(_)}
  }


  private def bindArguments(args: List[Formal]) =
    args.foreach { case Formal(typeTree, id) =>
      val tpe = setType(typeTree)
      id.setType(tpe)
    }

  private def bindFields(classDecl: ClassDeclTree) =
    classDecl.fields.foreach { case varDecl@VarDecl(typeTree, varId, init, _) =>
      typeTree match {
        case Some(t) =>
          val tpe = setType(t)
          varId.setType(tpe)
        case None    =>
      }

      init ifDefined {new StatementBinder(classDecl.getSymbol, varDecl.isStatic).bindExpr(_)}
    }

  private class StatementBinder(scope: Symbol, isStaticContext: Boolean) {

    class VariableData(val symbol: VariableSymbol, val scopeLevel: Int)

    def bindStatement(tree: Tree): Unit = bind(tree, Map(), 0)

    private def bind(statement: Tree,
      localVars: Map[String, VariableData],
      scopeLevel: Int,
      canBreakContinue: Boolean = false): Map[String, VariableData] =
      statement match {
        case Block(stats)                                   =>
          stats.dropRight(1).foreach {
            case UselessStatement(expr) => WarningUselessStatement(expr)
            case _                      =>
          }
          stats.foldLeft(localVars)((currentLocalVars, nextStatement) => bind(nextStatement, currentLocalVars, scopeLevel + 1, canBreakContinue))
          localVars
        case varDecl@VarDecl(typeTree, id, init, modifiers) =>
          val newSymbol = new VariableSymbol(id.name, modifiers).setPos(varDecl)
          id.setSymbol(newSymbol)
          varDecl.setSymbol(newSymbol)

          typeTree ifDefined { t =>
            val tpe = setType(t)
            id.setType(tpe)
          }

          variableUsage += newSymbol -> false
          if (!varDecl.isFinal)
            variableReassignment += newSymbol -> false

          init ifDefined { expr => bind(expr, localVars, scopeLevel, canBreakContinue) }

          localVars.get(id.name) ifDefined { varId =>
            if (varId.scopeLevel == scopeLevel)
              ErrorVariableAlreadyDefined(id.name, varId.symbol.line, varDecl)
          }

          localVars + (id.name -> new VariableData(newSymbol, scopeLevel))
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
        case If(condition, thn, els)                        =>
          bind(condition, localVars, scopeLevel)
          bind(thn, localVars, scopeLevel, canBreakContinue)
          els ifDefined { els => bind(els, localVars, scopeLevel, canBreakContinue) }
          localVars
        case While(condition, stat)                         =>
          bind(condition, localVars, scopeLevel)
          bind(stat, localVars, scopeLevel, canBreakContinue = true)
          localVars
        case PrintStatTree(expr)                            =>
          bind(expr, localVars, scopeLevel)
          localVars
        case Error(expr)                                    =>
          bind(expr, localVars, scopeLevel)
          localVars
        case Return(expr)                                   =>
          expr ifDefined { expr => bind(expr, localVars, scopeLevel) }
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


    private def bindExpr(startingTree: ExprTree, localVars: Map[String, VariableData], scopeLevel: Int): Unit = {
      val traverser = new TreeTraverser {

        override def traverse(t: Tree) = t match {
          case acc@Access(obj, application) =>
            obj match {
              case _: Empty            =>
                // Parser fills access with Empty() when using implicit
                // this or implicit static call. Replaces the empty with
                // class name or this.
                val classSymbol = scope match {
                  case m: MethodSymbol => m.classSymbol
                  case c: ClassSymbol  => c
                  case _               => ???
                }
                val obj = if (isStaticContext) ClassID(classSymbol.name) else This()
                obj.setSymbol(classSymbol).setPos(acc)
                acc.obj = obj
              case id@VariableID(name) =>
                lookupVariableSymbol(id, localVars) match {
                  case Some(varSymbol) => setVarIdentifierSymbol(id, varSymbol)
                  case None            =>
                    // access object is not a variable, must be a class
                    globalScope.lookupClass(importMap, name) match {
                      case Some(classSymbol) =>
                        val classId = ClassID(name).setPos(id).setSymbol(classSymbol)
                        acc.obj = classId
                      case None              => ErrorCantResolveSymbol(name, id)
                    }
                }
              case _                   =>
                traverse(obj)
            }
            application match {
              case _: VariableID =>
              // This is a field. Since we don't know what class it belongs to we do nothing
              case _ => traverse(application)
            }
          case Assign(to, expr)             =>
            traverse(to, expr)
            to ifInstanceOf[VariableID] { id =>
              setVariableUsed(id)
              setVariableReassigned(id)
            }
          case IncrementDecrementTree(expr) =>
            traverse(expr)
            expr ifInstanceOf[VariableID] { id =>
              setVariableUsed(id)
              setVariableReassigned(id)
            }
          case thisTree: This               =>
            scope match {
              case methodSymbol: MethodSymbol =>
                if (isStaticContext)
                  ErrorThisInStaticContext(thisTree)
                methodSymbol.classSymbol match {
                  case e: ExtensionClassSymbol => thisTree.setSymbol(e.originalClassSymbol.get)
                  case classSymbol             => thisTree.setSymbol(classSymbol)
                }
              case _: ClassSymbol             => ErrorThisInStaticContext(thisTree)
              case _                          => ???
            }
          case superSymbol@Super(specifier) =>
            scope match {
              case methodSymbol: MethodSymbol =>
                if (isStaticContext)
                  ErrorSuperInStaticContext(superSymbol)
                val parents = methodSymbol.classSymbol.parents

                specifier match {
                  case Some(spec) =>
                    parents.find(_.name == spec.name) match {
                      case Some(p) =>
                        superSymbol.setSymbol(p)
                      case None    =>
                        ErrorSuperSpecifierDoesNotExist(spec.name, methodSymbol.classSymbol.name, spec)
                    }
                  case _          =>
                    superSymbol.setSymbol(methodSymbol.classSymbol)
                  // Set symbol to this and let the typechecker decide later.
                }
              case _: ClassSymbol             => ErrorSuperInStaticContext(superSymbol)
              case _                          => ???
            }
          case tpe: TypeTree                => setType(tpe)
          case id: VariableID               => setVarIdentifierSymbol(id, localVars)
          case _                            => super.traverse(t)
        }
      }
      traverser.traverse(startingTree)
    }

    private def setVariableUsed(id: VariableID) = variableUsage += id.getSymbol -> true
    private def setVariableReassigned(id: VariableID) = variableReassignment += id.getSymbol -> true

    private def setVarIdentifierSymbol(id: VariableID, localVars: Map[String, VariableData]): Unit = {
      lookupVariableSymbol(id, localVars) match {
        case Some(symbol) => setVarIdentifierSymbol(id, symbol)
        case None         => ErrorCantResolveSymbol(id.name, id)
      }
    }

    private def setVarIdentifierSymbol(id: VariableID, symbol: VariableSymbol) = {
      id.setSymbol(symbol)
      variableUsage += symbol -> true
    }

    private def lookupVariableSymbol(id: VariableID, localVars: Map[String, VariableData]): Option[VariableSymbol] = {
      val name = id.name

      def lookupLocalVar() = localVars.get(name).map(_.symbol)
      def lookupArgument(methodSymbol: MethodSymbol) = methodSymbol.lookupArgument(name)
      def lookupField(methodSymbol: MethodSymbol) = {
        val m = methodSymbol.lookupField(name)
        m collect {
          case sym if isStaticContext && !sym.isStatic =>
            ErrorAccessNonStaticFromStatic(id.name, id)
        }
        m
      }
      scope match {
        case methodSymbol: MethodSymbol => // Binding symbols inside a method
          lookupLocalVar().orElse(lookupArgument(methodSymbol).orElse(lookupField(methodSymbol)))
        case classSymbol: ClassSymbol   => // Binding symbols inside a class (fields)
          classSymbol.lookupField(name)
        case _                          => ???
      }
    }
  }

  private def ensureMethodNotDefined(meth: MethodDeclTree): Unit = {
    // This is done in the binding stage since we are then gauranteed that
    // all types have been added to the global scope
    val name = meth.id.name
    val argTypes = meth.args.map(_.tpe.getType)
    val classSymbol = meth.getSymbol.classSymbol
    classSymbol.findMethod(name, argTypes, exactTypes = true) match {
      case Some(oldMeth) => ErrorMethodAlreadyDefined(meth.getSymbol.signature, oldMeth.line, meth)
      case None          => classSymbol.addMethod(meth.getSymbol)
    }
  }

  private def ensureOperatorNotDefined(operator: OperatorDecl): Unit = {
    // This is done in the binding stage since we are then guaranteed that
    // all types have been added to the global scope
    val operatorType = operator.operatorType
    val argTypes = operator.args.map(_.tpe.getType)

    val op = operator.getSymbol.asInstanceOf[OperatorSymbol]
    val classSymbol = operator.getSymbol.classSymbol
    classSymbol.lookupOperator(operatorType, argTypes, exactTypes = true) match {
      case Some(oldOperator) =>
        ErrorOperatorAlreadyDefined(op.signature, oldOperator.line, operator)
      case None              =>
        operator.getSymbol.classSymbol.addOperator(op)
    }
  }

  private def setType(tpe: TypeTree): Type = {
    tpe match {
      case BooleanType()             => tpe.setType(Bool)
      case IntType()                 => tpe.setType(Int)
      case LongType()                => tpe.setType(Long)
      case FloatType()               => tpe.setType(Float)
      case DoubleType()              => tpe.setType(Double)
      case CharType()                => tpe.setType(Char)
      case UnitType()                => tpe.setType(TUnit)
      case tpeId@ClassID(name, _)    =>
        globalScope.lookupClass(importMap, name) match {
          case Some(classSymbol) => tpeId.setSymbol(classSymbol)
          case None              => ErrorUnknownType(name, tpeId)
        }
      case ArrayType(arrayTpe)       =>
        setType(arrayTpe)
        tpe.setType(TArray(arrayTpe.getType))
      case NullableType(nullableTpe) =>
        val t = setType(nullableTpe).getNullable
        nullableTpe.setType(t)
        tpe.setType(t)
    }
    tpe.getType
  }

  private def setParentSymbol(classDecl: ClassDeclTree): Unit = {
    val id = classDecl.id
    val parents = classDecl.parents
    parents.foreach { parentId =>
      globalScope.lookupClass(importMap, parentId.name) match {
        case Some(parentSymbol) => parentId.setSymbol(parentSymbol)
        case None               => ErrorParentNotDeclared(parentId.name, parentId)
      }
    }

    val nonAbstractParents = parents.filter(!_.getSymbol.isAbstract)
    if (!classDecl.isAbstract && nonAbstractParents.isEmpty) {
      val defaultParent = ClassID(Main.TLangObject).setSymbol(Types.ObjectSymbol)
      val abstractParents = parents.filter(_.getSymbol.isAbstract)
      classDecl.parents = defaultParent :: abstractParents
    }
    classDecl.getSymbol.parents = classDecl.parents.map(_.getSymbol)
  }
}
