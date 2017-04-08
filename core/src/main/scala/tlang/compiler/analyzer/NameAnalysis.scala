package tlang.compiler
package analyzer

import tlang.compiler.analyzer.Symbols._
import tlang.compiler.analyzer.Types._
import tlang.compiler.ast.Trees
import tlang.compiler.ast.Trees._
import tlang.utils.Extensions._
import tlang.utils.Positioned

object NameAnalysis extends Pipeline[CompilationUnit, CompilationUnit] {

  var globalScope: GlobalScope = _

  def run(ctx: Context)(cus: List[CompilationUnit]): List[CompilationUnit] = {
    globalScope = new GlobalScope

    // Add all symbols first so each program instance can access
    // all symbols in binding
    val analyzers = cus map { cu =>
      val nameAnalyzer = new NameAnalyser(ctx, cu)
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

    cus
  }

}

class NameAnalyser(override val ctx: Context, cu: CompilationUnit) extends NameAnalysisErrors {

  import NameAnalysis._

  override def replaceNames(str: String): String = cu.imports.replaceNames(str)

  private var variableUsage        = Map[VariableSymbol, Boolean]()
  private var variableReassignment = Map[VariableSymbol, Boolean]()


  def addSymbols(): Unit = cu.classes foreach addSymbols

  def bindIdentifiers(): Unit = cu.classes.foreach(bind)

  def checkInheritanceCycles(): Unit = {

    var classesFoundInCycle = Set[ClassSymbol]()
    def checkInheritanceCycles(classSymbol: ClassSymbol, set: Set[ClassSymbol]): Unit = {
      if (classesFoundInCycle.contains(classSymbol))
        return

      if (set.contains(classSymbol)) {
        classesFoundInCycle ++= set
        report(InheritanceCycle(set, classSymbol, classSymbol))
        return
      }

      val newSet = set + classSymbol
      classSymbol.parents.foreach(checkInheritanceCycles(_, newSet))
    }

    globalScope.classes.foreach { case (_, classSymbol) => checkInheritanceCycles(classSymbol, Set[ClassSymbol]()) }
  }


  def checkVariableUsage(): Unit =
    variableUsage
      .filter { case (_, used) => !used }
      .foreach { case (variable, _) => report(UnusedVar(variable)) }

  def checkVariableReassignments(): Unit =
    variableReassignment
      .filter { case (_, reassigned) => !reassigned }
      .foreach { case (variable, _) => report(CouldBeVal(variable.name, variable)) }

  def checkValidParenting(): Unit =
    cu.classes.foreach { classDecl =>

      val nonTraits = classDecl.parents.filter(!_.getSymbol.isAbstract)
      if (nonTraits.size > 1) {
        report(ExtendMultipleClasses(nonTraits(1)))
        return
      }

      val nonTraitsAfterFirst = classDecl.parents.drop(1).filter(!_.getSymbol.isAbstract)
      if (nonTraitsAfterFirst.nonEmpty)
        report(NonFirstArgumentIsClass(nonTraits.head))
    }


  /*-------------------------------- Adding symbols --------------------------------*/

  private def addSymbols(classDecl: ClassDeclTree): Unit = {
    val sym = classDecl match {
      case ext: ExtensionDecl     =>
        val tpe = ext.tpe
        val fullName = (cu.pack.address :+ ExtensionDecl.seperator :+ tpe.name).mkString("::")
        val newSymbol = new ExtensionClassSymbol(fullName)
        cu.imports.addExtensionClass(newSymbol)
        newSymbol
      case clazz: IDClassDeclTree =>
        val id = clazz.id
        val fullName = (cu.pack.address :+ id.name).mkString("::")

        if (fullName in Main.Primitives) {
          globalScope.classes(fullName)
        } else {
          ensureClassNotDefined(id)
          val newSymbol = new ClassSymbol(fullName, classDecl.isAbstract)
          id.setSymbol(newSymbol)
          globalScope.classes += (fullName -> newSymbol)
          newSymbol
        }
    }

    sym.setPos(classDecl)
    classDecl.setSymbol(sym)
    classDecl.fields foreach { addSymbols(_, sym) }
    classDecl.methods foreach { addSymbols(_, sym) }
  }

  private def addSymbols(varDecl: VarDecl, classSymbol: ClassSymbol): Unit = {
    val id = varDecl.id
    val newSymbol = new FieldSymbol(id.name, varDecl.modifiers, classSymbol).setPos(varDecl)
    ensureIdentifierNotDefined(classSymbol.fields, id.name, varDecl)
    id.setSymbol(newSymbol)
    varDecl.setSymbol(newSymbol)

    val isStaticFinal = newSymbol.isStatic && newSymbol.isFinal
    if (classSymbol.isAbstract && !isStaticFinal)
      report(NonStaticFinalFieldInTrait(varDecl))

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
      case methDecl@MethodDecl(modifiers, _, _, retType, stat)      =>
        if (!classSymbol.isAbstract && stat.isEmpty)
          report(ClassUnimplementedMethod(methDecl))

        if (classSymbol.isAbstract && retType.isEmpty && stat.isEmpty)
          report(UnimplementedMethodNoReturnType(methDecl.signature, methDecl))

        new MethodSymbol(name, classSymbol, stat, modifiers)
      case conDecl@ConstructorDecl(modifiers, _, _, _, stat)        =>
        if (stat.isEmpty)
          report(AbstractConstructor(conDecl))

        val methSym = new MethodSymbol(name, classSymbol, stat, modifiers).setType(TUnit)
        if (modifiers.contains(Implicit()))
          methSym.annotations = Main.TImplicitConstructorAnnotation :: Nil
        methSym
      case opDecl@OperatorDecl(modifiers, operatorType, _, _, stat) =>
        if (stat.isEmpty)
          report(AbstractOperator(opDecl))

        new OperatorSymbol(operatorType, classSymbol, stat, modifiers)
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
    val fullName = cu.imports.getFullName(name)
    globalScope.classes.get(fullName) ifDefined { old =>
      report(ClassAlreadyDefined(name, old.line, id))
    }
  }

  private def ensureIdentifierNotDefined[T <: Symbol](map: Map[String, T], id: String, pos: Positioned): Unit =
    map.get(id) ifDefined { oldSymbol =>
      report(VariableAlreadyDefined(id, oldSymbol.line, pos))
    }


  /*-------------------------------- Binding symbols --------------------------------*/


  private def bind(tree: Tree): Unit = tree match {
    case extension@ExtensionDecl(tpe, methods)                           =>
      val extensionSym = extension.getSymbol.asInstanceOf[ExtensionClassSymbol]
      setType(tpe)
      extensionSym.setExtendedType(tpe.getType)
      methods foreach bind
    case classDecl@IDClassDeclTree(_, _, _, methods)                     =>
      setParentSymbol(classDecl)
      bindFields(classDecl)
      methods foreach bind
    case methDecl@MethodDecl(_, _, args, retType, stat)                  =>
      val methSym = methDecl.getSymbol

      retType ifDefined { tpe =>
        setType(tpe)
        methSym.setType(tpe.getType)
      }

      bindArguments(args)
      ensureMethodNotDefined(methDecl)
      stat ifDefined { new StatementBinder(methSym, methDecl.isStatic).bindStatement(_) }
    case constructorDecl@ConstructorDecl(_, _, args, _, stat)            =>

      bindArguments(args)
      ensureMethodNotDefined(constructorDecl)

      val conSym = constructorDecl.getSymbol
      stat ifDefined { new StatementBinder(conSym, false).bindStatement(_) }
    case operatorDecl@OperatorDecl(_, operatorType, args, retType, stat) =>
      val opSym = operatorDecl.getSymbol

      retType ifDefined { tpe =>
        val t = setType(tpe)
        opSym.setType(t)
        operatorType.setType(t)
      }

      bindArguments(args)
      ensureOperatorNotDefined(operatorDecl)

      val isStaticOperator = operatorDecl.isStatic

      val argTypes = opSym.argTypes
      val classSymbol = opSym.classSymbol

      // Ensure that operator pertains to the class defined in and that
      // types are not nullable
      val nullableTypes = (retType ++ args.map(_.tpe)).filterInstance[NullableType]

      // We don't want to report OperatorWrongTypes if types are nullable
      if (!operatorType.isInstanceOf[ArraySlice] && nullableTypes.nonEmpty) {
        nullableTypes.foreach(tpe => report(NullableInOperator(operatorType.opSign, tpe)))
      } else if (isStaticOperator) {
        val tpe = classSymbol match {
          case e: ExtensionClassSymbol => e.getExtendedType
          case _                       => TObject(classSymbol)
        }

        if (!argTypes.contains(tpe))
          report(OperatorWrongTypes(operatorType, argTypes, classSymbol, tpe.name, operatorDecl))
      }

      stat ifDefined { new StatementBinder(opSym, isStaticOperator).bindStatement(_) }
  }


  private def bindArguments(args: List[Formal]) =
    for (Formal(typeTree, id) <- args) {
      val tpe = setType(typeTree)
      id.setType(tpe)
    }

  private def bindFields(classDecl: ClassDeclTree) =
    classDecl.fields.foreach { case varDecl@VarDecl(typeTree, varId, init, _) =>
      typeTree ifDefined { t =>
        val tpe = setType(t)
        varId.setType(tpe)
      }

      init ifDefined { new StatementBinder(classDecl.getSymbol, varDecl.isStatic).bindExpr(_) }
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
          stats.dropRight(1)
            .collect { case Trees.UselessStatement(expr) => expr }
            .foreach { expr => report(UselessStatement(expr)) }

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

          localVars.get(id.name).filter(_.scopeLevel == scopeLevel) ifDefined { varId =>
            report(VariableAlreadyDefined(id.name, varId.symbol.line, varDecl))
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
            report(BreakContinueOutsideLoop(statement, statement))
          localVars
        case expr: ExprTree                                 =>
          bindExpr(expr, localVars, scopeLevel)
          localVars
        case _                                              => ???
      }

    def bindExpr(tree: ExprTree): Unit = bindExpr(tree, Map(), 0)


    private def bindExpr(startingTree: ExprTree, localVars: Map[String, VariableData], scopeLevel: Int): Unit = {
      val traverser = new Trees.Traverser {

        override def _traverse(t: Tree): Unit = t match {
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
                    // access object is not a variable, must be a class or a primitive
                    // eg. StaticClass.method()
                    val sym = name match {
                      case _ =>
                        globalScope.lookupClass(cu.imports, name) match {
                          case Some(classSymbol) => classSymbol
                          case None              =>
                            val alternatives = variableAlternatives(localVars) :::
                                               globalScope.classNames ::: Primitives.map(_.name)

                            report(CantResolveSymbol(name, alternatives, id))
                            new ClassSymbol("", false)
                        }
                    }
                    acc.obj = ClassID(name).setPos(id).setSymbol(sym)
                }
              case _                   =>
                _traverse(obj)
            }
            application match {
              case _: VariableID =>
              // This is a field. Since we don't know what class it belongs to we do nothing
              case _ => _traverse(application)
            }
          case Assign(to, expr)             =>
            _traverse(to)
            _traverse(expr)
            to ifInstanceOf[VariableID] { id =>
              setVariableUsed(id)
              setVariableReassigned(id)
            }
          case IncrementDecrementTree(expr) =>
            _traverse(expr)
            expr ifInstanceOf[VariableID] { id =>
              setVariableUsed(id)
              setVariableReassigned(id)
            }
          case thisTree: This               =>
            scope match {
              case methodSymbol: MethodSymbol =>
                if (isStaticContext)
                  report(ThisInStaticContext(thisTree))
                methodSymbol.classSymbol match {
                  case e: ExtensionClassSymbol => e.getExtendedType match {
                    case TObject(sym) => thisTree.setSymbol(sym)
                    case _            => // No symbol with other types
                  }
                  case classSymbol             => thisTree.setSymbol(classSymbol)
                }
              case _: ClassSymbol             => report(ThisInStaticContext(thisTree))
              case _                          => ???
            }
          case superSymbol@Super(specifier) =>
            scope match {
              case methodSymbol: MethodSymbol =>
                if (isStaticContext)
                  report(SuperInStaticContext(superSymbol))
                val parents = methodSymbol.classSymbol.parents

                specifier match {
                  case Some(spec) =>
                    parents.find(_.name == spec.name) match {
                      case Some(p) =>
                        superSymbol.setSymbol(p)
                      case None    =>
                        report(SuperSpecifierDoesNotExist(spec.name, methodSymbol.classSymbol.name, spec))
                    }
                  case None       =>
                    superSymbol.setSymbol(methodSymbol.classSymbol)
                  // Set symbol to this and let the typechecker decide later.
                }
              case _: ClassSymbol             => report(SuperInStaticContext(superSymbol))
              case _                          => ???
            }
          case tpe: TypeTree                => setType(tpe)
          case id: VariableID               => setVarIdentifierSymbol(id, localVars)
          case _                            => super._traverse(t)
        }
      }
      traverser.traverse(startingTree)
    }

    private def setVariableUsed(id: VariableID) = variableUsage += id.getSymbol -> true
    private def setVariableReassigned(id: VariableID) = variableReassignment += id.getSymbol -> true

    private def setVarIdentifierSymbol(id: VariableID, localVars: Map[String, VariableData]): Unit = {
      lookupVariableSymbol(id, localVars) match {
        case Some(symbol) => setVarIdentifierSymbol(id, symbol)
        case None         => report(CantResolveSymbol(id.name, variableAlternatives(localVars), id))
      }
    }

    private def setVarIdentifierSymbol(id: VariableID, symbol: VariableSymbol) = {
      id.setSymbol(symbol)
      variableUsage += symbol -> true
    }

    private def variableAlternatives(localVars: Map[String, VariableData]): List[String] = {

      def getLocalVars = localVars.keys.toList
      def getArguments(methodSymbol: MethodSymbol) = methodSymbol.argList.map(_.name)
      def getFields(classSymbol: ClassSymbol) = classSymbol.fields.filter { case (_, sym) =>
        !isStaticContext || sym.isStatic
      }.keys.toList

      scope match {
        case methodSymbol: MethodSymbol => // Binding symbols inside a method
          getLocalVars ::: getArguments(methodSymbol) ::: getFields(methodSymbol.classSymbol)
        case classSymbol: ClassSymbol   => // Binding symbols inside a class (fields)
          getFields(classSymbol)
        case _                          => ???
      }
    }

    private def lookupVariableSymbol(id: VariableID, localVars: Map[String, VariableData]): Option[VariableSymbol] = {
      val name = id.name

      def lookupLocalVar() = localVars.get(name).map(_.symbol)
      def lookupArgument(methodSymbol: MethodSymbol) = methodSymbol.lookupArgument(name)
      def lookupField(methodSymbol: MethodSymbol) = {
        val m = methodSymbol.lookupField(name)
        m map { sym =>
          if (isStaticContext && !sym.isStatic)
            report(AccessNonStaticFromStatic(id.name, id))
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
    // This is done in the binding stage since we are then guaranteed that
    // all types have been added to the global scope
    val name = meth.id.name
    val argTypes = meth.args.map(_.tpe.getType)
    val classSymbol = meth.getSymbol.classSymbol
    classSymbol.findMethod(name, argTypes, exactTypes = true) match {
      case Some(oldMeth) => report(MethodAlreadyDefined(meth.getSymbol.signature, oldMeth.line, meth))
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
    classSymbol.lookupOperator(operatorType, argTypes, cu.imports, exactTypes = true) match {
      case Some(oldOperator) => report(OperatorAlreadyDefined(op.signature, oldOperator.line, operator))
      case None              => operator.getSymbol.classSymbol.addOperator(op)
    }
  }

  private def setType(tpe: TypeTree): Type = {
    tpe match {
      case UnitType()                => tpe.setType(TUnit)
      case tpeId@ClassID(name, _)    =>
        globalScope.lookupClass(cu.imports, name) match {
          case Some(classSymbol) => tpeId.setSymbol(classSymbol)
          case None              =>
            val alternatives = globalScope.classNames
            report(UnknownType(name, alternatives, tpeId))
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
    val parents = classDecl.parents
    if (parents.isEmpty) {
      classDecl.getSymbol.parents ::= ObjectSymbol
      return
    }

    parents.foreach { parentId =>
      globalScope.lookupClass(cu.imports, parentId.name) match {
        case Some(parentSymbol) => parentId.setSymbol(parentSymbol)
        case None               => report(ParentNotDeclared(parentId.name, globalScope.classNames, parentId))
      }
    }

    classDecl.getSymbol.parents = classDecl.parents.map(_.getSymbol)
  }
}
