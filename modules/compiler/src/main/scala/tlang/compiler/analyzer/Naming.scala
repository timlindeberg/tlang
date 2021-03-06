package tlang
package compiler
package analyzer

import tlang.compiler.analyzer.Symbols.{AnnotationSymbol, _}
import tlang.compiler.analyzer.Types._
import tlang.compiler.ast.Trees
import tlang.compiler.ast.Trees._
import tlang.compiler.imports.ClassSymbolLocator
import tlang.compiler.messages.Reporter
import tlang.compiler.output.Output
import tlang.compiler.output.debug.ASTOutput
import tlang.formatting.{ErrorStringContext, Formatter}
import tlang.utils.{Logging, Positioned}

object Naming extends CompilerPhase[CompilationUnit, CompilationUnit] with Logging {

  def run(ctx: Context)(cus: List[CompilationUnit]): List[CompilationUnit] = {
    val globalScope = new GlobalScope(ClassSymbolLocator(ctx.classPath))

    // Add all symbols first so each program instance can access
    // all symbols in binding
    val analyzers = ctx.executor.map(cus) { cu =>
      val nameAnalyzer = NameAnalyser(ctx.reporter, createErrorStringContext(ctx, cu), cu, globalScope)
      nameAnalyzer.addSymbols()
      nameAnalyzer
    }

    analyzers.foreach { nameAnalyzer =>
      nameAnalyzer.bindIdentifiers()
      nameAnalyzer.checkInheritanceCycles()
      nameAnalyzer.checkVariableUsage()
      nameAnalyzer.checkVariableReassignments()
      nameAnalyzer.checkValidParenting()
    }

    cus
  }

  override def description(implicit formatter: Formatter): String =
    "Resolves names and attaches symbols to trees."

  override def debugOutput(output: List[CompilationUnit])(implicit formatter: Formatter): Output = ASTOutput(phaseName, output)
}

case class NameAnalyser(
  override val reporter: Reporter,
  override val errorStringContext: ErrorStringContext,
  cu: CompilationUnit,
  globalScope: GlobalScope) extends NamingErrors with Logging {

  override def replaceNames(str: String): String = cu.imports.replaceNames(str)

  private var variableUsage = Map[VariableSymbol, Boolean]()
  private var variableReassignment = Map[VariableSymbol, Boolean]()

  import errorStringContext.formatter

  def addSymbols(): Unit = {
    info"Adding symbols to ${ cu.sourceDescription }"
    cu.classes foreach addSymbols
  }

  def bindIdentifiers(): Unit = {
    info"Binding identifiers in ${ cu.sourceDescription }"
    cu.classes foreach bind
  }

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
      classSymbol.parents foreach { checkInheritanceCycles(_, newSet) }
    }

    globalScope.classes foreach { case (_, classSymbol) => checkInheritanceCycles(classSymbol, Set[ClassSymbol]()) }
  }

  def checkVariableUsage(): Unit =
    variableUsage
      .filter { case (variable, used) => !used && !variable.name.startsWith("_") }
      .foreach { case (variable, _) => report(UnusedVar(variable)) }

  def checkVariableReassignments(): Unit =
    variableReassignment
      .filter { case (_, reassigned) => !reassigned }
      .foreach { case (variable, _) => report(CouldBeVal(variable.name, variable)) }

  def checkValidParenting(): Unit =
    cu.classes.foreach { classDecl =>
      val nonTraits = classDecl.parents.filter(!_.getSymbol.isAbstract)
      if (nonTraits.lengthCompare(1) > 0) {
        report(ExtendMultipleClasses(nonTraits(1)))
        return
      }

      val nonTraitsAfterFirst = classDecl.parents.drop(1).filter(!_.getSymbol.isAbstract)
      if (nonTraitsAfterFirst.nonEmpty)
        report(NonFirstArgumentIsClass(nonTraits.head))
    }

  /*-------------------------------- Adding symbols --------------------------------*/

  private def addSymbols(classDecl: ClassDeclTree): Unit = {
    debug"Adding symbol to ${ classDecl.name }"

    val sym = createClassSymbol(classDecl)
    addAnnotations(sym, classDecl)
    sym.setPos(classDecl)
    classDecl.setSymbol(sym)
    classDecl.fields foreach { addSymbols(_, sym) }
    classDecl.methods foreach { addSymbols(_, sym) }
  }

  private def createClassSymbol(classDecl: ClassDeclTree): ClassSymbol = {
    val id = classDecl.id
    val fullName = (cu.pack.address :+ id.name).mkString("::")
    // This is here for when we compile the primitive classes.
    // Since they are already imported we'll get a conflict otherwise
    if (fullName in Constants.Primitives) {
      return globalScope.classes(fullName)
    }

    ensureClassNotDefined(id)

    val sym = classDecl match {
      case _: ClassDecl | _: TraitDecl =>
        new ClassSymbol(fullName)
      case _: ExtensionDecl            =>
        new ExtensionClassSymbol(fullName) use { cu.imports.addExtensionClass }
      case ann: AnnotationDecl         =>
        verifyAnnotationDeclaration(ann)
        new ClassSymbol(fullName)
    }
    sym.isAbstract = classDecl.isAbstract
    id.setSymbol(sym)
    globalScope.classes += (fullName -> sym)
    sym
  }

  private def addSymbols(varDecl: VarDecl, classSymbol: ClassSymbol): Unit = {
    debug"Adding field to ${ classSymbol.name }"

    val id = varDecl.id
    val sym = new FieldSymbol(id.name, varDecl.modifiers, classSymbol).setPos(varDecl)
    ensureIdentifierNotDefined(classSymbol.fields, id.name, varDecl)
    id.setSymbol(sym)
    varDecl.setSymbol(sym)

    val isStaticFinal = sym.isStatic && sym.isFinal
    if (classSymbol.isAbstract && !isStaticFinal)
      report(NonStaticFinalFieldInTrait(varDecl))

    // Check usage for private fields
    variableUsage += sym -> (varDecl.accessibility != Private())
    addAnnotations(sym, varDecl)

    classSymbol.addField(sym)
  }

  private def addSymbols(methDecl: MethodDeclTree, classSymbol: ClassSymbol): Unit = {
    debug"Adding symbol to ${ methDecl.signature }"

    val sym = createMethodSymbol(methDecl, classSymbol)
    addAnnotations(sym, methDecl)
    sym.setPos(methDecl)
    methDecl.id.setSymbol(sym)
    methDecl.setSymbol(sym)
    methDecl.args foreach { arg => addSymbols(arg, sym) }
  }

  private def createMethodSymbol(methDecl: MethodDeclTree, classSymbol: ClassSymbol): MethodSymbol = {
    val id = methDecl.id
    val name = id.name
    methDecl match {
      case methDecl@MethodDecl(_, modifiers, _, _, retType, stat)      =>
        if (!classSymbol.isAbstract && stat.isEmpty)
          report(ClassUnimplementedMethod(methDecl))

        if (classSymbol.isAbstract && retType.isEmpty && stat.isEmpty)
          report(UnimplementedMethodNoReturnType(methDecl.signature, methDecl))

        new MethodSymbol(name, classSymbol, stat, modifiers)
      case conDecl@ConstructorDecl(_, modifiers, _, _, _, stat)        =>
        if (stat.isEmpty)
          report(AbstractConstructor(conDecl))

        val methSym = new MethodSymbol(name, classSymbol, stat, modifiers).setType(TUnit)
        if (modifiers.contains(Implicit())) {
          val annotation = AnnotationSymbol(Constants.TImplicitConstructorAnnotation)
            .setType(Types.ImplicitConstructorAnnotation)
          methSym.addAnnotation(annotation)
        }
        methSym
      case opDecl@OperatorDecl(operatorType, modifiers, _, _, _, stat) =>
        if (stat.isEmpty)
          report(AbstractOperator(opDecl))

        new OperatorSymbol(operatorType, classSymbol, stat, modifiers)
    }
  }

  private def addSymbols(formal: Formal, methSymbol: MethodSymbol): Unit = {
    debug"Adding symbol to argument ${ formal.id.name } in ${ methSymbol.name }"

    val id = formal.id
    val modifiers: Set[Modifier] = Set(Private(), Final())
    val newSymbol = new VariableSymbol(id.name, modifiers).setPos(id)
    ensureIdentifierNotDefined(methSymbol.args, id.name, id)

    id.setSymbol(newSymbol)
    formal.setSymbol(newSymbol)

    addAnnotations(newSymbol, formal)
    methSymbol.addArgument(newSymbol)

    // Don't put out warning when args is unused since it's implicitly defined
    // or if the method is abstract
    variableUsage += newSymbol -> (methSymbol.isMainMethod || methSymbol.isAbstract)
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

  private def addAnnotations(sym: Symbol, annotatable: Annotatable): Unit = {
    annotatable.annotations foreach { annotation =>
      sym.addAnnotation(addAnnotationSymbol(annotation))
    }
  }

  private def addAnnotationSymbol(annotation: Annotation): AnnotationSymbol = {
    annotation.values foreach { case KeyValuePair(id, _) =>
      id.setSymbol(new VariableSymbol(id.name))
    }

    val elements = annotation.values map { case KeyValuePair(id, expr) =>
      val value = expr match {
        case IntLit(x)    => IntAnnotationValue(x)
        case LongLit(x)   => LongAnnotationValue(x)
        case FloatLit(x)  => FloatAnnotationValue(x)
        case DoubleLit(x) => DoubleAnnotationValue(x)
        case StringLit(x) => StringAnnotationValue(x)
        case _            =>
          report(AnnotationNeedsLiteralValue(expr))
          IntAnnotationValue(-1)
      }
      (id.name, value)
    }

    AnnotationSymbol(annotation.id.name, elements) use { annotation.setSymbol(_) }
  }

  private def verifyAnnotationDeclaration(annotation: AnnotationDecl): Unit = {
    annotation.methods foreach {
      case meth@(_: OperatorDecl)    => report(OperatorInAnnotation(meth))
      case meth@(_: ConstructorDecl) => report(ConstructorInAnnotation(meth))
      case meth                      =>
        if (meth.isStatic) {
          report(StaticInAnnotation(meth))
        }
        if (!meth.isAbstract) {
          report(ImplementedMethodInAnnotation(meth))
        }
        if (meth.accessibility != Public()) {
          report(PrivateMethodInAnnotation(meth))
        }
        meth.retType ifDefined { tpe =>
          if (setType(tpe) notIn Types.AnnotationTypes) {
            report(InvalidMethodReturnTypeInAnnotation(meth))
          }
        }
    }
  }

  /*-------------------------------- Binding symbols --------------------------------*/

  private def bind(tree: Tree): Unit = tree match {
    case annotation@Annotation(id, values)                                            =>
      setType(id)
      annotation.getSymbol.setType(annotation.id)
    case extension@ExtensionDecl(id, extendedType, methods, annotations)              =>
      setType(id)
      setType(extendedType)

      val extensionSym = extension.getSymbol.asInstanceOf[ExtensionClassSymbol]
      extensionSym.setExtendedType(extendedType.getType)
      methods foreach bind
      annotations foreach bind
    case classDecl@ClassDeclTree(_, _, _, methods, annotations)                       =>
      setParentSymbol(classDecl)
      bindFields(classDecl)
      if (!methods.existsInstance[ConstructorDecl]) {
        addDefaultConstructor(classDecl)
      }
      methods foreach bind
      annotations foreach bind
    case methDecl@MethodDecl(_, _, annotations, args, retType, stat)                  =>
      val methSym = methDecl.getSymbol

      retType ifDefined { tpe =>
        setType(tpe)
        methSym.setType(tpe.getType)
      }

      annotations foreach bind
      bindArguments(args)
      ensureMethodNotDefined(methDecl)
      stat ifDefined { new StatementBinder(methSym, methDecl.isStatic).bindStatement(_) }
    case constructorDecl@ConstructorDecl(_, _, annotations, args, _, stat)            =>
      annotations foreach bind
      bindArguments(args)
      ensureMethodNotDefined(constructorDecl)

      val conSym = constructorDecl.getSymbol
      stat ifDefined { new StatementBinder(conSym, false).bindStatement(_) }
    case operatorDecl@OperatorDecl(operatorType, _, annotations, args, retType, stat) =>

      val opSym = operatorDecl.getSymbol

      retType ifDefined { tpe =>
        val t = setType(tpe)
        opSym.setType(t)
        operatorType.setType(t)
      }

      annotations foreach bind
      bindArguments(args)
      ensureOperatorNotDefined(operatorDecl)

      val isStaticOperator = operatorDecl.isStatic

      val argTypes = opSym.argTypes
      val classSymbol = opSym.classSymbol

      if (isStaticOperator) {
        val tpe = classSymbol match {
          case e: ExtensionClassSymbol => e.getExtendedType
          case _                       => TObject(classSymbol)
        }

        if (!argTypes.contains(tpe))
          report(OperatorWrongTypes(operatorType.signature(argTypes), classSymbol, tpe.name, operatorDecl))
      }

      stat ifDefined { new StatementBinder(opSym, isStaticOperator).bindStatement(_) }
  }

  private def bindArguments(args: List[Formal]): Unit =
    for (Formal(typeTree, id, annotations) <- args) {
      val tpe = setType(typeTree)
      id.setType(tpe)
      annotations foreach bind
    }

  private def bindFields(classDecl: ClassDeclTree): Unit =
    classDecl.fields.foreach { case varDecl@VarDecl(varId, typeTree, init, _, annotations) =>
      annotations foreach bind
      typeTree ifDefined { t =>
        val tpe = setType(t)
        varId.setType(tpe)
      }

      init ifDefined { new StatementBinder(classDecl.getSymbol, varDecl.isStatic).bindExpr(_) }
    }

  private def addDefaultConstructor(classDecl: ClassDeclTree): Unit = {
    val methSym = new MethodSymbol("new", classDecl.getSymbol, Some(Block(Nil)), Set(Public())).setType(TUnit)
    classDecl.getSymbol.addMethod(methSym)
  }

  private class StatementBinder(scope: Symbol, isStaticContext: Boolean) {

    class VariableData(val symbol: VariableSymbol, val scopeLevel: Int)

    def bindStatement(tree: Tree): Unit = bind(tree, Map(), 0)

    private def bind(statement: Tree,
      localVars: Map[String, VariableData],
      scopeLevel: Int,
      canBreakContinue: Boolean = false
    ): Map[String, VariableData] =
      statement match {
        case Block(stats)                                      =>
          stats
            .dropRight(1) // The last statement is an implicit return
            .collect { case Trees.UselessStatement(expr) => expr }
            .foreach { expr => report(UselessStatement(expr)) }

          stats.foldLeft(localVars) { (currentLocalVars, nextStatement) =>
            bind(nextStatement, currentLocalVars, scopeLevel + 1, canBreakContinue)
          }
          localVars
        case varDecl@VarDecl(id, typeTree, init, modifiers, _) =>
          val newSymbol = new VariableSymbol(id.name, modifiers).setPos(id)
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
        case For(init, condition, post, stat)                  =>
          val newVars = init.foldLeft(localVars) { (currentLocalVars, nextStatement) =>
            bind(nextStatement, currentLocalVars, scopeLevel + 1)
          }
          bind(condition, newVars, scopeLevel)
          post.foreach(bind(_, newVars, scopeLevel, canBreakContinue))
          bind(stat, newVars, scopeLevel + 1, canBreakContinue = true)
          localVars
        case Foreach(varDecl, container, stat)                 =>
          val newVars = bind(varDecl, localVars, scopeLevel)
          bind(container, localVars, scopeLevel)
          bind(stat, newVars, scopeLevel + 1, canBreakContinue = true)
          localVars
        case If(condition, thn, els)                           =>
          bind(condition, localVars, scopeLevel)
          bind(thn, localVars, scopeLevel, canBreakContinue)
          els ifDefined { els => bind(els, localVars, scopeLevel, canBreakContinue) }
          localVars
        case While(condition, stat)                            =>
          bind(condition, localVars, scopeLevel)
          bind(stat, localVars, scopeLevel, canBreakContinue = true)
          localVars
        case PrintStatTree(expr)                               =>
          bind(expr, localVars, scopeLevel)
          localVars
        case Error(expr)                                       =>
          bind(expr, localVars, scopeLevel)
          localVars
        case Return(expr)                                      =>
          expr ifDefined { expr => bind(expr, localVars, scopeLevel) }
          localVars
        case _: Break | _: Continue                            =>
          if (!canBreakContinue) {
            val breakOrContinue = if (statement.isInstanceOf[Break]) "break" else "continue"
            report(BreakContinueOutsideLoop(breakOrContinue, statement))
          }
          localVars
        case expr: ExprTree                                    =>
          bindExpr(expr, localVars, scopeLevel)
          localVars
        case _                                                 => ???
      }

    def bindExpr(tree: ExprTree): Unit = bindExpr(tree, Map(), 0)

    private def bindExpr(startingTree: ExprTree, localVars: Map[String, VariableData], scopeLevel: Int): Unit = {
      val traverser = new Trees.Traverser {
        def traversal: TreeTraversal = {
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
                              globalScope.classNames :::
                              Primitives.map(_.name)

                            report(CantResolveSymbol(name, alternatives, id))
                            new ClassSymbol("")
                        }
                    }
                    acc.obj = ClassID(name).setPos(id).setSymbol(sym)
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
            traverse(to)
            traverse(expr)
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
                  case Some(classSpecifier) =>
                    parents.find { _.name == classSpecifier.name } match {
                      case Some(parent) =>
                        superSymbol.setSymbol(parent)
                        classSpecifier.setSymbol(parent)
                      case None         =>
                        report(SuperSpecifierDoesNotExist(classSpecifier.name, methodSymbol.classSymbol.name, classSpecifier))
                    }
                  case None                 =>
                    superSymbol.setSymbol(methodSymbol.classSymbol)
                  // Set symbol to this and let the typechecker decide later.
                }
              case _: ClassSymbol             => report(SuperInStaticContext(superSymbol))
              case _                          => ???
            }
          case tpe: TypeTree                => setType(tpe)
          case id: VariableID               => setVarIdentifierSymbol(id, localVars)
        }
      }
      traverser.traverse(startingTree)
    }

    private def setVariableUsed(id: VariableID): Unit = variableUsage += id.getSymbol -> true
    private def setVariableReassigned(id: VariableID): Unit = variableReassignment += id.getSymbol -> true

    private def setVarIdentifierSymbol(id: VariableID, localVars: Map[String, VariableData]): Unit = {
      lookupVariableSymbol(id, localVars) match {
        case Some(symbol) => setVarIdentifierSymbol(id, symbol)
        case None         => report(CantResolveSymbol(id.name, variableAlternatives(localVars), id))
      }
    }

    private def setVarIdentifierSymbol(id: VariableID, symbol: VariableSymbol): Unit = {
      debug"Binding id ${ id.name } to symbol ${ symbol.repr }"
      id.setSymbol(symbol)
      variableUsage += symbol -> true
    }

    private def variableAlternatives(localVars: Map[String, VariableData]): List[String] = {

      def getLocalVars = localVars.keys.toList

      def getArguments(methodSymbol: MethodSymbol) = methodSymbol.argList.map(_.name)

      def getFields(classSymbol: ClassSymbol) =
        classSymbol.fields
          .filter { case (_, sym) => !isStaticContext || sym.isStatic }
          .keys
          .toList

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
        val field = methodSymbol.classSymbol.lookupField(name)
        field foreach { sym =>
          if (isStaticContext && !sym.isStatic)
            report(AccessNonStaticFromStatic(id.name, id))
        }
        field
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
    // This is done in the binding phase since we are then guaranteed that
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
    // This is done in the binding phase since we are then guaranteed that
    // all types have been added to the global scope
    val operatorType = operator.operatorType
    val argTypes = operator.args.map(_.tpe.getType)

    val op = operator.getSymbol.asInstanceOf[OperatorSymbol]
    val classSymbol = operator.getSymbol.classSymbol
    classSymbol.lookupOperator(operatorType, argTypes, cu.imports, exactTypes = true) match {
      case Some(oldOperator) => report(OperatorAlreadyDefined(op.signature, oldOperator, operator))
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
    val classSymbol = classDecl.getSymbol

    if (parents.isEmpty) {
      classSymbol.addParent(ObjectSymbol)
      return
    }

    parents.foreach { parentId =>
      globalScope.lookupClass(cu.imports, parentId.name) match {
        case Some(parentSymbol) => parentId.setSymbol(parentSymbol)
        case None               => report(ParentNotDeclared(parentId.name, globalScope.classNames, parentId))
      }
    }

    classSymbol.parents = parents.map(_.getSymbol)
  }
}
