package tlang
package compiler
package analyzer

import tlang.compiler.analyzer.Knowledge.{Identifier, _}
import tlang.compiler.analyzer.Symbols.{ClassSymbol, FieldSymbol, VariableSymbol}
import tlang.compiler.analyzer.Types.TUnit
import tlang.compiler.ast.Trees
import tlang.compiler.ast.Trees._
import tlang.compiler.imports.Imports
import tlang.compiler.messages.Reporter
import tlang.compiler.output.Output
import tlang.compiler.output.debug.ASTOutput
import tlang.formatting.{ErrorStringContext, Formatter}
import tlang.utils.{Logging, Positioned}

object Flowing extends CompilerPhase[CompilationUnit, CompilationUnit] {

  override def run(ctx: Context)(cus: List[CompilationUnit]): List[CompilationUnit] = {
    ctx.executor.foreach(cus) { cu =>
      cu.classes foreach { clazz =>
        val errorStringContext = createErrorStringContext(ctx, cu)
        val flowAnalyser = ClassFlowAnalyser(ctx.reporter, errorStringContext, cu.imports)
        flowAnalyser(clazz)
      }
    }
    cus
  }

  override def description(implicit formatter: Formatter): String =
    "Performs data flow analysis and catches errors such as accessing objects that could potentially be null or using uninitialized variables."

  override def debugOutput(output: List[CompilationUnit])(implicit formatter: Formatter): Output = ASTOutput(phaseName, output)
}

case class ClassFlowAnalyser(
  override val reporter: Reporter,
  override val errorStringContext: ErrorStringContext,
  imports: Imports) extends FlowingErrors with Logging {

  override def replaceNames(str: String): String = imports.replaceNames(str)

  def apply(clazz: ClassDeclTree): Unit = {
    info"Performing flow analysis on ${ clazz.name }"
    val fieldKnowledge = clazz.fields.foldLeft(new Knowledge()) { analyzeField }
    clazz
      .methods
      .filter { _.stat.isDefined }
      .foreach { analyzeMethod(_, fieldKnowledge) }

    val hasDefaultConstructor = !clazz.methods.existsInstance[ConstructorDecl]
    if (hasDefaultConstructor) {
      verifyFinalFieldsInitialized(clazz.getSymbol, fieldKnowledge, "new()", clazz.id)
    }
  }

  def analyzeField(knowledge: Knowledge, field: VarDecl): Knowledge = {
    val sym = field.getSymbol
    val varId = VarIdentifier(sym)
    knowledge.assignment(varId, field.initiation)
  }

  def analyzeMethod(meth: MethodDeclTree, fieldKnowledge: Knowledge): Knowledge = {
    val methSymbol = meth.getSymbol
    val args = methSymbol.argList
    val argMap: Map[Identifier, Set[VarKnowledge]] = args
      .map { v => VarIdentifier(v) -> Set[VarKnowledge](Initialized) }
      .toMap
    val argKnowledge = new Knowledge(argMap)
    val knowledge: Knowledge = fieldKnowledge + argKnowledge

    val methodAnalyzer = MethodFlowAnalyzer(reporter, errorStringContext, meth, imports)

    val methodKnowledge = methodAnalyzer.analyze(meth.stat.get, knowledge)
    val returnType = methSymbol.getType
    if (returnType != TUnit && methodKnowledge.flowEnded.isEmpty) {
      report(NotAllPathsReturnAValue(meth.id))
    }

    meth match {
      case cons: ConstructorDecl =>
        verifyFinalFieldsInitialized(cons.getSymbol.classSymbol, methodKnowledge, cons.signature, cons.id)
      case _                     =>
    }
    methodKnowledge
  }

  def verifyFinalFieldsInitialized(classSymbol: ClassSymbol, knowledge: Knowledge, constructor: String, errorPos: Positioned): Unit = {
    classSymbol.fields
      .values
      .filter { _.isFinal }
      .foreach { fieldSymbol =>
        val id = VarIdentifier(fieldSymbol)
        if (!knowledge.is[Initialized](id)) {
          report(FieldMayNotHaveBeenInitialized(
            fieldSymbol.name,
            fieldSymbol.getPos,
            classSymbol.name,
            constructor,
            errorPos))
        }
      }
  }
}

case class MethodFlowAnalyzer(
  override val reporter: Reporter,
  override val errorStringContext: ErrorStringContext,
  methodDeclTree: MethodDeclTree,
  imports: Imports) extends FlowingErrors with Logging {

  def analyze(tree: StatTree, knowledge: Knowledge): Knowledge = {
    trace"Analyzing ${ tree.toString.stripNewlines.trim } at line ${ tree.line } with knowledge: $knowledge"
    tree match {
      case Block(stats)                      =>
        val endKnowledge = stats.foldLeft(knowledge) { (currentKnowledge, next) => analyze(next, currentKnowledge) }
        endKnowledge.flowEnded match {
          case Some(stat) if stat != stats.last =>
            val index = stats.indexOf(stat)
            val startStat = stats(index + 1)
            val endStat = stats.last
            val pos = Block(Nil).setPos(startStat, endStat)
            report(DeadCode(startStat.line, endStat.line, pos))
          case _                                =>
        }
        endKnowledge
      case VarDecl(id, _, init, _, _)        =>
        val varId = VarIdentifier(id.getSymbol)
        init match {
          case Some(initExpr) =>
            val afterInit = analyzeExpr(initExpr, knowledge)
            afterInit.assignment(varId, init)
          case None           => knowledge.assignment(varId, None)
        }
      case For(init, condition, post, stat)  =>
        val afterInit = init.foldLeft(knowledge) { (currentKnowledge, next) => analyze(next, currentKnowledge) }
        val afterCondition = analyzeCondition(condition, afterInit)
        val afterStat = analyze(stat, afterCondition)

        val afterPost = post.foldLeft(afterStat) { (knowledge, p) => analyze(p, knowledge) }
        knowledge.filterReassignedVariables(tree, afterPost)
      case Foreach(varDecl, container, stat) =>
        analyzeExpr(container, knowledge)
        checkValidUse(container, knowledge)
        val varId = VarIdentifier(varDecl.id.getSymbol)
        val varDeclKnowledge = knowledge.assignment(varId, None, Initialized)
        val afterStat = analyze(stat, varDeclKnowledge)
        knowledge.filterReassignedVariables(tree, afterStat)
      case If(condition, thn, els)           =>
        val afterCondition = analyzeCondition(condition, knowledge)
        val conditionKnowledge = afterCondition - knowledge
        val invertedCondition = knowledge + conditionKnowledge.invert

        val thnKnowledge = analyze(thn, afterCondition)
        val elsKnowledge = els map { e => analyze(e, invertedCondition) }
        val thnEnded = thnKnowledge.flowEnded.isDefined

        elsKnowledge match {
          case Some(elsKnowledge) =>
            val elsEnded = elsKnowledge.flowEnded.isDefined
            if (thnEnded)
              report(UnnecessaryElse(els.get))

            if (elsEnded && !thnEnded)
              thnKnowledge // we know the then branch has executed
            else if (!elsEnded && thnEnded)
              elsKnowledge // we know the else branch has executed
            else if (elsEnded && thnEnded)
              knowledge.endFlow(tree) // execution is dead after if branch
            else {
              val intersect = thnKnowledge.intersection(elsKnowledge, Some(tree))
              intersect
                .filterReassignedVariables(thn, thnKnowledge)
                .filterReassignedVariables(els.get, elsKnowledge)
            }
          case None               =>
            if (thnEnded)
              invertedCondition
            else
              knowledge.filterReassignedVariables(tree, thnKnowledge)
        }
      case While(condition, stat)            =>
        val afterCondition = analyzeCondition(condition, knowledge)
        val afterWhile = analyze(stat, afterCondition)
        knowledge.filterReassignedVariables(tree, afterWhile)
      case PrintStatTree(expr)               =>
        analyzeExpr(expr, knowledge)
      case Error(expr)                       =>
        analyzeExpr(expr, knowledge)
        knowledge.endFlow(tree)
      case Return(expr)                      =>
        expr ifDefined { analyzeExpr(_, knowledge) }
        knowledge.endFlow(tree)
      case _: Break | _: Continue            =>
        knowledge.endFlow(tree)
      case exprTree: ExprTree                =>
        analyzeExpr(exprTree, knowledge)
    }
  }

  def analyzeCondition(tree: ExprTree, knowledge: Knowledge): Knowledge = tree match {
    case And(lhs, rhs)                   =>
      val lhsKnowledge = analyzeCondition(lhs, knowledge)
      val rhsKnowledge = analyzeCondition(rhs, lhsKnowledge)
      val conditionKnowledge = rhsKnowledge - knowledge
      knowledge.addAndKnowledge(conditionKnowledge)
    case Or(lhs, rhs)                    =>
      val lhsKnowledge = analyzeCondition(lhs, knowledge)
      val rhsKnowledge = analyzeCondition(rhs, lhsKnowledge)
      val conditionKnowledge = rhsKnowledge - knowledge
      knowledge.addOrKnowledge(conditionKnowledge)
    case Not(expr)                       =>
      // Apply De Morgans law:
      expr match {
        case And(lhs, rhs) => analyzeCondition(Or(Not(lhs), Not(rhs)), knowledge)
        case Or(lhs, rhs)  => analyzeCondition(And(Not(lhs), Not(rhs)), knowledge)
        case _             =>
          getBoolVarCondition(expr, knowledge) match {
            case Some(condition) => analyzeCondition(Not(condition), knowledge)
            case None            =>
              val exprKnowledge = analyzeCondition(expr, knowledge)
              knowledge + (exprKnowledge - knowledge).invert
          }
      }
    case assignable: Assignable          =>
      analyzeExpr(assignable, knowledge)
      getBoolVarCondition(assignable, knowledge) match {
        case Some(condition) =>
          analyzeCondition(condition, knowledge)
        case None            =>
          // Not a bool var
          nullCheck(tree, assignable, knowledge, isNull = false)
      }
    case eq@EqualsOperatorTree(lhs, rhs) =>
      val isNull = tree.isInstanceOf[Equals]
      val lhsKnowledge = analyzeExpr(lhs, knowledge)
      var rhsKnowledge = analyzeExpr(rhs, lhsKnowledge)

      def getOther(exprTree: ExprTree): Option[ExprTree] = {
        val l = List(lhs, rhs)
        if (!l.contains(exprTree)) None else l.find(_ != exprTree)
      }

      if (eq.isInstanceOf[Equals])
        rhsKnowledge = analyzeComparison(eq, rhsKnowledge)

      getOther(NullLit()) ifDefined { x => return nullCheck(tree, x, rhsKnowledge, isNull) }

      getOther(TrueLit()) ifDefined { x =>
        getBoolVarCondition(x, rhsKnowledge) ifDefined { condition =>
          return analyzeCondition(condition, rhsKnowledge)
        }
      }

      getOther(FalseLit()) ifDefined { x =>
        getBoolVarCondition(x, rhsKnowledge) ifDefined { condition =>
          return analyzeCondition(Not(condition), rhsKnowledge)
        }
      }
      rhsKnowledge
    case comp: ComparisonOperatorTree    =>
      analyzeComparison(comp, knowledge)
    case _                               =>
      analyzeExpr(tree, knowledge)
  }

  def analyzeExpr(tree: ExprTree, topKnowledge: Knowledge): Knowledge = {
    var knowledge = topKnowledge
    val traverser = new Trees.Traverser {
      def traversal: TreeTraversal = {
        case Ternary(condition, thn, els)       =>
          val afterCondition = analyzeCondition(condition, knowledge)
          val conditionKnowledge = afterCondition - knowledge

          analyzeExpr(thn, afterCondition)
          analyzeExpr(els, knowledge + conditionKnowledge.invert)
        case acc@NormalAccess(obj, _)           =>
          traverseChildren(acc)
          checkValidUse(obj, knowledge)
        case assign@Assign(obj, from)           =>
          traverse(from)
          obj match {
            case _: VariableID =>
            // Don't analyze identifiers in assignments since we don't want to produce errors for variables being
            // reassigned, e.g if they're uninitialized
            case _ => analyzeExpr(obj, knowledge)
          }

          knowledge.getIdentifier(obj) match {
            case Some(varId) =>
              // Reset knowledge
              checkReassignment(varId, knowledge, assign)
              knowledge = knowledge.assignment(varId, Some(from), Reassigned(assign))
            case None        =>
              getField(obj) match {
                case Some(field) => checkReassignment(field, knowledge, isMaybeInitialized = true, assign)
                case None        =>
              }
          }
        case binOp@BinaryOperatorTree(lhs, rhs) =>
          traverse(lhs)
          traverse(rhs)
          binOp ifInstanceOf[Div] { _ =>
            knowledge.getNumericValue(rhs) ifDefined { v => if (v == 0) report(DivideByZero(rhs.toString, binOp)) }
          }
          binOp match {
            case _: EqualsOperatorTree | _: And | _: Or =>
            case _                                      =>
              // Rest of the binary operators need to be null checked before use
              val opArgumentTypes = binOp.lookupOperator((lhs.getType, rhs.getType), imports).get.argTypes

              if (!opArgumentTypes(0).isNullable)
                checkValidUse(lhs, knowledge)
              if (!opArgumentTypes(1).isNullable)
                checkValidUse(rhs, knowledge)
          }
        case ExtractNullable(expr)              =>
          traverse(expr)
        case op@UnaryOperatorTree(expr)         =>
          traverse(expr)
          op match {
            case _: Not => // Not can be used for nullable types
            case _      =>
              val opArgumentTypes = op.lookupOperator(expr.getType, imports).get.argTypes

              if (!opArgumentTypes.head.isNullable)
                checkValidUse(expr, knowledge)

              op.ifInstanceOf[IncrementDecrementTree] { incDec =>
                knowledge.getIdentifier(expr) match {
                  case Some(varId) =>
                    checkReassignment(varId, knowledge, op)
                    val v = if (incDec.isIncrement) 1 else -1
                    knowledge = knowledge.addToNumericValue(varId, v)
                  case None        =>
                    getField(incDec.expr) match {
                      case Some(field) => checkReassignment(field, knowledge, isMaybeInitialized = true, op)
                      case None        =>
                    }
                }
              }
          }
        case arrOp@ArrayOperatorTree(arr)       =>
          traverse(arr)

          // TODO: This currently doesn't work properly in loops
          //          arrOp match {
          //            case arrRead@ArrayRead(_, index) =>
          //              knowledge.getNumericValue(index) ifDefined { value =>
          //                if (value < 0)
          //                  report(OutOfBounds(index.toString, value, 0, arrRead))
          //                else
          //                  getArraySize(arr, knowledge) ifDefined { size =>
          //                    if (value >= size)
          //                      report(OutOfBounds(index.toString, value, size - 1, arrRead))
          //                  }
          //              }
          //            case _                           =>
          //          }
          checkValidUse(arr, knowledge)
        case v: VariableID                      =>
          knowledge.getIdentifier(v) ifDefined { varId =>
            verifyVariableInitialized(varId, v, knowledge)
          }
      }
    }
    traverser.traverse(tree)
    knowledge
  }

  @scala.annotation.tailrec
  private def getField(expr: ExprTree): Option[FieldSymbol] = expr match {
    case v: VariableID if v.getSymbol.isInstanceOf[FieldSymbol] => Some(v.getSymbol.asInstanceOf[FieldSymbol])
    case Access(_, application)                                 => getField(application)
    case _                                                      => None
  }

  private def analyzeComparison(comparison: BinaryOperatorTree, knowledge: Knowledge): Knowledge = {
    val lhs = comparison.lhs
    val rhs = comparison.rhs
    for ((a, b) <- List((lhs, rhs), (rhs, lhs)))
      knowledge.getIdentifier(a) ifDefined { varId =>
        knowledge.getNumericValue(b) ifDefined { numericValue =>
          val newKnowledge = comparison match {
            case _: LessThan          => Less(numericValue)
            case _: LessThanEquals    => LessEq(numericValue)
            case _: GreaterThan       => Greater(numericValue)
            case _: GreaterThanEquals => GreaterEq(numericValue)
            case _: Equals            => NumericValue(numericValue)
          }
          return knowledge.add(varId, newKnowledge)
        }
      }
    knowledge
  }

  private def checkReassignment(varId: Identifier, knowledge: Knowledge, pos: Positioned): Unit = {
    varId.symbol ifDefined { sym =>
      checkReassignment(sym, knowledge, knowledge.isMaybe[Initialized](varId), pos)
    }
  }

  private def checkReassignment(sym: VariableSymbol, knowledge: Knowledge, isMaybeInitialized: Boolean, pos: Positioned): Unit = {
    if (!sym.isFinal) {
      return
    }

    // We can only assign vals inside constructors
    val isConstructor = methodDeclTree.isInstanceOf[ConstructorDecl]
    if (!isConstructor || isMaybeInitialized) {
      report(ReassignmentToVal(sym.name, pos))
    }
  }

  private def getBoolVarCondition(obj: ExprTree, knowledge: Knowledge) =
    knowledge.getIdentifier(obj) flatMap { id => knowledge.get[BoolValue](id).map(_.condition) }

  private def getArraySize(obj: ExprTree, knowledge: Knowledge) =
    knowledge.getIdentifier(obj) flatMap { id => knowledge.get[ArraySize](id).map(_.size) }

  private def nullCheck(t: Tree, obj: ExprTree, knowledge: Knowledge, isNull: Boolean) =
    knowledge.getIdentifier(obj) match {
      case Some(varId) =>
        knowledge.get[IsNull](varId) match {
          case Some(IsNull(knownNull)) =>
            report(UnnecessaryCheck(obj.toString, knownNull, t))
            knowledge
          case None                    => knowledge.add(varId, IsNull(isNull))
        }
      case None        => knowledge
    }

  private def checkValidUse(obj: ExprTree, knowledge: Knowledge): Unit = {
    if (obj.isInstanceOf[ClassID])
      return

    val objTpe = obj.getType
    obj match {
      case NormalAccess(_, MethodCall(meth, _)) if meth.getType.isNullable =>
        if (objTpe.isNullable)
          report(AccessNullableMethod(meth.getSymbol.signature, obj))
      case _                                                               =>
        knowledge.getIdentifier(obj) ifDefined { varId =>
          if (objTpe.isNullable)
            knowledge.get[IsNull](varId) match {
              case Some(IsNull(isNull)) if isNull => report(AccessIsNull(obj.toString, obj))
              case None                           => report(AccessMightBeNull(obj.toString, obj))
              case _                              =>
            }

          verifyVariableInitialized(varId, obj, knowledge)
        }
    }
  }

  private def verifyVariableInitialized(varId: Identifier, obj: ExprTree, knowledge: Knowledge): Unit = {
    varId.symbol ifDefined { varSym =>
      if (!varSym.isInstanceOf[FieldSymbol] && !knowledge.is[Initialized](varId))
        report(VariableNotInitialized(obj.toString, obj))
    }
  }
}

