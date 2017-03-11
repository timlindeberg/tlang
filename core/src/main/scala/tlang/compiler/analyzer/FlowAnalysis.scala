package tlang.compiler.analyzer

import tlang.compiler.analyzer.Knowledge.{Identifier, _}
import tlang.compiler.analyzer.Symbols.FieldSymbol
import tlang.compiler.ast.Trees
import tlang.compiler.ast.Trees._
import tlang.compiler.imports.ImportMap
import tlang.compiler.utils.Positioned
import tlang.compiler.{Context, Pipeline}
import tlang.utils.Extensions._

/**
  * Created by timlindeberg on 22/07/16.
  */
object FlowAnalysis extends Pipeline[CompilationUnit, CompilationUnit] {

  override def run(ctx: Context)(cus: List[CompilationUnit]): List[CompilationUnit] = {
    cus foreach { cu =>
      cu.classes foreach { clazz =>
        val flowAnalyser = new FlowAnalyser(ctx, cu.importMap)
        flowAnalyser(clazz)
      }
    }
    cus
  }

}

class FlowAnalyser(override val ctx: Context, override val importMap: ImportMap) extends FlowAnalysisErrors {

  def apply(clazz: ClassDeclTree): Unit = {

    val fieldKnowledge = clazz.fields.foldLeft(new Knowledge()) { (knowledge, field) =>
      val sym = field.getSymbol
      val varId = VarIdentifier(sym)
      if (sym.modifiers.contains(Final()))
        knowledge.assignment(varId, field.initation, Initialized)
      else
        knowledge.add(varId, Initialized)
    }

    clazz.methods.filter(_.stat.isDefined) foreach { meth =>
      val args = meth.getSymbol.argList
      val argMap: Map[Identifier, Set[VarKnowledge]] =
        args.map { v => VarIdentifier(v) -> Set[VarKnowledge](Initialized) }.toMap
      val argKnowledge = new Knowledge(argMap)
      val knowledge: Knowledge = fieldKnowledge + argKnowledge
      analyze(meth.stat.get, knowledge)
    }

  }

  def analyze(tree: StatTree, knowledge: Knowledge): Knowledge = {
    //println(s"${tree.line}: $tree")
    //println(knowledge)
    //println("----------------------------------------")
    tree match {
      case Block(stats)                      =>
        val endKnowledge = stats.foldLeft(knowledge)((currentKnowledge, next) => analyze(next, currentKnowledge))
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
      case VarDecl(_, id, init, _)           =>
        val varId = VarIdentifier(id.getSymbol)
        init match {
          case Some(i) =>
            val afterInit = analyzeExpr(i, knowledge)
            afterInit.assignment(varId, init)
          case None    => knowledge.assignment(varId, None)
        }
      case For(init, condition, post, stat)  =>
        val afterInit = init.foldLeft(knowledge)((currentKnowledge, next) => analyze(next, currentKnowledge))
        val afterCondition = analyzeCondition(condition, afterInit)
        val afterStat = analyze(stat, afterCondition)

        val afterPost = post.foldLeft(afterStat)((knowledge, p) => analyze(p, knowledge))
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
              intersect.filterReassignedVariables(thn, thnKnowledge).filterReassignedVariables(els.get, elsKnowledge)
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
        expr ifDefined {analyzeExpr(_, knowledge)}
        knowledge.endFlow(tree)
      case _: Break | _: Continue            =>
        knowledge.endFlow(tree)
      case exprTree: ExprTree                =>
        analyzeExpr(exprTree, knowledge)
    }
  }

  def analyzeCondition(tree: ExprTree, knowledge: Knowledge): Knowledge = tree match {
    case And(lhs, rhs)                =>
      val lhsKnowledge = analyzeCondition(lhs, knowledge)
      val rhsKnowledge = analyzeCondition(rhs, lhsKnowledge)
      val conditionKnowledge = rhsKnowledge - knowledge
      knowledge.addAndKnowledge(conditionKnowledge)
    case Or(lhs, rhs)                 =>
      val lhsKnowledge = analyzeCondition(lhs, knowledge)
      val rhsKnowledge = analyzeCondition(rhs, lhsKnowledge)
      val conditionKnowledge = rhsKnowledge - knowledge
      knowledge.addOrKnowledge(conditionKnowledge)
    case Not(expr)                    =>
      // Apply De Morgans law:
      expr match {
        case And(lhs, rhs) =>
          analyzeCondition(Or(Not(lhs), Not(rhs)), knowledge)
        case Or(lhs, rhs)  =>
          analyzeCondition(And(Not(lhs), Not(rhs)), knowledge)
        case _             =>
          getBoolVarCondition(expr, knowledge) match {
            case Some(condition) => analyzeCondition(Not(condition), knowledge)
            case None            =>
              val exprKnowledge = analyzeCondition(expr, knowledge)
              knowledge + (exprKnowledge - knowledge).invert
          }
      }
    case assignable: Assignable       =>
      analyzeExpr(assignable, knowledge)
      getBoolVarCondition(assignable, knowledge) match {
        case Some(condition) =>
          analyzeCondition(condition, knowledge)
        case None            =>
          // Not a bool var
          nullCheck(tree, assignable, knowledge, isNull = false)
      }
    case EqualsOperatorTree(lhs, rhs) =>
      val isNull = tree.isInstanceOf[Equals]
      val lhsKnowledge = analyzeExpr(lhs, knowledge)
      val rhsKnowledge = analyzeExpr(rhs, lhsKnowledge)

      // Returns the value which
      def getOther(exprTree: ExprTree): Option[ExprTree] = {
        val l = List(lhs, rhs)
        if (!l.contains(exprTree))
          return None
        l.find(_ != exprTree)
      }

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
    case _                            =>
      analyzeExpr(tree, knowledge)
  }

  def analyzeExpr(tree: ExprTree, topKnowledge: Knowledge): Knowledge = {
    var knowledge = topKnowledge
    val traverser = new Trees.Traverser {
      override def _traverse(t: Tree): Unit = t match {
        case Ternary(condition, thn, els)       =>
          val afterCondition = analyzeCondition(condition, knowledge)
          val conditionKnowledge = afterCondition - knowledge

          analyzeExpr(thn, afterCondition)
          analyzeExpr(els, knowledge + conditionKnowledge.invert)
        case acc@Access(obj, _)                 =>
          super._traverse(acc)
          checkValidUse(obj, knowledge)
        case assign@Assign(obj, from)           =>
          super._traverse(t)

          knowledge.getIdentifier(obj) ifDefined { varId =>
            // Reset knowledge
            checkReassignment(varId, t)
            knowledge = knowledge.assignment(varId, Some(from), Reassigned(assign))
          }
        case binOp@BinaryOperatorTree(lhs, rhs) =>
          _traverse(lhs)
          _traverse(rhs)
          binOp ifInstanceOf[Div] { _ =>
            knowledge.getNumericValue(rhs) ifDefined { v => if (v == 0) report(DivideByZero(rhs, binOp)) }
          }
          binOp match {
            case _: EqualsOperatorTree | _: And | _: Or =>
            case _                                      =>
              // Rest of the binary operators need to be null checked before use
              checkValidUse(lhs, knowledge)
              checkValidUse(rhs, knowledge)
          }
        case ExtractNullable(expr)              =>
          _traverse(expr)
        case op@UnaryOperatorTree(expr)         =>
          _traverse(expr)
          checkValidUse(expr, knowledge)
          op.ifInstanceOf[IncrementDecrementTree]({ incDec =>
            knowledge.getIdentifier(expr) ifDefined { varId =>
              checkReassignment(varId, t)
              val v = if (incDec.isIncrement) 1 else -1
              knowledge = knowledge.addToNumericValue(varId, v)
            }
          })
        case ArrayOperatorTree(arr)             =>
          _traverse(arr)

          t match {
            case arrRead@ArrayRead(_, index) =>
              knowledge.getNumericValue(index) ifDefined { value =>
                if (value < 0)
                  report(OutOfBounds(index, value, 0, arrRead))
                else
                  getArraySize(arr, knowledge) ifDefined { size =>
                    if (value >= size)
                      report(OutOfBounds(index, value, size - 1, arrRead))
                  }
              }
            case _                           =>
          }
          checkValidUse(arr, knowledge)
        case _                                  =>
          super._traverse(t)
      }
    }
    traverser.traverse(tree)
    knowledge
  }

  private def checkReassignment(varId: Identifier, pos: Positioned) =
    varId.symbol ifDefined { sym =>
      if (sym.modifiers.contains(Final()))
        report(ReassignmentToVal(sym.name, pos))
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
            report(UnnecessaryCheck(obj, knownNull, t))
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
        knowledge.getIdentifier(obj) match {
          case Some(varId) =>
            if (objTpe.isNullable)
              knowledge.get[IsNull](varId) match {
                case Some(IsNull(isNull)) if isNull => report(AccessIsNull(obj, obj))
                case None                           => report(AccessMightBeNull(obj, obj))
                case _                              =>
              }
            varId.symbol ifDefined { varSym =>
              if (!varSym.isInstanceOf[FieldSymbol] && knowledge.get[Initialized](varId).isEmpty)
                report(VariableNotInitialized(obj, obj))
            }
          case _           =>
        }
    }
  }

}