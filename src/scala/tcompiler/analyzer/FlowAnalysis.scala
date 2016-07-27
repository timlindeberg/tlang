package tcompiler.analyzer

import tcompiler.analyzer.Symbols.{FieldSymbol, VariableSymbol}
import tcompiler.analyzer.Types.{PrimitiveType, TNull, TObject, Type}
import tcompiler.ast.{ForeachTraverser, Printer, TreeTraverser}
import tcompiler.ast.Trees._
import tcompiler.imports.ImportMap
import tcompiler.utils.{Context, Pipeline}
import tcompiler.utils.Extensions._

import scala.collection.mutable.ListBuffer

/**
  * Created by timlindeberg on 22/07/16.
  */
object FlowAnalysis extends Pipeline[List[CompilationUnit], List[CompilationUnit]] {

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

class FlowAnalyser(override var ctx: Context, override var importMap: ImportMap) extends FlowAnalysisErrors {

  case class VarIdentifier(v: VariableSymbol, fields: List[VariableSymbol]) {

    def value = if (fields.isEmpty) v else fields.last
    override def toString = {
      val f = if (fields.isEmpty) "Nil" else s"[ ${fields.mkString(", ")} ]"
      s"VarIdentifier($v, $f)"
    }
  }

  case class Knowledge(varKnowledge: Map[VarIdentifier, Set[VarKnowledge]] = Map(),
                       events: Map[Int, Set[(VarIdentifier, VarKnowledge)]] = Map(),
                       flowEnded: Option[StatTree] = None) {

    def addLocalVar(v: VariableSymbol, knowledge: Set[VarKnowledge], fields: List[VariableSymbol] = Nil) = {
      val vId = VarIdentifier(v, fields)
      copy(varKnowledge = varKnowledge + (vId -> knowledge))
    }

    def isNull(v: VarIdentifier): Option[Boolean] =
      varKnowledge.getOrElse(v, Set()).find(_.isInstanceOf[IsNull]) match {
        case Some(IsNull(value)) => Some(value)
        case _                   => None
      }

    def nullCheck(t: Tree, varId: VarIdentifier, isNull: Boolean) = {
      val nullKnowledge = IsNull(isNull)
      val event = getEvent(t, varId, nullKnowledge)
      val oldKnowledge = varKnowledge.getOrElse(varId, Set()).filterNotType[IsNull]
      val newKnowledge = oldKnowledge + nullKnowledge
      copy(varKnowledge = varKnowledge + (varId -> newKnowledge), events = events + event)
    }

    def endFlow(at: StatTree) = copy(flowEnded = Some(at))

    def eventsFrom(t: Tree): Set[(VarIdentifier, VarKnowledge)] = events.getOrElse(System.identityHashCode(t), Set())

    def invertKnowledgeFrom(t: Tree): Knowledge =
      eventsFrom(t).foldLeft(this)((knowledge, event) => event match {
        case (varId, vKnowledge) =>
          // replace the knowledge with the inverted knowledge
          val invertedKnowledge = varKnowledge(varId) - vKnowledge + vKnowledge.invert
          knowledge.copy(varKnowledge = varKnowledge + (varId -> invertedKnowledge))
      })

    def and(and: Tree, lhs: Tree, rhs: Tree): Knowledge = {
      val hash = System.identityHashCode(and)
      val topSet = eventsFrom(lhs) ++ eventsFrom(rhs)
      copy(events = events + (hash -> topSet))
    }

    def or(or: Tree, topSet: Set[(VarIdentifier, VarKnowledge)]): Knowledge = {
      val hash = System.identityHashCode(or)
      // Wrap knowledge in OrCheck thingys
      val newKnowledge = topSet.map { case (varId, k) =>
        val v = varKnowledge(varId)
        val set = if(v.contains(k)) v else v + OrCheck(k)
        varId -> set
      }.toMap
      val newEventSets: Set[(VarIdentifier, VarKnowledge)] = topSet.map { case (varId, k) => (varId, OrCheck(k)) }

      val newEvents = events + (hash -> newEventSets)
      copy(varKnowledge = varKnowledge ++ newKnowledge, events = newEvents)
    }

    private def getEvent(t: Tree, varId: VarIdentifier, newKnowledge: VarKnowledge) = {
      val hash = System.identityHashCode(t)
      val e = events.getOrElse(hash, Set())
      hash -> (e + (varId -> newKnowledge))
    }

    override def toString = {
      val vars = varKnowledge.map { case (VarIdentifier(v, fields), knowledge) =>
        val f = if (fields.isEmpty) "" else fields.mkString(".", ".", "")
        s"$v$f -> { ${knowledge.mkString(", ")} }"
      }.mkString("\n")

      val flow = flowEnded match {
        case Some(ended) => s"\nFlow ended at ${ended.line}: $ended"
        case None        => ""
      }
      vars + flow
    }
  }

  trait VarKnowledge {
    def invert: VarKnowledge = this
  }

  case object Initialized extends VarKnowledge
  case object Used extends VarKnowledge
  case object Reassigned extends VarKnowledge
  case class IsNull(value: Boolean) extends VarKnowledge {
    override def invert = IsNull(!value)
  }
  case class OrCheck(v1: VarKnowledge) extends VarKnowledge {
    override def invert = v1.invert
  }

  def apply(clazz: ClassDecl): Unit = {
    def getVarKnowledgeMap(list: List[VariableSymbol]): Map[VarIdentifier, Set[VarKnowledge]] =
      list.map(v => VarIdentifier(v, Nil) -> Set[VarKnowledge](Initialized)).toMap

    val fieldKnowledge = getVarKnowledgeMap(clazz.fields.map(_.getSymbol))
    clazz.methods.filter(_.stat.isDefined) foreach { meth =>
      val argKnowledge = getVarKnowledgeMap(meth.getSymbol.argList)
      val knowledge = Knowledge(varKnowledge = fieldKnowledge ++ argKnowledge)
      analyze(meth.stat.get, knowledge)
    }
  }

  def analyze(tree: StatTree, knowledge: Knowledge): Knowledge = {
    println(s"${tree.line}: $tree")
    println(knowledge)
    println("------------------------------------------------")
    tree match {
      case Block(stats)                      =>
        val endKnowledge = stats.foldLeft(knowledge)((currentKnowledge, next) => analyze(next, currentKnowledge))
        endKnowledge.flowEnded match {
          case Some(stat) if stat != stats.last => WarningDeadCode(stat.line, stats.last.line, stat)
          case _                                =>
        }
        knowledge
      case varDecl@VarDecl(tpe, id, init, _) =>
        val varSymbol = id.getSymbol
        var varKnowledge: Set[VarKnowledge] = init.map(varIsNull).getOrElse(Set())

        if (init.isDefined || varSymbol.getType.isInstanceOf[PrimitiveType])
          varKnowledge += Initialized
        knowledge.addLocalVar(varSymbol, varKnowledge)
      case For(init, condition, post, stat)  =>
        val afterInit = init.foldLeft(knowledge)((currentKnowledge, next) => analyze(next, currentKnowledge))
        val afterCondition = analyzeCondition(condition, afterInit)
        analyze(stat, afterCondition)

        post.foreach(analyze(_, afterCondition))
        knowledge
      case Foreach(varDecl, container, stat) =>
        analyzeExpr(container, knowledge)
        checkNull(container, knowledge)
        val newKnowledge = analyze(varDecl, knowledge)
        analyze(stat, newKnowledge)
        knowledge
      case If(condition, thn, els)           =>
        val afterCondition = analyzeCondition(condition, knowledge)
        val thnKnowledge = analyze(thn, afterCondition)
        els ifDefined {analyze(_, afterCondition.invertKnowledgeFrom(condition))}
        if (thnKnowledge.flowEnded.isDefined)
          afterCondition.invertKnowledgeFrom(condition)
        else
          knowledge
      case While(condition, stat)            =>
        val afterCondition = analyzeCondition(condition, knowledge)
        analyze(stat, afterCondition)
        knowledge
      case PrintStatTree(expr)               =>
        analyze(expr, knowledge)
      case Error(expr)                       =>
        analyze(expr, knowledge)
        knowledge.endFlow(tree)
      case Return(expr)                      =>
        expr ifDefined {analyze(_, knowledge)}
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
      rhsKnowledge.and(tree, lhs, rhs)
    case Or(lhs, rhs)                    =>
      val lhsKnowledge = analyzeCondition(lhs, knowledge)
      val rhsKnowledge = analyzeCondition(rhs, lhsKnowledge)
      val orKnowledge = lhsKnowledge.eventsFrom(lhs) ++ rhsKnowledge.eventsFrom(rhs)
      knowledge.or(tree, orKnowledge)
    case Not(expr)                       =>
      // Apply De Morgans law:
      expr match {
        case And(lhs, rhs) =>
          analyzeCondition(Or(Not(lhs), Not(rhs)), knowledge)
        case Or(lhs, rhs)  =>
          analyzeCondition(And(Not(lhs), Not(rhs)), knowledge)
        case _             =>
          val exprKnowledge = analyzeCondition(expr, knowledge)
          exprKnowledge.invertKnowledgeFrom(expr)
      }
    case acc@Access(obj, app)            =>
      analyzeExpr(acc, knowledge)
      nullCheck(tree, knowledge, acc, isNull = false)
    case v: VariableID                   =>
      nullCheck(tree, knowledge, v, isNull = false)
    case eq@EqualsOperatorTree(lhs, rhs) =>
      val isNull = eq.isInstanceOf[Equals]
      val lhsKnowledge = analyzeExpr(lhs, knowledge)
      val rhsKnowledge = analyzeExpr(rhs, lhsKnowledge)
      if (lhs == NullLit())
        nullCheck(tree, rhsKnowledge, rhs, isNull)
      else if (rhs == NullLit())
        nullCheck(tree, rhsKnowledge, lhs, isNull)
      else
        knowledge
    case _                               =>
      analyzeExpr(tree, knowledge)
  }

  def analyzeExpr(tree: ExprTree, topKnowledge: Knowledge): Knowledge = {
    var knowledge = topKnowledge
    val traverser = new TreeTraverser {
      override def traverse(t: Tree) = t match {
        case Ternary(condition, thn, els)         =>
          val afterCondition = analyzeExpr(condition, knowledge)
          analyzeExpr(thn, afterCondition)
          analyzeExpr(els, afterCondition.invertKnowledgeFrom(condition))
        case acc@Access(obj, app)                 =>
          traverse(obj, app)
          checkNull(obj, knowledge)
        case Assign(v@VariableID(name), exprTree) =>
          // Reset knowledge
          traverse(exprTree)
          val varKnowledge = varIsNull(exprTree) + Initialized + Reassigned
          knowledge = knowledge.addLocalVar(v.getSymbol, varKnowledge)
        case eq@EqualsOperatorTree(lhs, rhs)      =>
          traverse(lhs, rhs)
        case BinaryOperatorTree(lhs, rhs)         =>
          // Rest of the binary operators need to be null checked before use
          traverse(lhs, rhs)
          checkNull(lhs, knowledge)
          checkNull(rhs, knowledge)
        case UnaryOperatorTree(expr)              =>
          traverse(expr)
          checkNull(expr, knowledge)
        case ArrayOperatorTree(arr)               =>
          traverse(arr)
          checkNull(arr, knowledge)
        case _                                    =>
          super.traverse(t)
      }
    }
    traverser.traverse(tree)
    knowledge
  }

  private def checkNull(obj: ExprTree, knowledge: Knowledge): Unit = {
    if (!obj.getType.isNullable)
      return

    obj match {
      case NormalAccess(_, MethodCall(meth, _)) => ErrorAccessNullableMethod(meth.getSymbol.signature, obj)
      case _                                    =>
        getVarIdentifier(obj) ifDefined {
          varId =>
            knowledge.isNull(varId) match {
              case Some(isNull) if isNull => ErrorAccessIsNull(varId.value, obj)
              case None                   => ErrorAccessMightBeNull(varId.value, obj)
              case _                      =>
            }
        }
    }
  }

  private def getVarIdentifier(obj: ExprTree): Option[VarIdentifier] = {
    obj match {
      case v: VariableID    => Some(VarIdentifier(v.getSymbol, Nil))
      case Assign(to, from) => getVarIdentifier(to)
      case access: Access   =>
        var acc: ExprTree = access
        val fields = new ListBuffer[VariableSymbol]()
        while (acc.isInstanceOf[Access]) {
          val s = acc.asInstanceOf[Access]
          s.application match {
            case v: VariableID => fields += v.getSymbol
            case _             => return None
          }
          acc = s.obj
        }
        val revFields = fields.reverse.toList
        acc match {
          case v: VariableID => Some(VarIdentifier(v.getSymbol, revFields))
          case v: ClassID    =>
            if (revFields.nonEmpty)
              Some(VarIdentifier(revFields.head, revFields.drop(1)))
            else access.application match {
              case v: VariableID => Some(VarIdentifier(v.getSymbol, Nil))
              case _             => None
            }
          case _             => None
        }
      case _                => None
    }

  }

  private def nullCheck(t: Tree, knowledge: Knowledge, obj: ExprTree, isNull: Boolean) = {
    getVarIdentifier(obj) match {
      case Some(v) => knowledge.nullCheck(t, v, isNull)
      case _       => knowledge
    }
  }

  private def varIsNull(assignment: ExprTree) = {
    var varKnowledge: Set[VarKnowledge] = Set()
    assignment.getType match {
      case TNull              => varKnowledge += IsNull(true)
      case t if !t.isNullable => varKnowledge += IsNull(false)
      case _                  =>
    }

    varKnowledge
  }

}