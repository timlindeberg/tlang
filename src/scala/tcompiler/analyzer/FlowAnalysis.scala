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
                       events: Map[Int, Set[Event]] = Map(),
                       flowEnded: Option[StatTree] = None) {

    def addLocalVar(v: VariableSymbol, knowledge: Set[VarKnowledge], fields: List[VariableSymbol] = Nil) = {
      val vId = VarIdentifier(v, fields)
      copy(varKnowledge = varKnowledge + (vId -> knowledge))
    }

    def isNull(v: VariableSymbol, fields: List[VariableSymbol] = Nil): Option[Boolean] = isNull(VarIdentifier(v, fields))
    def isNull(v: VarIdentifier): Option[Boolean] =
      varKnowledge.getOrElse(v, Set()).find(_.isInstanceOf[IsNull]) match {
        case Some(IsNull(value)) => Some(value)
        case _                   => None
      }


    def nullCheck(t: Tree, varId: VarIdentifier, isNull: Boolean) = {
      val k = addEvent(t, NullCheck(varId))
      val info = varKnowledge.getOrElse(varId, Set()).filter(!_.isInstanceOf[IsNull]) + IsNull(isNull)
      k.copy(varKnowledge = varKnowledge + (varId -> info))
    }

    def used(name: String) = {

    }

    def endFlow(at: StatTree) = copy(flowEnded = Some(at))

    def eventsFrom(t: Tree): Set[Event] = events.getOrElse(System.identityHashCode(t), Set())

    def invertKnowledgeFrom(t: Tree): Knowledge = {
      eventsFrom(t).filterType[NullCheck].foldLeft(this)((knowledge, event) => event match {
        case NullCheck(vars) =>
          val isNull = varKnowledge(vars).findInstance[IsNull]
          val v = varKnowledge(vars).filter(!_.isInstanceOf[IsNull]) + IsNull(!isNull.value)
          val tup = (vars, v)
          knowledge.copy(varKnowledge = varKnowledge + tup)
        case _               => this
      })
    }

    def bindEvents(topEvent: Tree, others: Tree*): Knowledge = {
      val hash = System.identityHashCode(topEvent)
      val topSet = others.flatMap(eventsFrom).toSet
      copy(events = events + (hash -> topSet))
    }

    private def addEvent(t: Tree, event: Event) = {
      val hash = System.identityHashCode(t)
      val e = events.getOrElse(hash, Set())
      copy(events = events + (hash -> (e + event)))
    }

    override def toString = {
      varKnowledge.map { case (VarIdentifier(v, fields), knowledge) =>
        val f = if (fields.isEmpty) "" else fields.mkString(".", ".", "")
        s"$v$f -> { ${knowledge.mkString(", ")} }"
      }.mkString("\n") +
        (flowEnded match {
          case Some(ended) => s"\nFlow ended at ${ended.line}: $ended"
          case None        => ""
        })
    }
  }

  trait Event
  case class NullCheck(v: VarIdentifier) extends Event
  case class TypeCheck(v: VariableSymbol, tpe: Type) extends Event

  trait VarKnowledge

  case object Initialized extends VarKnowledge
  case object Used extends VarKnowledge
  case object Reassigned extends VarKnowledge
  case class IsNull(value: Boolean) extends VarKnowledge

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
        val afterCondition = analyzeExpr(condition, afterInit)
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
        val afterCondition = analyzeExpr(condition, knowledge)
        val thnKnowledge = analyze(thn, afterCondition)
        els ifDefined {analyze(_, afterCondition.invertKnowledgeFrom(condition))}
        if (thnKnowledge.flowEnded.isDefined)
          thnKnowledge.invertKnowledgeFrom(condition)
        else
          knowledge
      case While(condition, stat)            =>
        val afterCondition = analyze(condition, knowledge)
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

  def analyzeExpr(tree: ExprTree, knowledge: Knowledge): Knowledge = {
    tree match {
      case And(lhs, rhs)                        =>
        val lhsKnowledge = analyzeExpr(lhs, knowledge)
        val rhsKnowledge = analyzeExpr(rhs, lhsKnowledge)
        rhsKnowledge.bindEvents(tree, lhs, rhs)
      case Or(lhs, rhs)                         =>
        // We cannot learn things from or statements unless by Demorgans law
        analyzeExpr(lhs, knowledge)
        analyzeExpr(rhs, knowledge)
        knowledge
      case Not(expr)                            =>
        val exprKnowledge = analyzeExpr(expr, knowledge)
        exprKnowledge.invertKnowledgeFrom(expr)
      case Ternary(condition, thn, els)         =>
        val afterCondition = analyzeExpr(condition, knowledge)
        analyzeExpr(thn, afterCondition)
        analyzeExpr(els, afterCondition.invertKnowledgeFrom(condition))
      case acc@Access(obj, app)                 =>
        analyzeExpr(obj, knowledge)
        analyzeExpr(app, knowledge)

        checkNull(obj, knowledge)
        knowledge
      case Assign(v@VariableID(name), exprTree) =>
        // Reset knowledge
        val varKnowledge = varIsNull(exprTree) + Initialized + Reassigned
        knowledge.addLocalVar(v.getSymbol, varKnowledge)
      case eq@EqualsOperatorTree(lhs, rhs)      =>
        val isNull = eq.isInstanceOf[Equals]
        analyzeExpr(lhs, knowledge)
        analyzeExpr(rhs, knowledge)
        if (lhs == NullLit())
          nullCheck(tree, knowledge, rhs, isNull)
        else if (rhs == NullLit())
          nullCheck(tree, knowledge, lhs, isNull)
        else
          knowledge
      case BinaryOperatorTree(lhs, rhs)         =>
        analyzeExpr(lhs, knowledge)
        analyzeExpr(rhs, knowledge)
        checkNull(lhs, knowledge)
        checkNull(rhs, knowledge)
        knowledge
      case UnaryOperatorTree(expr)              =>
        analyzeExpr(expr, knowledge)
        checkNull(expr, knowledge)
        knowledge
      case ArrayOperatorTree(arr)               =>
        analyzeExpr(arr, knowledge)
        checkNull(arr, knowledge)
        knowledge
      case t                                    =>
        val traverser = new TreeTraverser {
          override def traverse(t: Tree) = {
            analyzeExpr(t.asInstanceOf[ExprTree], knowledge)
            super.traverse(t)
          }
        }
        t.children.foreach(traverser.traverse)
        knowledge
    }
  }

  private def checkNull(obj: ExprTree, knowledge: Knowledge): Unit = {
    if (!obj.getType.isNullable)
      return

    obj match {
      case NormalAccess(_, MethodCall(meth, _)) => ErrorAccessNullableMethod(meth.getSymbol.signature, obj)
      case _                                    =>
        getVarIdentifier(obj) ifDefined { varId =>
          knowledge.isNull(varId) match {
            case Some(isNull) if isNull => ErrorAccessIsNull(varId.value, obj)
            case None                   => ErrorAccessMightBeNull(varId.value, obj)
            case _                      =>
          }
        }
    }
  }

  private def conjunctions(expr: ExprTree): List[ExprTree] = {
    expr match {
      case Or(lhs, rhs) => conjunctions(lhs) ::: conjunctions(rhs)
      case expr         => List(expr)
    }

  }

  private def getVarIdentifier(obj: ExprTree): Option[VarIdentifier] = {
    obj match {
      case v: VariableID  => Some(VarIdentifier(v.getSymbol, Nil))
      case access: Access =>
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