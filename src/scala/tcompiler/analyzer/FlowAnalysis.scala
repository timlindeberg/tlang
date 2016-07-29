package tcompiler.analyzer

import com.sun.org.apache.xpath.internal.operations.Mod
import tcompiler.analyzer.Symbols.{ClassSymbol, VariableSymbol}
import tcompiler.analyzer.Types._
import tcompiler.ast.TreeTraverser
import tcompiler.ast.Trees._
import tcompiler.imports.ImportMap
import tcompiler.utils.{Context, Pipeline}
import tcompiler.utils.Extensions._

import scala.collection.mutable.ListBuffer
import scala.collection.mutable

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

  trait Identifier extends Typed {
    def baseIdentiferEquals(varId: Identifier): Boolean
  }

  case class VarIdentifier(varSymbol: VariableSymbol) extends Identifier {
    def baseIdentiferEquals(varId: Identifier) = varId match {
      case VarIdentifier(v) => varSymbol == v
      case _                => false
    }
    override val getType = varSymbol.getType
    override def equals(any: Any) = any match {
      case v: VarIdentifier => varSymbol == v.varSymbol
      case _                => false
    }

    override def hashCode = varSymbol.hashCode
    override def toString = varSymbol.name
  }

  case class ClassIdentifier(classSymbol: ClassSymbol) extends Identifier {
    def baseIdentiferEquals(varId: Identifier) = varId match {
      case ClassIdentifier(c) => classSymbol == c
      case _                  => false
    }
    override val getType = classSymbol.getType
    override def equals(any: Any) = any match {
      case v: ClassIdentifier => classSymbol == v.classSymbol
      case _                  => false
    }

    override def hashCode = classSymbol.hashCode
    override def toString = classSymbol.name
  }

  case class AccessIdentifier(id: Identifier, varSymbol: VariableSymbol) extends Identifier {
    def baseIdentiferEquals(varId: Identifier): Boolean =
      if (id == varId)
        true
      else
        id.baseIdentiferEquals(varId)
    override val getType = varSymbol.getType
    override def equals(any: Any) = any match {
      case v: AccessIdentifier => id == v.id && varSymbol == v.varSymbol
      case _                   => false
    }

    override def hashCode = id.hashCode ^ varSymbol.hashCode
    override def toString = s"$id.${varSymbol.name}"
  }

  case class ArrayItemIdentifier(id: Identifier, index: Int) extends Identifier {
    def baseIdentiferEquals(varId: Identifier) =
      if (id == varId)
        true
      else
        id.baseIdentiferEquals(varId)
    override val getType = id.getType.asInstanceOf[TArray].tpe

    override def equals(any: Any) = any match {
      case v: ArrayItemIdentifier => id == v.id && index == v.index
      case _                      => false
    }

    override def hashCode = id.hashCode ^ index.hashCode
    override def toString = s"$id[$index]"
  }

  case class Knowledge(varKnowledge: Map[Identifier, Set[VarKnowledge]] = Map(),
                       events: Map[Int, Set[(Identifier, VarKnowledge)]] = Map(),
                       flowEnded: Option[StatTree] = None) {

    def assignment(varId: Identifier, init: Option[ExprTree], extraInfo: VarKnowledge*) = {
      val varTpe = varId.getType
      val knowledge = getKnowledge(varId, varTpe, init)
      // Remove old knowledge of the given variable symbol
      val filteredKnowledge = filterOldKnowledge(varId)
      val extra = varId -> extraInfo.toSet
      copy(varKnowledge = filteredKnowledge ++ (extra :: knowledge))
    }

    def addToNumericValue(varId: Identifier, increment: Int) = {
      numericValue(varId) match {
        case Some(v) => setNumericValue(varId, v + increment)
        case _       => this
      }
    }

    def setNumericValue(varId: Identifier, value: Int) = {
      val numericKnowledge = NumericValue(value)
      val oldKnowledge = varKnowledge.getOrElse(varId, Set()).filterNotType[NumericValue]
      val newKnowledge = oldKnowledge + numericKnowledge
      copy(varKnowledge = varKnowledge + (varId -> newKnowledge))
    }

    private def filterOldKnowledge(varId: Identifier) =
      varKnowledge.filter { case (id, _) => !id.baseIdentiferEquals(varId) }

    def isNull(v: Identifier): Option[Boolean] =
      varKnowledge.getOrElse(v, Set()).findInstance[IsNull] match {
        case Some(IsNull(value)) => Some(value)
        case _                   => None
      }

    def numericValue(v: Identifier): Option[Int] =
      varKnowledge.getOrElse(v, Set()).findInstance[NumericValue] match {
        case Some(NumericValue(value)) => Some(value)
        case _                         => None
      }

    def nullCheck(t: Tree, varId: Identifier, isNull: Boolean) = {
      val nullKnowledge = IsNull(isNull)
      val event = getEvent(t, varId, nullKnowledge)
      val oldKnowledge = varKnowledge.getOrElse(varId, Set()).filterNotType[IsNull]
      val newKnowledge = oldKnowledge + nullKnowledge
      copy(varKnowledge = varKnowledge + (varId -> newKnowledge), events = events + event)
    }

    def endFlow(at: StatTree) = copy(flowEnded = Some(at))

    def eventsFrom(t: Tree): Set[(Identifier, VarKnowledge)] = events.getOrElse(System.identityHashCode(t), Set())

    def invertKnowledgeFrom(t: Tree): Knowledge = {
      eventsFrom(t).foldLeft(this)((knowledge, event) => event match {
        case (varId, vKnowledge) =>
          knowledge.copy(varKnowledge = varKnowledge + (varId -> invertKnowledge(varId, vKnowledge)))
      })
    }


    def invertKnowledge(varId: Identifier, vKnowledge: VarKnowledge): Set[VarKnowledge] = {
      varKnowledge(varId) - vKnowledge + vKnowledge.invert
    }

    /**
      * Maps the events of lhs and rhs to and.
      */
    def and(and: Tree, lhs: Tree, rhs: Tree): Knowledge = {
      val topSet = eventsFrom(lhs) ++ eventsFrom(rhs)
      copy(events = events + (hash(and) -> topSet))
    }

    /**
      * Maps the inverted events of expr to not.
      */
    def not(not: Tree, expr: Tree): Knowledge = {
      val h = hash(not)
      val topSet = eventsFrom(expr)
      val newKnowledge = topSet.map { case (varId, k) => varId -> invertKnowledge(varId, k) }.toMap
      val newEventSets: Set[(Identifier, VarKnowledge)] = topSet.map { case (varId, k) => varId -> k.invert }
      val newEvents = events + (h -> newEventSets)
      copy(varKnowledge = varKnowledge ++ newKnowledge, events = newEvents)
    }

    /**
      * Maps the events in topSet wrapped in OrChecks to or.
      */
    def or(or: Tree, topSet: Set[(Identifier, VarKnowledge)]): Knowledge = {
      // Wrap knowledge in OrChecks
      val newKnowledge = mutable.Map[Identifier, Set[VarKnowledge]]()
      var newEventSets = Set[(Identifier, VarKnowledge)]()

      topSet.foreach { case (varId, k) =>
        val v = varKnowledge.getOrElse(varId, Set())
        val e = if (v.contains(k)) k else OrCheck(k)
        newKnowledge += varId -> (v + e)
        newEventSets += varId -> e
      }
      val newEvents = events + (hash(or) -> newEventSets)
      copy(varKnowledge = varKnowledge ++ newKnowledge, events = newEvents)
    }


    private def getKnowledge(varId: Identifier, varTpe: Type, maybeInit: Option[ExprTree]): List[(Identifier, Set[VarKnowledge])] = {
      if (maybeInit.isEmpty) {
        val s: Set[VarKnowledge] = if (varTpe.isInstanceOf[PrimitiveType]) Set(Initialized) else Set()
        return List(varId -> s)
      }
      val init = maybeInit.get
      val knowledge = ListBuffer[(Identifier, Set[VarKnowledge])]()
      var varIdKnowledge = Set[VarKnowledge](Initialized)
      varIdKnowledge ++= nullKnowledge(varTpe, init)

      varTpe match {
        case TArray(arrTpe) =>
          init match {
            case ArrayLit(value)                     =>
              value.zipWithIndex.foreach { case (init, index) =>
                val arrayVarId = new ArrayItemIdentifier(varId, index)
                knowledge ++= getKnowledge(arrayVarId, arrTpe, Some(init))
              }
              varIdKnowledge += ArraySize(value.size)
            case NewArray(tpe, size :: rest) =>
              getNumericValue(size, this) ifDefined  { value => varIdKnowledge += ArraySize(value)}
            case _                                   =>
          }
        case _: TInt        =>
          getNumericValue(init, this) ifDefined { value => varIdKnowledge += NumericValue(value) }
        case _              =>
      }

      knowledge += varId -> varIdKnowledge
      knowledge.toList
    }

    private def nullKnowledge(varTpe: Type, init: ExprTree): Set[VarKnowledge] = {
      if (!varTpe.isNullable)
        return Set()

      init.getType match {
        case TNull              => Set(IsNull(true))
        case t if !t.isNullable => Set(IsNull(false))
        case _                  => Set()
      }
    }

    private def getEvent(t: Tree, varId: Identifier, newKnowledge: VarKnowledge) = {
      val h = hash(t)
      val e = events.getOrElse(h, Set())
      h -> (e + (varId -> newKnowledge))
    }

    private def hash(t: Tree) = System.identityHashCode(t)

    override def toString = {
      val vars = varKnowledge.map { case (expr, knowledge) =>
        s"$expr -> { ${knowledge.mkString(", ")} }"
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
  case class ArraySize(size: Int) extends VarKnowledge
  case class NumericValue(value: Int) extends VarKnowledge
  case class BoolValue(check: ExprTree) extends VarKnowledge

  def apply(clazz: ClassDecl): Unit = {

    def getVarKnowledgeMap(list: List[VariableSymbol]): Map[Identifier, Set[VarKnowledge]] =
      list.map(v => new VarIdentifier(v) -> Set[VarKnowledge](Initialized)).toMap

    val fieldKnowledge = getVarKnowledgeMap(clazz.fields.map(_.id.getSymbol))
    clazz.methods.filter(_.stat.isDefined) foreach { meth =>
      val argKnowledge = getVarKnowledgeMap(meth.getSymbol.argList)
      val knowledge = Knowledge(varKnowledge = fieldKnowledge ++ argKnowledge)
      analyze(meth.stat.get, knowledge)
    }
  }

  def analyze(tree: StatTree, knowledge: Knowledge): Knowledge = {
    println(s"${tree.line}: $tree")
    println(knowledge)
    println("----------------------------------------")
    tree match {
      case Block(stats)                      =>
        val endKnowledge = stats.foldLeft(knowledge)((currentKnowledge, next) => analyze(next, currentKnowledge))
        endKnowledge.flowEnded match {
          case Some(stat) if stat != stats.last => WarningDeadCode(stat.line, stats.last.line, stat)
          case _                                =>
        }
        endKnowledge
      case varDecl@VarDecl(_, id, init, _)   =>
        val varId = new VarIdentifier(id.getSymbol)
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

        post.foreach(analyze(_, afterStat))
        knowledge
      case Foreach(varDecl, container, stat) =>
        analyzeExpr(container, knowledge)
        checkIfIsNull(container, knowledge)
        val newKnowledge = analyze(varDecl, knowledge)
        analyze(stat, newKnowledge)
        knowledge
      case If(condition, thn, els)           =>
        val afterCondition = analyzeCondition(condition, knowledge)
        val thnKnowledge = analyze(thn, afterCondition)
        els ifDefined {analyze(_, afterCondition.invertKnowledgeFrom(condition))}
        if (thnKnowledge.flowEnded.isDefined) {
          val a = afterCondition.invertKnowledgeFrom(condition)
          a
        }
        else
          knowledge
      case While(condition, stat)            =>
        val afterCondition = analyzeCondition(condition, knowledge)
        analyze(stat, afterCondition)
        knowledge
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
      rhsKnowledge.and(tree, lhs, rhs)
    case Or(lhs, rhs)                 =>
      val lhsKnowledge = analyzeCondition(lhs, knowledge)
      val rhsKnowledge = analyzeCondition(rhs, lhsKnowledge)
      val orKnowledge = lhsKnowledge.eventsFrom(lhs) ++ rhsKnowledge.eventsFrom(rhs)
      knowledge.or(tree, orKnowledge)
    case Not(expr)                    =>
      // Apply De Morgans law:
      expr match {
        case And(lhs, rhs) =>
          analyzeCondition(Or(Not(lhs), Not(rhs)), knowledge)
        case Or(lhs, rhs)  =>
          analyzeCondition(And(Not(lhs), Not(rhs)), knowledge)
        case _             =>
          val exprKnowledge = analyzeCondition(expr, knowledge)
          exprKnowledge.not(tree, expr)
      }
    case assignable: Assignable       =>
      val afterAssignable = analyzeExpr(assignable, knowledge)
      nullCheck(tree, assignable, afterAssignable, isNull = false)
    case EqualsOperatorTree(lhs, rhs) =>
      val isNull = tree.isInstanceOf[Equals]
      val lhsKnowledge = analyzeExpr(lhs, knowledge)
      val rhsKnowledge = analyzeExpr(rhs, lhsKnowledge)
      if (lhs == NullLit())
        nullCheck(tree, rhs, rhsKnowledge, isNull)
      else if (rhs == NullLit())
        nullCheck(tree, lhs, rhsKnowledge, isNull)
      else
        rhsKnowledge
    case _                            =>
      analyzeExpr(tree, knowledge)
  }

  def analyzeExpr(tree: ExprTree, topKnowledge: Knowledge): Knowledge = {
    var knowledge = topKnowledge
    val traverser = new TreeTraverser {
      override def traverse(t: Tree) = t match {
        case Ternary(condition, thn, els)        =>
          val afterCondition = analyzeCondition(condition, knowledge)
          analyzeExpr(thn, afterCondition)
          analyzeExpr(els, afterCondition.invertKnowledgeFrom(condition))
        case acc@Access(obj, app)                =>
          traverse(obj, app)
          checkIfIsNull(obj, knowledge)
        case Assign(obj, from)                   =>
          super.traverse(tree)
          getIdentifier(obj, knowledge) ifDefined { varId =>
            // Reset knowledge
            knowledge = knowledge.assignment(varId, Some(from), Reassigned)
          }
        case eq@EqualsOperatorTree(lhs, rhs)     =>
          traverse(lhs, rhs)
        case BinaryOperatorTree(lhs, rhs)        =>
          // Rest of the binary operators need to be null checked before use
          traverse(lhs, rhs)
          checkIfIsNull(lhs, knowledge)
          checkIfIsNull(rhs, knowledge)
        case incDec@IncrementDecrementTree(expr) =>
          traverse(expr)
          checkIfIsNull(expr, knowledge)
          getIdentifier(expr, knowledge) ifDefined { varId =>
            val v = if (incDec.isIncrement) 1 else -1
            knowledge = knowledge.addToNumericValue(varId, v)
          }
        case UnaryOperatorTree(expr)             =>
          traverse(expr)
          checkIfIsNull(expr, knowledge)
        case ArrayOperatorTree(arr)              =>
          traverse(arr)
          checkIfIsNull(arr, knowledge)
        case _                                   =>
          super.traverse(t)
      }
    }
    traverser.traverse(tree)
    knowledge
  }

  private def getNumericValue(expr: ExprTree, knowledge: Knowledge): Option[Int] = {
    expr match {
      case IntLit(value)                   => Some(value)
      case assignable: Assignable          => getIdentifier(assignable, knowledge) flatMap { id => knowledge.numericValue(id) }
      case op@BinaryOperatorTree(lhs, rhs) =>
        getNumericValue(lhs, knowledge) flatMap { lhsValue =>
          getNumericValue(rhs, knowledge) flatMap { rhsValue =>
            val operator = getBinaryOperator(op)
            Some(operator(lhsValue, rhsValue))
          }
        }
      case op@UnaryOperatorTree(expr)      =>
        getNumericValue(expr, knowledge) flatMap { exprValue =>
          val operator = getUnaryOperator(op)
          Some(operator(exprValue))
        }
      case _                               => None
    }

  }

  private def getUnaryOperator(unaryOperatorTree: UnaryOperatorTree): Int => Int = unaryOperatorTree match {
    case _: Hash                     => _.hashCode
    case _: Negation                 => -_
    case _: LogicNot                 => ~_
    case inc: IncrementDecrementTree => if (inc.isIncrement) _ + 1 else _ - 1
  }

  private def getBinaryOperator(binaryOperatorTree: BinaryOperatorTree): (Int, Int) => Int = binaryOperatorTree match {
    case _: Plus       => _ + _
    case _: Minus      => _ - _
    case _: Times      => _ * _
    case _: Div        => _ / _
    case _: Modulo     => _ % _
    case _: LogicAnd   => _ & _
    case _: LogicOr    => _ | _
    case _: LogicXor   => _ ^ _
    case _: LeftShift  => _ << _
    case _: RightShift => _ >> _
  }

  private def nullCheck(t: Tree, obj: ExprTree, knowledge: Knowledge, isNull: Boolean) = {
    getIdentifier(obj, knowledge) match {
      case Some(varId) => knowledge.nullCheck(t, varId, isNull)
      case None        => knowledge
    }
  }

  private def getIdentifier(obj: ExprTree, knowledge: Knowledge): Option[Identifier] =
    obj match {
      case v: VariableID      => Some(new VarIdentifier(v.getSymbol))
      case c: ClassID         => Some(new ClassIdentifier(c.getSymbol))
      case Assign(to, from)   => getIdentifier(to, knowledge)
      case access: Access     => getAccessIdentifier(access, knowledge)
      case arrRead: ArrayRead => getArrayIdentifier(arrRead, knowledge)
      case _                  => None
    }

  private def getAccessIdentifier(access: Access, knowledge: Knowledge): Option[Identifier] = {
    access match {
      case Access(obj, _: MethodCall)  => None
      case Access(obj, id: VariableID) => getIdentifier(obj, knowledge) match {
        case Some(objId) => Some(new AccessIdentifier(objId, id.getSymbol))
        case None        => None
      }
      case _                           => None
    }
  }

  private def getArrayIdentifier(arrRead: ArrayRead, knowledge: Knowledge): Option[Identifier] = {
    val arr = arrRead.arr
    if (!arr.getType.isInstanceOf[TArray])
      return None

    getIdentifier(arr, knowledge) flatMap { id =>
      val index = arrRead.index
      getNumericValue(index, knowledge) flatMap { value =>
        Some(new ArrayItemIdentifier(id, value))
      }
    }
  }

  private def checkIfIsNull(obj: ExprTree, knowledge: Knowledge): Unit = {
    if (!obj.getType.isNullable)
      return

    obj match {
      case NormalAccess(_, MethodCall(meth, _)) if meth.getType.isNullable =>
        ErrorAccessNullableMethod(meth.getSymbol.signature, obj)
      case _                                                               =>
        getIdentifier(obj, knowledge) match {
          case Some(varId) =>
            knowledge.isNull(varId) match {
              case Some(isNull) if isNull => ErrorAccessIsNull(obj, obj)
              case None                   => ErrorAccessMightBeNull(obj, obj)
              case _                      =>
            }
          case _           => ???
        }
    }
  }

}