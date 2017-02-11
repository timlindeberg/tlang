package tcompiler.analyzer

import tcompiler.analyzer.Symbols.{ClassSymbol, FieldSymbol, VariableSymbol}
import tcompiler.analyzer.Types._
import tcompiler.ast.Trees._
import tcompiler.utils.Extensions._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.reflect.{ClassTag, classTag}

/**
  * Created by Tim Lindeberg on 1/14/2017.
  */
object Knowledge {

  trait Identifier extends Typed {
    def symbol: Option[VariableSymbol]

    def baseIdentifierEquals(varId: Identifier): Boolean
  }

  case class VarIdentifier(varSymbol: VariableSymbol) extends Identifier {
    val symbol = Some(varSymbol)
    def baseIdentifierEquals(varId: Identifier): Boolean = varId match {
      case VarIdentifier(v) => varSymbol == v
      case _                => false
    }
    override val getType: Type = varSymbol.getType
    override def toString: String = varSymbol.name
  }

  case class ClassIdentifier(classSymbol: ClassSymbol) extends Identifier {
    val symbol = None
    def baseIdentifierEquals(varId: Identifier): Boolean = varId match {
      case ClassIdentifier(c) => classSymbol == c
      case _                  => false
    }
    override val getType: TObject = classSymbol.getType
    override def toString: String = classSymbol.name
  }

  case class AccessIdentifier(id: Identifier, varSymbol: VariableSymbol) extends Identifier {
    val symbol = Some(varSymbol)
    def baseIdentifierEquals(varId: Identifier): Boolean = {
      if (id == varId) true
      else id.baseIdentifierEquals(varId)
    }

    override val getType: Type = varSymbol.getType
    override def toString = s"$id.${varSymbol.name}"
  }

  case class ArrayItemIdentifier(id: Identifier, index: Int) extends Identifier {
    val symbol = None
    def baseIdentifierEquals(varId: Identifier): Boolean = {
      if (id == varId) true
      else id.baseIdentifierEquals(varId)
    }

    override val getType: Type = id.getType.asInstanceOf[TArray].tpe

    override def toString = s"$id[$index]"
  }


  trait VarKnowledge {
    def invert: VarKnowledge = this
  }

  trait VarKnowledgeWrapper extends VarKnowledge {
    def inner: VarKnowledge
  }

  // Used to wrap knowledge to show it came from an and or an or expression
  case class OrKnowledge(inner: VarKnowledge) extends VarKnowledgeWrapper {override def invert = AndKnowledge(inner.invert)}
  case class AndKnowledge(inner: VarKnowledge) extends VarKnowledgeWrapper {override def invert = OrKnowledge(inner.invert)}

  // These should be objects but then it doesnt work as type parameters
  class Initialized extends VarKnowledge {override def toString = "Initialized"}
  class Used extends VarKnowledge {override def toString = "Used"}
  val Initialized = new Initialized
  val Used        = new Used

  case class Reassigned(at: StatTree) extends VarKnowledge {override def toString = s"Reassigned(${at.line}:${at.col})"}
  case class IsNull(value: Boolean) extends VarKnowledge {override def invert = IsNull(!value)}
  case class ArraySize(size: Int) extends VarKnowledge
  case class NumericValue(value: Int) extends VarKnowledge
  case class BoolValue(condition: ExprTree) extends VarKnowledge

  // TODO: Ranges?
  case class Greater(value: Int) extends VarKnowledge {
    override def invert = LessEq(value)
  }
  case class GreaterEq(value: Int) extends VarKnowledge {
    override def invert = Less(value)
  }
  case class Less(value: Int) extends VarKnowledge {
    override def invert = GreaterEq(value)
  }
  case class LessEq(value: Int) extends VarKnowledge {
    override def invert = Greater(value)
  }

  case class Knowledge(varKnowledge: Map[Identifier, Set[VarKnowledge]] = Map(),
    flowEnded: Option[StatTree] = None) {

    def +(other: Knowledge): Knowledge = {
      val knowledge = mutable.Map[Identifier, Set[VarKnowledge]]()
      List(this, other) foreach {
        _.varKnowledge foreach { case (id, knowledge1) =>
          val v = knowledge.getOrElse(id, Set())
          knowledge += id -> (v ++ knowledge1)
        }
      }
      val flow = if (other.flowEnded.isDefined) other.flowEnded else flowEnded
      Knowledge(knowledge.toMap, flow)
    }

    def -(other: Knowledge): Knowledge = {
      val knowledge = mutable.Map[Identifier, Set[VarKnowledge]]()
      varKnowledge.foreach { case (id, knowledge1) =>
        other.varKnowledge.get(id) match {
          case Some(knowledge2) =>
            val diff = knowledge1.diff(knowledge2)
            if (diff.nonEmpty)
              knowledge += id -> diff
          case None             => knowledge += id -> knowledge1
        }
      }
      val flow = (flowEnded, other.flowEnded) match {
        case (Some(f1), Some(f2)) if f1 == f2 => None
        case _                                => flowEnded
      }
      Knowledge(knowledge.toMap, flow)
    }

    def intersection(other: Knowledge, newFlowEnded: Option[StatTree]): Knowledge = {
      val knowledge = mutable.Map[Identifier, Set[VarKnowledge]]()
      varKnowledge.foreach { case (id, knowledge1) =>
        other.varKnowledge.get(id) ifDefined { knowledge2 =>
          knowledge += id -> knowledge1.intersect(knowledge2)
        }
      }

      val flow = newFlowEnded match {
        case Some(f) => if (flowEnded.isDefined && other.flowEnded.isDefined) Some(f) else None
        case None    => None
      }
      Knowledge(knowledge.toMap, flow)
    }

    def add[T <: VarKnowledge : ClassTag](varId: Identifier, newKnowledge: T): Knowledge = {
      varId.symbol ifDefined { sym =>
        if (sym.isInstanceOf[FieldSymbol] && !sym.isFinal) {
          // cannot gain knowledge about var fields since they can change at any time
          return this
        }
      }
      val knowledge = varKnowledge.getOrElse(varId, Set()).filterNotInstance[T] + newKnowledge
      copy(varKnowledge = varKnowledge + (varId -> knowledge))
    }


    def get[T <: VarKnowledge : ClassTag](varId: Identifier): Option[T] = {
      val v = varKnowledge.getOrElse(varId, Set())
      v.findInstance[T] match {
        case Some(x) => Some(x)
        case None    => v.findInstance[AndKnowledge] flatMap {
          case AndKnowledge(x) =>
            if (classTag[T].runtimeClass.isInstance(x))
              Some(x.asInstanceOf[T])
            else
              None
          case _               => None
        }
      }
    }

    def invert: Knowledge = copy(varKnowledge = {
      val knowledge = mutable.Map[Identifier, Set[VarKnowledge]]()
      varKnowledge foreach { case (varId, k) =>
        knowledge += varId -> k.map(_.invert)
      }
      knowledge.toMap
    })

    def assignment(varId: Identifier, init: Option[ExprTree], extraInfo: VarKnowledge*): Knowledge = {
      val varTpe = varId.getType
      val initial = getInitialKnowledge(varId, varTpe, init)
      // Remove old knowledge of the given variable symbol
      val newKnowledge = filterOldKnowledge(varId) ++ initial
      val varIdKnowledge = newKnowledge(varId) ++ extraInfo.toSet
      copy(varKnowledge = newKnowledge + (varId -> varIdKnowledge))
    }

    def addToNumericValue(varId: Identifier, increment: Int): Knowledge = {
      get[NumericValue](varId) match {
        case Some(NumericValue(v)) => setNumericValue(varId, v + increment)
        case _                     => this
      }
    }

    def setNumericValue(varId: Identifier, value: Int): Knowledge = {
      val numericKnowledge = NumericValue(value)
      val oldKnowledge = varKnowledge.getOrElse(varId, Set()).filterNotInstance[NumericValue]
      val newKnowledge = oldKnowledge + numericKnowledge
      copy(varKnowledge = varKnowledge + (varId -> newKnowledge))
    }


    def endFlow(at: StatTree): Knowledge = copy(flowEnded = Some(at))

    def addOrKnowledge(from: Knowledge): Knowledge = _addWrapped(from, OrKnowledge)
    def addAndKnowledge(from: Knowledge): Knowledge = _addWrapped(from, AndKnowledge)

    def filterReassignedVariables(branch: StatTree, afterBranch: Knowledge): Knowledge = {
      val gainedKnowledge = afterBranch - this
      val newKnowledge = mutable.Map[Identifier, Set[VarKnowledge]]() ++ varKnowledge
      gainedKnowledge.varKnowledge foreach { case (varId, knowledge) =>
        knowledge.findInstance[Reassigned] match {
          case Some(Reassigned(_)) =>
            // Variable could have gotten reassigned in the branch so we have to
            // remove previous knowledge
            // The knowledge that can be saved from the branch is the intersection
            // of the knowledge after the branch and the original knowledge
            val inter = intersection(afterBranch, None)
            val varIdKnowledge = inter.varKnowledge.getOrElse(varId, Set())
            inter.varKnowledge foreach { case (vId, k) => newKnowledge += vId -> k }
            newKnowledge += varId -> (varIdKnowledge + Reassigned(branch))
          case None                =>
        }
      }
      copy(varKnowledge = newKnowledge.toMap)
    }

    def getIdentifier(obj: ExprTree): Option[Identifier] =
      obj match {
        case v: VariableID      => Some(VarIdentifier(v.getSymbol))
        case c: ClassID         => Some(ClassIdentifier(c.getSymbol))
        case Assign(to, _)      => getIdentifier(to)
        case access: Access     => getAccessIdentifier(access)
        case arrRead: ArrayRead => getArrayIdentifier(arrRead)
        case _                  => None
      }


    def getNumericValue(expr: ExprTree): Option[Int] = {
      expr match {
        case IntLit(value)                   => Some(value)
        case assignable: Assignable          => getIdentifier(assignable) flatMap { id => get[NumericValue](id).map(_.value) }
        case op@BinaryOperatorTree(lhs, rhs) =>
          getNumericValue(lhs) flatMap { lhsValue =>
            getNumericValue(rhs) map { rhsValue =>
              val operator = getBinaryOperator(op)
              operator(lhsValue, rhsValue)
            }
          }
        case op@UnaryOperatorTree(expr)      =>
          getNumericValue(expr) flatMap { exprValue =>
            getUnaryOperator(op) map { operator =>
              // we currently cannot infer knowledge when increment and decrement is used
              operator(exprValue)
            }
          }
        case _                               => None
      }

    }

    private def _addWrapped[T <: VarKnowledgeWrapper : ClassTag](from: Knowledge, cons: VarKnowledge => T) = {
      val newKnowledge = mutable.Map[Identifier, Set[VarKnowledge]]()
      from.varKnowledge.foreach { case (varId, vKnowledge) =>
        val v = varKnowledge.getOrElse(varId, Set())
        val wrapped = vKnowledge map {
          case k: VarKnowledgeWrapper => k
          case k if v.contains(k)     => k
          case k                      => cons(k)
        }
        newKnowledge += varId -> (v ++ wrapped)
      }
      copy(varKnowledge = varKnowledge ++ newKnowledge)
    }

    private def getAccessIdentifier(access: Access): Option[Identifier] = {
      access match {
        case Access(_, _: MethodCall)    => None
        case Access(obj, id: VariableID) => getIdentifier(obj) match {
          case Some(objId) => Some(AccessIdentifier(objId, id.getSymbol))
          case None        => None
        }
        case _                           => None
      }
    }

    private def getArrayIdentifier(arrRead: ArrayRead): Option[Identifier] = {
      val arr = arrRead.arr
      if (!arr.getType.isInstanceOf[TArray])
        return None

      getIdentifier(arr) flatMap { id =>
        val index = arrRead.index
        getNumericValue(index) flatMap { value =>
          Some(ArrayItemIdentifier(id, value))
        }
      }
    }

    private def getInitialKnowledge(varId: Identifier, varTpe: Type, maybeInit: Option[ExprTree]): List[(Identifier, Set[VarKnowledge])] = {
      if (maybeInit.isEmpty) {
        val s: Set[VarKnowledge] = if (varTpe in Primitives) Set(Initialized) else Set()
        return List(varId -> s)
      }
      val init = maybeInit.get
      val knowledge = ListBuffer[(Identifier, Set[VarKnowledge])]()
      var varIdKnowledge = Set[VarKnowledge](Initialized)

      // Old knowledge gets transferred to new var
      getIdentifier(init) ifDefined { initId =>
        if (varKnowledge.contains(initId)) {
          varKnowledge.getOrElse(initId, Set()) foreach { k => varIdKnowledge += k }
          knowledge += varId -> varIdKnowledge
          return knowledge.toList
        }
      }

      if (varTpe.isNullable) {
        init.getType match {
          case TNull              => varIdKnowledge += IsNull(true)
          case t if !t.isNullable => varIdKnowledge += IsNull(false)
          case _                  =>
        }
      }

      varTpe match {
        case TArray(arrTpe)      =>
          init match {
            case ArrayLit(value)        =>
              value.zipWithIndex.foreach { case (init, index) =>
                val arrayVarId = ArrayItemIdentifier(varId, index)
                knowledge ++= getInitialKnowledge(arrayVarId, arrTpe, Some(init))
              }
              varIdKnowledge += ArraySize(value.size)
            case NewArray(_, size :: _) =>
              getNumericValue(size) ifDefined { value => varIdKnowledge += ArraySize(value) }
            case _                      =>
          }
        case _ if varTpe == Int  => getNumericValue(init) ifDefined { value => varIdKnowledge += NumericValue(value) }
        case _ if varTpe == Bool => varIdKnowledge += BoolValue(init)
        case _                   =>
      }

      knowledge += varId -> varIdKnowledge
      knowledge.toList
    }

    private def filterOldKnowledge(varId: Identifier) =
      varKnowledge.filter { case (id, _) => !id.baseIdentifierEquals(varId) }

    private def getUnaryOperator(unaryOperatorTree: UnaryOperatorTree): Option[Int => Int] = unaryOperatorTree match {
      case _: Hash                   => Some(_.hashCode)
      case _: Negation               => Some(-_)
      case _: LogicNot               => Some(~_)
      case _: IncrementDecrementTree => None
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

    override def toString: String = {
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

}
