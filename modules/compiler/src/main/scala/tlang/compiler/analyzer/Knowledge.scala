package tlang
package compiler
package analyzer

import tlang.compiler.analyzer.Symbols.{ClassSymbol, FieldSymbol, VariableSymbol}
import tlang.compiler.analyzer.Types._
import tlang.compiler.ast.Trees._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.reflect.{ClassTag, classTag}

object Knowledge {

  trait Identifier extends Typed {
    def symbol: Option[VariableSymbol]
    def baseIdentifierEquals(varId: Identifier): Boolean
  }

  case class VarIdentifier(varSymbol: VariableSymbol) extends Identifier {
    override val symbol: Option[VariableSymbol] = Some(varSymbol)
    override def baseIdentifierEquals(varId: Identifier): Boolean = varId match {
      case VarIdentifier(v) => varSymbol == v
      case _                => false
    }
    override val getType: Type = varSymbol.getType
    override def toString: String = varSymbol.name
  }

  case class ClassIdentifier(classSymbol: ClassSymbol) extends Identifier {
    override val symbol: Option[VariableSymbol] = None
    override def baseIdentifierEquals(varId: Identifier): Boolean = varId match {
      case ClassIdentifier(c) => classSymbol == c
      case _                  => false
    }
    override val getType: TObject = classSymbol.getType
    override def toString: String = classSymbol.name
  }

  case class AccessIdentifier(id: Identifier, varSymbol: VariableSymbol) extends Identifier {
    override val symbol: Option[VariableSymbol] = Some(varSymbol)
    override def baseIdentifierEquals(varId: Identifier): Boolean = {
      if (id == varId) true
      else id.baseIdentifierEquals(varId)
    }

    override val getType: Type = varSymbol.getType
    override def toString = s"$id.${ varSymbol.name }"
  }

  case class ArrayItemIdentifier(id: Identifier, index: Long) extends Identifier {
    override val symbol: Option[VariableSymbol] = None
    override def baseIdentifierEquals(varId: Identifier): Boolean = {
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
  case class OrKnowledge(inner: VarKnowledge) extends VarKnowledgeWrapper {
    override def invert = AndKnowledge(inner.invert)
  }
  case class AndKnowledge(inner: VarKnowledge) extends VarKnowledgeWrapper {
    override def invert = OrKnowledge(inner.invert)
  }

  case class MaybeKnowledge(inner: VarKnowledge) extends VarKnowledgeWrapper

  // These should be objects but then it doesnt work as type parameters
  class Initialized extends VarKnowledge {override def toString = "Initialized" }
  class Used extends VarKnowledge {override def toString = "Used" }
  val Initialized = new Initialized
  val Used = new Used

  case class Reassigned(at: StatTree) extends VarKnowledge {override def toString = s"Reassigned(${ at.line }:${ at.col })" }
  case class IsNull(value: Boolean) extends VarKnowledge {override def invert = IsNull(!value) }
  case class ArraySize(size: Long) extends VarKnowledge
  case class NumericValue(value: Long) extends VarKnowledge
  case class BoolValue(condition: ExprTree) extends VarKnowledge

  // TODO: Ranges?
  case class Greater(value: Long) extends VarKnowledge {
    override def invert = LessEq(value)
  }
  case class GreaterEq(value: Long) extends VarKnowledge {
    override def invert = Less(value)
  }
  case class Less(value: Long) extends VarKnowledge {
    override def invert = GreaterEq(value)
  }
  case class LessEq(value: Long) extends VarKnowledge {
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
      val ids = varKnowledge.keys ++ other.varKnowledge.keys
      val knowledge = ids
        .map { id =>
          val k1 = varKnowledge.getOrElse(id, Set())
          val k2 = other.varKnowledge.getOrElse(id, Set())
          val idKnowledge =
            k1.intersect(k2) ++
              k1.diff(k2).map { toMaybeKnowledge } ++
              k2.diff(k1).map { toMaybeKnowledge }
          id -> idKnowledge
        }
        .toMap

      val flow = newFlowEnded match {
        case Some(f) => if (flowEnded.isDefined && other.flowEnded.isDefined) Some(f) else None
        case None    => None
      }
      Knowledge(knowledge, flow)
    }

    def add[T <: VarKnowledge : ClassTag](varId: Identifier, newKnowledge: T): Knowledge = {
      varId.symbol ifDefined { sym =>
        if (sym.isInstanceOf[FieldSymbol] && !sym.isFinal) {
          // Cannot gain knowledge about var fields since they can change at any time.
          // For example, they could be changed in a different thread between two instructions
          return this
        }
      }
      val knowledge = varKnowledge.getOrElse(varId, Set()).filterNotInstance[T] + newKnowledge
      copy(varKnowledge = varKnowledge + (varId -> knowledge))
    }

    def getMaybe[T <: VarKnowledge : ClassTag](varId: Identifier): Option[T] = {
      val knowledge = varKnowledge.getOrElse(varId, Set())
        .filterInstance[MaybeKnowledge]
        .map { _.inner }
      get[T](knowledge)
        .orElse(get[T](varId))
    }

    def get[T <: VarKnowledge : ClassTag](varId: Identifier): Option[T] = {
      val knowledge = varKnowledge.getOrElse(varId, Set())
      get[T](knowledge)
    }

    def get[T <: VarKnowledge : ClassTag](knowledge: Set[VarKnowledge]): Option[T] = {
      knowledge.findInstance[T] match {
        case Some(x) => Some(x)
        case None    => knowledge.findInstance[AndKnowledge] flatMap {
          case AndKnowledge(x) =>
            if (classTag[T].runtimeClass.isInstance(x))
              Some(x.asInstanceOf[T])
            else
              None
          case _               => None
        }
      }
    }

    def is[T <: VarKnowledge : ClassTag](varId: Identifier): Boolean = get[T](varId).nonEmpty
    def isMaybe[T <: VarKnowledge : ClassTag](varId: Identifier): Boolean = getMaybe[T](varId).nonEmpty

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

    def addToNumericValue(varId: Identifier, increment: Long): Knowledge = {
      get[NumericValue](varId) match {
        case Some(NumericValue(v)) => setNumericValue(varId, v + increment)
        case _                     => this
      }
    }

    def setNumericValue(varId: Identifier, value: Long): Knowledge = {
      val numericKnowledge = NumericValue(value)
      val oldKnowledge = varKnowledge.getOrElse(varId, Set()).filterNotInstance[NumericValue]
      val newKnowledge = oldKnowledge + numericKnowledge
      copy(varKnowledge = varKnowledge + (varId -> newKnowledge))
    }

    def endFlow(at: StatTree): Knowledge = copy(flowEnded = Some(at))

    def addOrKnowledge(from: Knowledge): Knowledge = addWrapped(from, OrKnowledge)
    def addAndKnowledge(from: Knowledge): Knowledge = addWrapped(from, AndKnowledge)

    def filterReassignedVariables(branch: StatTree, afterBranch: Knowledge): Knowledge = {
      val gainedKnowledge = afterBranch - this
      val newKnowledge = mutable.Map[Identifier, Set[VarKnowledge]]() ++ varKnowledge
      gainedKnowledge.varKnowledge foreach { case (varId, knowledge) =>
        knowledge.findInstance[Reassigned] ifDefined { _ =>
          // Variable could have gotten reassigned in the branch so we have to
          // remove previous knowledge
          // The knowledge that can be saved from the branch is the intersection
          // of the knowledge after the branch and the original knowledge
          val inter = intersection(afterBranch, newFlowEnded = None)
          val varIdKnowledge = inter.varKnowledge.getOrElse(varId, Set())
          inter.varKnowledge foreach { case (vId, k) => newKnowledge += vId -> k }
          newKnowledge += varId -> (varIdKnowledge + Reassigned(branch))
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

    def getNumericValue(expr: ExprTree): Option[Long] = {
      expr match {
        case IntLit(value)                   => Some(value)
        case LongLit(value)                  => Some(value)
        case CharLit(value)                  => Some(value)
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

    private def toMaybeKnowledge(knowledge: VarKnowledge): MaybeKnowledge = knowledge match {
      case knowledge: MaybeKnowledge => knowledge
      case _                         => MaybeKnowledge(knowledge)
    }

    private def addWrapped[T <: VarKnowledgeWrapper : ClassTag](from: Knowledge, cons: VarKnowledge => T) = {
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
        case Access(_, _: MethodCall)       => None
        case Access(This(), id: VariableID) => Some(VarIdentifier(id.getSymbol))
        case Access(obj, id: VariableID)    => getIdentifier(obj) match {
          case Some(objId) => Some(AccessIdentifier(objId, id.getSymbol))
          case None        => None
        }
        case _                              => None
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
        // vars should be default initialized but not vals
        val isFinal = varId.symbol exists { _.isFinal }
        val s: Set[VarKnowledge] = if ((varTpe in Primitives) && !isFinal) Set(Initialized) else Set()
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

    private def getUnaryOperator(unaryOperatorTree: UnaryOperatorTree): Option[Long => Long] = unaryOperatorTree match {
      case _: Hash                   => Some(_.hashCode.toLong)
      case _: Negation               => Some(-_)
      case _: LogicNot               => Some(~_)
      case _: IncrementDecrementTree => None
    }

    private def getBinaryOperator(binaryOperatorTree: BinaryOperatorTree): (Long, Long) => Long = binaryOperatorTree match {
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

    override def toString: String = varKnowledgeDescription + flowKnowledgeDescription

    private def varKnowledgeDescription: String = {
      if (varKnowledge.isEmpty)
        return ""

      varKnowledge
        .map { case (expr, knowledge) => (expr.toString, s"{ ${ knowledge.mkString(", ") } }") }
        .aligned
    }

    private def flowKnowledgeDescription: String = {
      flowEnded match {
        case Some(ended) => NL + s"Flow ended at ${ ended.line }: $ended"
        case None        => ""
      }
    }
  }

}
