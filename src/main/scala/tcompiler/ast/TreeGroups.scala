package tcompiler.ast

import tcompiler.ast.Trees._

object TreeGroups {
  object BinaryOperator {
    def unapply(e: ExprTree): Option[(ExprTree, ExprTree)] = e match {
      case Plus(lhs, rhs)   => Some((lhs, rhs))
      case Minus(lhs, rhs)  => Some((lhs, rhs))
      case Times(lhs, rhs)  => Some((lhs, rhs))
      case Div(lhs, rhs)    => Some((lhs, rhs))
      case Modulo(lhs, rhs) => Some((lhs, rhs))
      case _                => None
    }
  }

  object LogicalOperator {
    def unapply(e: ExprTree): Option[(ExprTree, ExprTree)] = e match {
      case LogicAnd(lhs, rhs) => Some((lhs, rhs))
      case LogicOr(lhs, rhs)  => Some((lhs, rhs))
      case LogicXor(lhs, rhs) => Some((lhs, rhs))
      case _                  => None
    }
  }

  object ShiftOperator {
    def unapply(e: ExprTree): Option[(ExprTree, ExprTree)] = e match {
      case LeftShift(lhs, rhs)  => Some((lhs, rhs))
      case RightShift(lhs, rhs) => Some((lhs, rhs))
      case _                    => None
    }
  }

  object ComparisonOperator {
    def unapply(e: ExprTree): Option[(ExprTree, ExprTree)] = e match {
      case LessThan(lhs, rhs)          => Some((lhs, rhs))
      case LessThanEquals(lhs, rhs)    => Some((lhs, rhs))
      case GreaterThan(lhs, rhs)       => Some((lhs, rhs))
      case GreaterThanEquals(lhs, rhs) => Some((lhs, rhs))
      case _                           => None
    }
  }

  object EqualsOperator {
    def unapply(e: ExprTree): Option[(ExprTree, ExprTree)] = e match {
      case Equals(lhs, rhs)    => Some((lhs, rhs))
      case NotEquals(lhs, rhs) => Some((lhs, rhs))
      case _                   => None
    }
  }

  // Other stuff

  object PrintStatement {
    def unapply(e: StatTree): Option[ExprTree] = e match {
      case Print(expr)   => Some(expr)
      case Println(expr) => Some(expr)
      case _             => None
    }
  }

  object MathBinaryExpr {
    def unapply(e: ExprTree): Option[(ExprTree, ExprTree)] = e match {
      case Plus(lhs, rhs)       => Some((lhs, rhs))
      case Minus(lhs, rhs)      => Some((lhs, rhs))
      case LogicAnd(lhs, rhs)   => Some((lhs, rhs))
      case LogicOr(lhs, rhs)    => Some((lhs, rhs))
      case LogicXor(lhs, rhs)   => Some((lhs, rhs))
      case Times(lhs, rhs)      => Some((lhs, rhs))
      case Div(lhs, rhs)        => Some((lhs, rhs))
      case Modulo(lhs, rhs)     => Some((lhs, rhs))
      case LeftShift(lhs, rhs)  => Some((lhs, rhs))
      case RightShift(lhs, rhs) => Some((lhs, rhs))
      case _                    => None
    }
  }

  object Comparison {
    def unapply(e: ExprTree): Option[(ExprTree, ExprTree)] = e match {
      case And(lhs, rhs)               => Some((lhs, rhs))
      case Or(lhs, rhs)                => Some((lhs, rhs))
      case LessThan(lhs, rhs)          => Some((lhs, rhs))
      case LessThanEquals(lhs, rhs)    => Some((lhs, rhs))
      case GreaterThan(lhs, rhs)       => Some((lhs, rhs))
      case GreaterThanEquals(lhs, rhs) => Some((lhs, rhs))
      case Equals(lhs, rhs)            => Some((lhs, rhs))
      case NotEquals(lhs, rhs)         => Some((lhs, rhs))
      case Instance(lhs, rhs)          => Some((lhs, rhs))
      case _                           => None
    }
  }

  object IncrementDecrement {
    def unapply(e: Tree): Option[ExprTree] = e match {
      case PreIncrement(id)  => Some(id)
      case PostIncrement(id) => Some(id)
      case PreDecrement(id)  => Some(id)
      case PostDecrement(id) => Some(id)
      case _                 => None
    }
  }
}
