package tlang
package compiler
package lowering

import tlang.compiler.analyzer.Symbols._
import tlang.compiler.analyzer.Types.{Int, TObject}
import tlang.compiler.ast.Trees._
import tlang.compiler.imports.Imports

/*
 * Replaces overloaded operator calls with calls to static methods
 *
 * Examples:
 * --------------------------------------------------------------------------------
 * a + b -> A.$Plus(a, b)
 * --------------------------------------------------------------------------------
 * ++a => A.$PreIncrement(a)
 * --------------------------------------------------------------------------------
 * a[5] = 5 => a.$ArrayAssign(5, 5)
 * --------------------------------------------------------------------------------
 */
case class OperatorCallLowerer(imports: Imports) extends TreeLowerer {

  private val treeBuilder = new TreeBuilder

  override def lower: PartialFunction[Tree, Tree] = {
    case op: OperatorTree => op match {
      // Don't replace these operators
      case _: And | _: Or | _: Not | _: ExtractNullable => op
      case BinaryOperatorTree(lhs, rhs)                 => lowerBinaryOperatorCall(op, lhs, rhs)
      case UnaryOperatorTree(expr)                      => lowerUnaryOperatorCall(op, expr)
      case ArrayOperatorTree(arr)                       => lowerArrayOperatorCall(op, arr)
      case _                                            => op
    }
  }

  private def lowerBinaryOperatorCall(op: OperatorTree, lhs: ExprTree, rhs: ExprTree): Tree = {
    // Don't replace null checks
    if (lhs.isInstanceOf[NullLit] || rhs.isInstanceOf[NullLit])
      return op

    if (!(isObject(lhs) || isObject(rhs)))
      return op

    val opSymbol = op.lookupOperator((lhs.getType, rhs.getType), imports).get
    val obj = getClassID(opSymbol)
    treeBuilder.createMethodCall(obj, opSymbol, lhs, rhs)
  }

  private def lowerUnaryOperatorCall(op: OperatorTree, expr: ExprTree): Tree = {
    if (!isObject(expr))
      return op

    val opSymbol = op.lookupOperator(expr.getType, imports).get
    val obj = getClassID(opSymbol)
    treeBuilder.createMethodCall(obj, opSymbol, expr)
  }

  private def lowerArrayOperatorCall(op: OperatorTree, arr: ExprTree): Tree = {
    val (obj, args) = op match {
      case ArrayRead(obj, index)               =>
        (obj, List(index))
      case Assign(ArrayRead(obj, index), expr) =>
        (obj, List(index, expr))
      case ArraySlice(obj, start, end, step)   =>
        val s = start.getOrElse(NullLit()).setType(Int.getNullable)
        val e = end.getOrElse(NullLit()).setType(Int.getNullable)
        val st = step.getOrElse(NullLit()).setType(Int.getNullable)
        (obj, List(s, e, st))
      case _                                   => ???
    }
    if (!isObject(obj)) {
      return op
    }

    val objectClassSymbol = obj.getType.asInstanceOf[TObject].classSymbol
    val opSymbol = objectClassSymbol.lookupOperator(op, args.map { _.getType }, imports).get
    treeBuilder.createMethodCall(obj, opSymbol, args)
  }

  private def isObject(expr: ExprTree) = expr.getType.isInstanceOf[TObject]

  private def getClassID(operatorSymbol: OperatorSymbol): ClassID = {
    val classSymbol = operatorSymbol.classSymbol
    ClassID(classSymbol.name).setSymbol(classSymbol)
  }
}
