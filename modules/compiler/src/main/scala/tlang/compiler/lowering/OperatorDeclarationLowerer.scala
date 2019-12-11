package tlang
package compiler
package lowering

import tlang.compiler.analyzer.Symbols._
import tlang.compiler.ast.Trees._

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
case class OperatorDeclarationLowerer() extends TreeLowerer {

  private val treeBuilder = new TreeBuilder

  override def lower: PartialFunction[Tree, Tree] = {
    case op: OperatorDecl =>
      val opSymbol = op.getSymbol.asInstanceOf[OperatorSymbol]
      val methDecl = getMethodDeclaration(op, opSymbol)
      methDecl.setSymbol(opSymbol).setPos(op)
  }

  private def getMethodDeclaration(op: OperatorDecl, opSymbol: OperatorSymbol): MethodDecl = {
    val methodID = MethodID(opSymbol.name).setSymbol(opSymbol).setPos(op)

    if (op.isAbstract)
      return MethodDecl(methodID, op.modifiers, op.annotations, op.args, op.retType, op.stat)

    opSymbol.operatorType match {
      case Assign(ArrayRead(_, _), _) => convertAssignmentOperator(op, opSymbol, methodID)
      case _                          => MethodDecl(methodID, op.modifiers, op.annotations, op.args, op.retType, op.stat)
    }
  }

  private def convertAssignmentOperator(op: OperatorDecl, opSymbol: OperatorSymbol, methodID: MethodID): MethodDecl = {
    // Convert array assignment so the value is returned in order to be consistent with other
    // types of assignments
    val valueId = op.args(1).id
    val retType = treeBuilder.getTypeTree(valueId.getType)
    val statements = getAssignmentOperatorStatements(op, valueId)
    opSymbol.setType(valueId.getType)
    MethodDecl(methodID, op.modifiers, op.annotations, op.args, Some(retType), Some(statements))
  }

  private def getAssignmentOperatorStatements(op: OperatorDecl, valueId: VariableID): Block = {
    val ret = Return(Some(valueId)).setType(valueId.getType)
    val stats = op.stat.get match {
      case Block(stats) if stats.isEmpty => List(ret)
      case Block(stats)                  =>
        val last = stats.last match {
          case Return(Some(s)) => s
          case s: StatTree     => s
        }

        stats.drop(1) :+ last :+ ret
      case Return(Some(s))               => List(s, ret)
      case stat: StatTree                => List(stat, ret)
    }
    Block(stats)
  }
}
