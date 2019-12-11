package tlang
package compiler
package lowering

import tlang.compiler.ast.Trees._

/*
 * Transform increment and decrement expressions on accesses and array reads.
 *
 * Examples:
 * --------------------------------------------------------------------------------
 * a++
 *
 * becomes:
 *
 * var $v = a
 * a = a + 1
 * $v
 * --------------------------------------------------------------------------------
 * --a
 *
 * becomes:
 *
 * a = a - 1
 * a
 * --------------------------------------------------------------------------------
 * GetObject().I++
 *
 * becomes:
 *
 * var $obj = GetObject()
 * var $v = $tmp.I
 * var $newV = $v + 1
 * $tmp.I = $newV
 * $v
 * --------------------------------------------------------------------------------
 * --GetArray()[GetIndex()*4]
 *
 * becomes:
 *
 * var $arr
 * var $idx = GetIndex()*4
 * var $v = a[$idx]
 * var $newV = $v - 1
 * $arr[$idx] = $newV
 * newV$x
 * --------------------------------------------------------------------------------
 */
case class IncrementDecrementLowerer() extends TreeLowerer {

  private val treeBuilder = new TreeBuilder

  import treeBuilder._

  override def lower: PartialFunction[Tree, Tree] = {
    case incDec: IncrementDecrementTree => incDec.expr match {
      case variable: VariableID => lowerVariableIdIncDec(incDec, variable)
      case acc: Access          => lowerAccessIncDec(incDec, acc)
      case arrayRead: ArrayRead => lowerArrReadIncDec(incDec, arrayRead)
    }
  }

  private def lowerVariableIdIncDec(incDec: IncrementDecrementTree, variable: VariableID) = {
    val plusOrMinus = getPlusOrMinus(incDec, variable)
    val v = if (incDec.isPre) variable else putVarDecl("v", variable)
    createIncDecExpr(incDec, variable, plusOrMinus, v)
  }

  private def lowerAccessIncDec(incDec: IncrementDecrementTree, acc: Access) = {
    val Access(obj, application) = acc
    obj match {
      case _: Identifier[_] =>
        val v = if (incDec.isPre) acc else putVarDecl("v", acc)
        createIncDecExpr(incDec, acc, v)
      case _                =>
        val objId = putVarDecl("obj", obj)
        val newAccess = NormalAccess(objId, application).setType(application)
        val v = if (newAccess.isStatic && incDec.isPre) newAccess else putVarDecl("v", newAccess)
        createIncDecExpr(incDec, newAccess, v)
    }
  }

  private def lowerArrReadIncDec(incDec: IncrementDecrementTree, arrayRead: ArrayRead) = {
    val ArrayRead(arr, index) = arrayRead

    val arrId = arr match {
      case _: VariableID => arr
      case _             => putVarDecl("arr", arr)
    }
    val indexId = index match {
      case _: VariableID | _: Literal[_] => index
      case _                             => putVarDecl("idx", index)
    }

    val a = ArrayRead(arrId, indexId).setType(arrayRead)
    val v = putVarDecl("v", a)
    createIncDecExpr(incDec, a, v)
  }

  private def createIncDecExpr(incDec: IncrementDecrementTree, assignTo: Assignable, value: ExprTree): Tree = {
    val plusOrMinus = getPlusOrMinus(incDec, value)
    if (incDec.isPre) {
      val newValue = putVarDecl("newV", plusOrMinus)
      createIncDecExpr(incDec, assignTo, newValue, newValue)
    } else {
      createIncDecExpr(incDec, assignTo, plusOrMinus, value)
    }
  }

  private def createIncDecExpr(incDec: IncrementDecrementTree, to: Assignable, from: ExprTree, value: ExprTree): Tree = {
    put(Assign(to, from).setType(to))
    put(PutOnStack(value))
    setPos(incDec)
    getCode
  }

  private def getPlusOrMinus(incDec: IncrementDecrementTree, value: ExprTree) = {
    val oneLiteral = createOne(value.getType)
    val plusOrMinus = if (incDec.isIncrement) Plus(value, oneLiteral) else Minus(value, oneLiteral)
    plusOrMinus.setType(value)
  }
}
