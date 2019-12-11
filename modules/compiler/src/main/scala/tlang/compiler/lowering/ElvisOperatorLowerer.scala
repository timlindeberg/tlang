package tlang
package compiler
package lowering

import tlang.compiler.analyzer.Types.Bool
import tlang.compiler.ast.Trees._

/*
 * Transforms the elvis operator
 *
 * Example:
 * --------------------------------------------------------------------------------
 * val a = b ?: -1
 *
 * becomes:
 *
 * val $tmp = b
 * val a = $tmp == null ? -1 : $tmp
 * --------------------------------------------------------------------------------
 */
case class ElvisOperatorLowerer() extends TreeLowerer {

  private val treeBuilder = new TreeBuilder

  import treeBuilder._

  override def lower: PartialFunction[Tree, Tree] = {
    case elvis@Elvis(nullableValue, ifNull) =>
      val nullableID = putVarDecl("tmp", nullableValue)

      val condition = Equals(nullableID, NullLit()).setType(Bool)
      put(PutOnStack(Ternary(condition, ifNull, nullableID).setType(elvis)))
      setPos(elvis)
      getCode
  }
}
