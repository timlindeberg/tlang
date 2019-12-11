package tlang
package compiler
package lowering

import tlang.compiler.ast.Trees.Tree

trait TreeLowerer {
  def apply(tree: Tree): Tree = {
    val f = lower
    if (f.isDefinedAt(tree)) f(tree) else tree
  }

  protected def lower: PartialFunction[Tree, Tree]
}
