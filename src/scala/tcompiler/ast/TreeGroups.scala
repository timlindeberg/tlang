package tcompiler.ast

import tcompiler.ast.Trees._

object TreeGroups {


  object UselessStatement {
    def unapply(e: StatTree): Option[ExprTree] = e match {
      case _: MethodCall |
           _: Assign |
           _: ArrayAssign |
           _: FieldAssign |
           _: PreIncrement |
           _: PostIncrement |
           _: PreDecrement |
           _: PostDecrement => None
      case expr:ExprTree => Some(expr)
      case _ => None
    }
  }


  // Other stuff

}
