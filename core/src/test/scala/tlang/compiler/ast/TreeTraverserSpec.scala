package tlang.compiler.ast

import tlang.compiler.ast.Trees._
import tlang.testutils.UnitSpec

class TreeTraverserSpec extends UnitSpec {

  behavior of "A Tree Traverser"

  it should "visit each node once" in {

    val clazz = ClassDecl(
      ClassID("Clazzy"),
      fields = List(
        VarDecl(VariableID("X"), initiation = Some(IntLit(0)))
      ),
      methods = List(
        MethodDecl(MethodID("Method1"), stat = Some(Block(List(
          Plus(IntLit(5), IntLit(6)),
          VarDecl(VariableID("x"), initiation = Some(IntLit(10)))
        )
        )))
      )
    )

    var count = 0

    val traverser = new Traverser {
      override def traversal: TreeTraversal = {
        case t =>
          count += 1
          traverseChildren(t)
      }
    }

    traverser(clazz)

    count shouldBe 14
  }

  it should "not recurse to children unless specified" in {

    val clazz = ClassDecl(
      ClassID("Clazzy"),
      fields = List(
        VarDecl(VariableID("X"), initiation = Some(IntLit(0)))
      ),
      methods = List(
        MethodDecl(MethodID("Method1"), stat = Some(Block(List(
          Plus(IntLit(5), IntLit(6)),
          VarDecl(VariableID("x"), initiation = Some(IntLit(10)))
        )
        )))
      )
    )

    var count = 0

    val traverser = new Traverser {
      override def traversal: TreeTraversal = {
        case _: Plus => // No further traversal
        case t       =>
          count += 1
          traverseChildren(t)
      }
    }

    traverser(clazz)

    count shouldBe 11

  }

}
