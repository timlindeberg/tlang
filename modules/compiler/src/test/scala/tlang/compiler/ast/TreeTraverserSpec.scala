package tlang.compiler.ast

import tlang.compiler.ast.Trees._
import tlang.compiler.testutils.TreeTesting
import tlang.testutils.UnitSpec

class TreeTraverserSpec extends UnitSpec with TreeTesting {

  behavior of "A Tree Traverser"

  it should "visit each node once" in {

    val clazz = ClassDecl(
      ClassID("Clazzy"),
      fields = List(
        VarDecl("X", initiation = IntLit(0))
      ),
      methods = List(
        MethodDecl("Method1", stat = Block(List(
          Plus(IntLit(5), IntLit(6)),
          VarDecl("x", initiation = IntLit(10))
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
        VarDecl("X", initiation = IntLit(0))
      ),
      methods = List(
        MethodDecl("Method1", stat = Block(List(
          Plus(IntLit(5), IntLit(6)),
          VarDecl("x", initiation = IntLit(10))
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
