package tlang
package compiler
package ast

import tlang.compiler.ast.Trees._
import tlang.compiler.testutils.TreeTesting
import tlang.testutils.UnitSpec

class TreeTransformerSpec extends UnitSpec with TreeTesting {

  behavior of "A tree transformer"

  it should "transform a tree" in {

    val transform = new Trees.Transformer {
      def transformation: TreeTransformation = {
        case IntLit(value)    => IntLit(value + 1)
        case VariableID(name) => VariableID(s"${ name }ABC")
        case Plus(lhs, rhs)   => Times(lhs, rhs)
      }
    }

    val clazz = ClassDecl(
      ClassID("Clazzy"),
      fields = List(
        VarDecl("X", initiation = IntLit(0))
      ),
      methods = List(
        MethodDecl("Method1", stat = Block(List(
          Plus(IntLit(5), IntLit(6)),
          VarDecl("x", initiation = IntLit(10))
        )
        ))
      )
    )

    transform(clazz) shouldBe ClassDecl(
      ClassID("Clazzy"),
      fields = List(
        VarDecl("XABC", initiation = IntLit(1))
      ),
      methods = List(
        MethodDecl("Method1", stat = Block(List(
          Times(IntLit(5), IntLit(6)),
          VarDecl("xABC", initiation = IntLit(11))
        )))
      )
    )
  }

  it should "return the same tree if the tree has not been modified" in {

    val addOneToLongLiterals = new Trees.Transformer {
      def transformation: TreeTransformation = {
        case LongLit(value) => LongLit(value + 1)
      }
    }

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

    val modified = addOneToLongLiterals(clazz)
    modified should be theSameInstanceAs clazz
  }

}
