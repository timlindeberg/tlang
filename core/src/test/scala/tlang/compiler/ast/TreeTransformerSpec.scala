package tlang.compiler.ast

import tlang.compiler.ast.Trees._
import tlang.testutils.UnitSpec

class TreeTransformerSpec extends UnitSpec {

  behavior of "A tree transformer"

  it should "transform a tree" in {

    val addOneToIntLiterals = new Trees.Transformer {
      def transformation: TreeTransformation = {
        case IntLit(value) => IntLit(value + 1)
      }
    }

    val clazz = ClassDecl(
      Trees.ClassID("Clazzy"),
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

    addOneToIntLiterals(clazz) shouldBe ClassDecl(
      Trees.ClassID("Clazzy"),
      fields = List(
        VarDecl(VariableID("X"), initiation = Some(IntLit(1)))
      ),
      methods = List(
        MethodDecl(MethodID("Method1"), stat = Some(Block(List(
          Plus(IntLit(6), IntLit(7)),
          VarDecl(VariableID("x"), initiation = Some(IntLit(11)))
        )
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
      Trees.ClassID("Clazzy"),
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

    val modified = addOneToLongLiterals(clazz)
    modified should be theSameInstanceAs clazz
  }

}