package tlang.compiler.ast

import tlang.compiler.ast.Trees._
import tlang.testutils.UnitSpec

class TreeTransformerSpec extends UnitSpec {

  behavior of "A tree transformer"

  it should "transform a tree" in {

    val addOneToIntLiterals = new Trees.Transformer {
      override protected def _transform(t: Tree): Tree = t match {
        case IntLit(value) => IntLit(value + 1)
        case _             => super._transform(t)
      }
    }

    val clazz = ClassDecl(
      Trees.ClassID("Clazzy"),
      fields = List(
        VarDecl(VariableID("X"), initation = Some(IntLit(0)))
      ),
      methods = List(
        MethodDecl(MethodID("Method1"), stat = Some(Block(List(
          Plus(IntLit(5), IntLit(6)),
          VarDecl(VariableID("x"), initation = Some(IntLit(10)))
        )
        )))
      )
    )

    addOneToIntLiterals(clazz) shouldBe ClassDecl(
      Trees.ClassID("Clazzy"),
      fields = List(
        VarDecl(VariableID("X"), initation = Some(IntLit(1)))
      ),
      methods = List(
        MethodDecl(MethodID("Method1"), stat = Some(Block(List(
          Plus(IntLit(6), IntLit(7)),
          VarDecl(VariableID("x"), initation = Some(IntLit(11)))
        )
        )))
      )
    )
  }

  it should "return the same tree if the tree has not been modified" in {

    val addOneToLongLiteralsTransformer = new Trees.Transformer {
      override protected def _transform(t: Tree): Tree = t match {
        case LongLit(value) => LongLit(value + 1)
        case _              => super._transform(t)
      }
    }

    val clazz = ClassDecl(
      Trees.ClassID("Clazzy"),
      fields = List(
        VarDecl(VariableID("X"), initation = Some(IntLit(0)))
      ),
      methods = List(
        MethodDecl(MethodID("Method1"), stat = Some(Block(List(
          Plus(IntLit(5), IntLit(6)),
          VarDecl(VariableID("x"), initation = Some(IntLit(10)))
        )
        )))
      )
    )

    val modified = addOneToLongLiteralsTransformer(clazz)
    modified should be theSameInstanceAs clazz
  }

}
