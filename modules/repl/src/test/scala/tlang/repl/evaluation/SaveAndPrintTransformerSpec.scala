package tlang.repl.evaluation

import tlang.compiler.analyzer.Types
import tlang.compiler.ast.Trees._
import tlang.compiler.code.TreeBuilder
import tlang.compiler.imports.Imports
import tlang.compiler.testutils.TreeTesting
import tlang.testutils.UnitSpec

class SaveAndPrintTransformerSpec extends UnitSpec with TreeTesting {

  behavior of "A SaveAndPrintTransformer"

  it should "add a variable declaration and a print statement marked blocks" in {
    // This is what is generated by ReplState when fetching a compilation unit
    val newStatement1 = Plus(IntLit(1), IntLit(2)).setType(Types.Int)
    val newStatement2 = Times(IntLit(3), IntLit(4)).setType(Types.Int)
    val statements = Block(List(
      Println(StringLit("A")),
      Println(StringLit("B")),
      Block(List(
        Evaluator.PrintMarker,
        newStatement1,
        newStatement2,
        Evaluator.PrintMarker)
      )
    ))


    val treeBuilder = mock[TreeBuilder]
    treeBuilder.createValDecl("res0", newStatement1, "") returns
      VarDecl("res0", Some(IntType), initiation = newStatement1, modifiers = Set(Private(), Final()))

    treeBuilder.createValDecl("res1", newStatement2, "") returns
      VarDecl("res1", Some(IntType), initiation = newStatement2, modifiers = Set(Private(), Final()))

    treeBuilder.stringConcat(StringLit("val res0: Int = "), VariableID("res0")) returns
      Plus(StringLit("val res0: Int = "), VariableID("res0"))

    treeBuilder.stringConcat(StringLit("val res1: Int = "), VariableID("res1")) returns
      Plus(StringLit("val res1: Int = "), VariableID("res1"))


    val replState = mock[ReplState]
    val imports = mock[Imports]
    imports.replaceNames("T::lang::Int") returns "Int"
    replState.imports returns imports


    val saveAndPrintTransformer = SaveAndPrintTransformer(treeBuilder, replState)
    val transformed = saveAndPrintTransformer(statements)

    transformed shouldBe Block(List(
      Println(StringLit("A")),
      Println(StringLit("B")),
      Evaluator.PrintMarker,
      VarDecl("res0", Some(IntType), initiation = newStatement1, modifiers = Set(Private(), Final())),
      Println(Plus(StringLit("val res0: Int = "), VariableID("res0"))),
      VarDecl("res1", Some(IntType), initiation = newStatement2, modifiers = Set(Private(), Final())),
      Println(Plus(StringLit("val res1: Int = "), VariableID("res1"))),
      Evaluator.PrintMarker
    ))

    there was one(replState).setNewStatements(List(
      Evaluator.PrintMarker,
      VarDecl("res0", Some(IntType), initiation = newStatement1, modifiers = Set(Private(), Final())),
      Println(Plus(StringLit("val res0: Int = "), VariableID("res0"))),
      VarDecl("res1", Some(IntType), initiation = newStatement2, modifiers = Set(Private(), Final())),
      Println(Plus(StringLit("val res1: Int = "), VariableID("res1"))),
      Evaluator.PrintMarker
    ))
  }

}
