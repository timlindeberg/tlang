package tlang
package repl
package evaluation

import tlang.compiler.analyzer.Types
import tlang.compiler.ast.PrettyPrinter
import tlang.compiler.ast.Trees._
import tlang.compiler.imports.Imports
import tlang.testutils.{TreeTesting, UnitSpec}

class ReplStateSpec extends UnitSpec with TreeTesting {

  behavior of "ReplState"

  it should "be modified" in {
    val replState = ReplState(mock[PrettyPrinter], mock[Imports])
    val methodA = createMethod(
      name = "A",
      args = List(Types.String),
      retType = Types.Int
    )
    val methodB = createMethod(
      name = "B",
      args = List(Types.Int, Types.String),
      retType = Types.String
    )
    replState.addMethods(List(methodA, methodB))

    val classA = ClassDecl("A")
    val classB = ClassDecl("B")

    replState.addClasses(List(classA, classB))

    replState.setNewStatements(List(
      Plus(IntLit(1), IntLit(2))
    ))

    replState.addStatementsToHistory()

    replState.setNewStatements(List(
      Times(IntLit(2), IntLit(3))
    ))

    val cu = replState.compilationUnit

    cu.classes should contain(classA)
    cu.classes should contain(classB)

    val maybeMainClass = cu.classes.find { _.id == Evaluator.ReplClassID }
    maybeMainClass should not be empty
    val mainClass = maybeMainClass.get

    mainClass.methods should contain(methodB)

    val maybeMainMethod = mainClass.methods.find { _.isMain }
    maybeMainMethod should not be empty
    val mainMethod = maybeMainMethod.get

    mainMethod.stat should not be empty
    val stat = mainMethod.stat.get
    stat shouldBe a[Block]
    val stats = stat.asInstanceOf[Block].stats

    stats should contain(Plus(IntLit(1), IntLit(2)))

    // The new statement to be printed:
    stats should contain(Block(List(Evaluator.PrintMarker, Times(IntLit(2), IntLit(3)), Evaluator.PrintMarker)))
  }

  it should "overwrite classes with the same name" in {
    val replState = ReplState(mock[PrettyPrinter], mock[Imports])

    val classA = ClassDecl("A")
    val classB = ClassDecl("A")
    val classC = ClassDecl("B")

    replState.addClasses(List(classA, classB, classC))

    val cu = replState.compilationUnit

    // Including the main class
    cu.classes should have size 3
    val c = cu.classes.find { _.id.name == "A" }
    c should not be empty
    c.get should be theSameInstanceAs classB
    cu.classes should contain(classC)
  }

  it should "overwrite methods with the same signature" in {
    val replState = ReplState(mock[PrettyPrinter], mock[Imports])

    val methodA = createMethod(
      name = "A",
      args = List(Types.String, Types.Int),
      retType = Types.Int
    )
    val methodB = createMethod(
      name = "A",
      args = List(Types.String, Types.Int),
      retType = Types.String
    )
    val methodC = createMethod(
      name = "A",
      args = List(Types.Int, Types.String),
      retType = Types.String
    )
    replState.addMethods(List(methodA, methodB, methodC))

    val cu = replState.compilationUnit

    val maybeMainClass = cu.classes.find { _.id == Evaluator.ReplClassID }
    maybeMainClass should not be empty
    val mainClass = maybeMainClass.get

    // Including the main method
    mainClass.methods should have size 3
    val m = mainClass.methods.find { _.signature == "A(java::lang::String, T::lang::Int)" }
    m should not be empty
    m.get should be theSameInstanceAs methodB

    mainClass.methods should contain(methodC)
  }

  it should "not add print statements to history" in {
    val replState = ReplState(mock[PrettyPrinter], mock[Imports])

    replState.setNewStatements(List(
      Print(IntLit(1)),
      Println(StringLit("ABC"))
    ))

    replState.addStatementsToHistory()
    replState.history shouldBe empty
  }
}
