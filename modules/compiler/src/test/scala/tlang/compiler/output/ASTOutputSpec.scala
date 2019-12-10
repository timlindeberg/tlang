package tlang
package compiler
package output

import tlang.compiler.ast.Trees._
import tlang.compiler.ast.{PrettyPrinter, TreePrinter}
import tlang.compiler.imports.Imports
import tlang.compiler.output.debug.ASTOutput
import tlang.compiler.testutils.TreeTesting
import tlang.formatting.{ErrorStringContext, Formatter}
import tlang.testutils.UnitSpec
import tlang.testutils.snapshot.SnapshotTesting

class ASTOutputSpec extends UnitSpec with TestContext with TreeTesting with SnapshotTesting {

  private val tree = CompilationUnit(
    pack = Package(List("A", "B", "C")),
    classes = List(
      ClassDecl("ASTOutputSpec",
        parents = List("F", "G"),
        fields = Nil,
        methods = List(
          MethodDecl("main",
            modifiers = Set(Public(), Static()),
            args = List(Formal(ArrayType("java::lang::String"), "args")),
            retType = UnitType(),
            stat = Block(List(
              Println(IntLit(1)),
              Println(IntLit(2))
            ))
          ),
          MethodDecl("D",
            modifiers = Set(Public(), Static()),
            args = Nil,
            retType = None,
            stat = Return(IntLit(1))
          ),
          MethodDecl("E",
            modifiers = Set(Private()),
            args = List(Formal("A", "a")),
            retType = None,
            stat = Return(IntLit(1))
          )
        )
      ),
      ClassDecl("F"),
      TraitDecl("G")
    ),
    imports = Imports(
      TestContext,
      ErrorStringContext()(Formatter.SimpleFormatter),
      imports = List(RegularImport(List("B")), WildCardImport(List("C")))
    )
  )

  it should "print debug info for trees" in {
    test("using pretty formatting") { makeASTOutput(Formatter.PrettyFormatter).pretty should matchSnapshot }
    test("using simple formatting") { makeASTOutput(Formatter.SimpleFormatter).pretty should matchSnapshot }
  }

  it should "not output JSON" in {
    makeASTOutput(Formatter.SimpleFormatter).json shouldBe empty
  }

  private val prettyPrinter = mock[PrettyPrinter] use { _.apply(*[Tree]) returns "Tree but pretty printed!" }
  private val treePrinter = mock[TreePrinter] use { _.apply(*[Tree]) returns List(("A", "B", "C", "D", "E")) }

  private def makeASTOutput(formatter: Formatter) =
    ASTOutput(prettyPrinter, treePrinter, "TestPhase", List(tree))(formatter)
}
