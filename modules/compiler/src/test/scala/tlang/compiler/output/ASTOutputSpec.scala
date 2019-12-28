package tlang
package compiler
package output

import tlang.compiler.analyzer.Symbols.{ClassSymbol, MethodSymbol}
import tlang.compiler.ast.Trees._
import tlang.compiler.ast.{PrettyPrinter, TreePrinter}
import tlang.compiler.imports.Imports
import tlang.compiler.output.debug.ASTOutput
import tlang.formatting.{ErrorStringContext, Formatter}
import tlang.testutils.{TestContext, TreeTesting, UnitSpec}
import tlang.testutils.snapshot.SnapshotTesting

class ASTOutputSpec extends UnitSpec with TestContext with TreeTesting with SnapshotTesting {

  private val tree = {
    val classSymbol = new ClassSymbol("ASTOutputSpec")
    CompilationUnit(
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
                Println(IntLit(1)).setNoPos(),
                Println(IntLit(2)).setNoPos()
              ))
            ).setRandomPos(),
            MethodDecl("D",
              modifiers = Set(Public(), Static()),
              args = Nil,
              retType = None,
              stat = Return(IntLit(1)).setNoPos()
            ).setRandomPos().setSymbol(new MethodSymbol("D", classSymbol)),
            MethodDecl("E",
              modifiers = Set(Private()),
              args = List(Formal("A", "a").setRandomPos()),
              retType = None,
              stat = Return(IntLit(1)).setNoPos()
            ).setRandomPos().setSymbol(new MethodSymbol("E", classSymbol))
          )
        ).setSymbol(classSymbol),
        ClassDecl("F").setRandomPos().setSymbol(new ClassSymbol("F")),
        TraitDecl("G").setRandomPos()
      ),
      imports = Imports(
        TestContext,
        ErrorStringContext()(Formatter.SimpleFormatter),
        imports = List(RegularImport(List("B")), WildCardImport(List("C")))
      )
    )
  }

  it should "print debug info for trees" in {
    test("using pretty formatting") { makeASTOutput(Formatter.PrettyFormatter).pretty should matchSnapshot }
    test("using simple formatting") { makeASTOutput(Formatter.SimpleFormatter).pretty should matchSnapshot }
  }

  it should "not output JSON" in {
    makeASTOutput(Formatter.SimpleFormatter).json shouldBe empty
  }

  private def makeASTOutput(implicit formatter: Formatter) = {
    val prettyPrinter = PrettyPrinter()
    var id = 0
    val idFunction = (_: Any) => {
      id += 1
      id
    }
    val treePrinter = TreePrinter(idFunction = idFunction)
    ASTOutput(prettyPrinter, treePrinter, "TestPhase", List(tree))
  }
}
