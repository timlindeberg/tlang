package tlang
package repl
package evaluation

import better.files.File
import tlang.compiler.ast.Trees._
import tlang.compiler.imports.Imports
import tlang.compiler.testutils.TreeTesting
import tlang.testutils.UnitSpec
import tlang.utils.{ExecutionResult, ProgramExecutor, StringSource}

class EvaluatorSpec extends UnitSpec with TreeTesting {

  behavior of "An Evaluator"

  // MOCKING GALORE
  // This test mostly verifies that output is chained correctly
  // between dependencies.
  // It also serves as an example of how the evaluation works.
  it should "evaluate commands" in {
    val command =
      """
        |class A =
        | Def plusOne(a: Int) = a + 1
        |
        |new A().plusOne(5)
      """.stripMargin

    val imports = mock[Imports]
    imports.imports returns Nil

    val parsedCommand = CompilationUnit(
      Package(),
      classes = List(
        ClassDecl(Evaluator.ReplClassID, methods = List(
          MethodDeclTree.mainMethod(
            NormalAccess(
              New(ClassID("A"), args = Nil),
              MethodCall(MethodID("print"), List(IntLit(5)))
            )
          )
        )),
        ClassDecl("A", methods = List(
          MethodDecl(
            id = MethodID("plusOne"),
            modifiers = Set(Public()),
            args = List(Formal(IntType, "a")),
            stat = Plus(VariableID("a"), IntLit(1))
          )
        ))
      ),
      imports
    )

    val parse = mock[(List[StringSource]) => List[CompilationUnit]]
    parse(List(StringSource(command, Evaluator.ClassName))) returns List(parsedCommand)

    val extractor = mock[Extractor]
    extractor(parsedCommand) returns List("Defined class A")

    val replState = mock[ReplState]

    val adjustedCompilationUnit = CompilationUnit(
      Package(),
      classes = List(
        ClassDecl(Evaluator.ReplClassID, methods = List(
          MethodDeclTree.mainMethod(List(
            // History added
            VarDecl("res0", Some(IntType), Some(Plus(IntLit(1), IntLit(2))), Set(Private(), Final())),
            // Block with print markers added
            Block(List(
              Evaluator.PrintMarker,
              NormalAccess(
                New(ClassID("A"), args = Nil),
                MethodCall(MethodID("print"), List(IntLit(5)))
              ),
              Evaluator.PrintMarker
            ))
          ))
        )),
        ClassDecl("A", methods = List(
          MethodDecl(
            id = MethodID("plusOne"),
            modifiers = Set(Public()),
            args = List(Formal(IntType, "a")),
            stat = Plus(VariableID("a"), IntLit(1))
          )
        ))
      ),
      imports
    )

    replState.compilationUnit returns adjustedCompilationUnit

    val analyze = mock[(List[CompilationUnit]) => List[CompilationUnit]]

    // Should really return the tree with symbols and types but whatever
    analyze(List(adjustedCompilationUnit)) returns List(adjustedCompilationUnit)

    val saveAndPrintTransformed = CompilationUnit(
      Package(),
      classes = List(
        ClassDecl(Evaluator.ReplClassID, methods = List(
          MethodDeclTree.mainMethod(List(
            // History added
            VarDecl("res0", Some(IntType), Some(Plus(IntLit(1), IntLit(2))), Set(Private(), Final())),
            // Block removed
            Evaluator.PrintMarker,
            // Variable declaration added
            VarDecl("res1",
              Some(IntType),
              Some(
                NormalAccess(
                  New(ClassID("A"), args = Nil),
                  MethodCall("print", List(IntLit(5)))
                )
              ),
              Set(Private(), Final())
            ),
            // Print statement added
            Println(Plus(StringLit("val res1: Int = "), VariableID("res1"))),
            Evaluator.PrintMarker
          ))
        )),
        ClassDecl("A", methods = List(
          MethodDecl("plusOne",
            modifiers = Set(Public()),
            args = List(Formal(IntType, "a")),
            stat = Plus(VariableID("a"), IntLit(1))
          )
        ))
      ),
      imports
    )

    val saveAndPrintTransformer = mock[SaveAndPrintTransformer]
    saveAndPrintTransformer(adjustedCompilationUnit) returns saveAndPrintTransformed

    val compile = mock[List[CompilationUnit] => Unit]

    val classFile = mock[File]
    val programExecutor = mock[ProgramExecutor]

    programExecutor(classFile) returns ExecutionResult(
      output =
        s"""
           |${ Evaluator.ReplOutputMarker }val res1: Int = 6
           |${ Evaluator.ReplOutputMarker }
           |"""".stripMargin,
      time = 0
    )

    val evaluator = Evaluator(
      classFile,
      extractor,
      programExecutor,
      saveAndPrintTransformer,
      replState,
      parse,
      analyze,
      compile
    )

    evaluator(command) shouldBe
      """|Defined class A
         |val res1: Int = 6""".stripMargin

    there was one(parse).apply(List(StringSource(command, Evaluator.ClassName)))
    there was one(extractor).apply(parsedCommand)
    there was one(analyze).apply(List(adjustedCompilationUnit))
    there was one(compile).apply(List(saveAndPrintTransformed))
    there was one(saveAndPrintTransformer).apply(adjustedCompilationUnit)
    there was one(programExecutor).apply(classFile)
  }
}
