package tcompiler

import java.io.File

import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}
import tcompiler.analyzer.{NameAnalysis, Symbols, TypeChecking}
import tcompiler.ast.Parser
import tcompiler.lexer.Lexer
import tcompiler.modification.{Imports, Templates}
import tcompiler.utils.{CompilationException, Context}

/**
 * Created by Tim Lindeberg on 4/2/2016.
 */
abstract class ErrorTester extends FlatSpec with Matchers with BeforeAndAfter {

  import TestUtils._

  def Name: String
  def Path: String

  before {
    Symbols.ID.reset()
  }

  behavior of Name
  TestUtils.programFiles(Path).foreach(test)

  def test(file: File): Unit =
    if (file.isDirectory)
      TestUtils.programFiles(file.getPath).foreach(test)
    else
      it should file.getName.toString in testFile(file)

  private def testFile(file: File): Unit = {
    val ctx = new Context(reporter = new tcompiler.utils.Reporter, file = file, outDir = Some(new File("./gen/" + file.getName + "/")))
    val expectedErrors = TestUtils.parseSolutions(file)

    try {
      (Lexer andThen Parser andThen Templates andThen Imports andThen NameAnalysis andThen TypeChecking).run(ctx)(ctx.file)
      // Check for warnings:
      if(ctx.reporter.warnings.isEmpty)
        fail("Test failed: No errors or warnings!")

      val warnings = ctx.reporter.warnings.mkString("\n\n")
      println(warnings)
      val warningCodes = TestUtils.parseErrorCodes(warnings)
      assertCorrect(warningCodes, expectedErrors)
    } catch {
      case t: CompilationException =>
        println(t.getMessage)
        println("---------------------------------------------------------------------")
        println
        val errorCodes = TestUtils.parseErrorCodes(t.getMessage)
        assertCorrect(errorCodes, expectedErrors)
    }
  }
}
