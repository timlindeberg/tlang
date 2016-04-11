package tcompiler

import java.io.File

import org.scalatest.{FlatSpec, Matchers}
import tcompiler.ast.Printer
import tcompiler.ast.Trees.Program
import tcompiler.utils.{CompilationException, Context, Pipeline}

/**
 * Created by Tim Lindeberg on 4/2/2016.
 */
abstract class ErrorTester extends FlatSpec with Matchers {

  import TestUtils._

  def Seperator = "---------------------------------------------------------------------\n"

  val PrintErrors = false

  def Name: String
  def Path: String
  def Pipeline: Pipeline[File, Program]

  behavior of Name
  TestUtils.programFiles(Path).foreach(test)

  def test(file: File): Unit =
    if (file.isDirectory){
      programFiles(file.getPath).foreach(testFile)
    } else{
      if(shouldBeIgnored(file))
        ignore should file.getName.toString in testFile(file)
      else
        it should file.getName.toString in testFile(file)
    }

  private def testFile(file: File): Unit = {
    val ctx = Context(reporter = new tcompiler.utils.Reporter, file = file, outDir = Some(new File("./gen/" + file.getName + "/")))
    val expectedErrors = TestUtils.parseSolutions(file)

    try {
      val prog = Pipeline.run(ctx)(ctx.file)

      //println(Printer(prog))
      // Check for warnings:
      if(ctx.reporter.warnings.isEmpty)
        fail("Test failed: No errors or warnings!")

      val warnings = ctx.reporter.warnings.mkString("\n\n")
      if(PrintErrors){
        println(Seperator)
        println(warnings)
      }

      val warningCodes = TestUtils.parseErrorCodes(warnings)
      assertCorrect(warningCodes, expectedErrors)
    } catch {
      case t: CompilationException =>
        if(PrintErrors){
          println(Seperator)
          println(t.getMessage)
        }
        val errorCodes = TestUtils.parseErrorCodes(t.getMessage)
        assertCorrect(errorCodes, expectedErrors)
    }
  }
}
