package tcompiler

import java.io.File

import tcompiler.utils.CompilationException

/**
 * Created by Tim Lindeberg on 4/2/2016.
 */
abstract class ErrorTester extends Tester {

  import TestUtils._

  def Seperator = "---------------------------------------------------------------------\n"

  def testFile(file: File): Unit = {
    val ctx = getTestContext(file)
    val expectedErrors = TestUtils.parseSolutions(file)

    try {
      Pipeline.run(ctx)(List(file))

      // Check for warnings:
      if(ctx.reporter.warnings.isEmpty)
        fail("Test failed: No errors or warnings!")

      val warnings = ctx.reporter.warningsString
      if(PrintErrors){
        println(Seperator)
        println(warnings)
      }

      val warningCodes = TestUtils.parseErrorCodes(warnings)
      assertCorrect(warningCodes, expectedErrors, warnings, checkLineNumbers = true)
    } catch {
      case t: CompilationException =>
        if(PrintErrors){
          println(Seperator)
          println(t.getMessage)
        }
        val errorCodes = TestUtils.parseErrorCodes(t.getMessage)
        assertCorrect(errorCodes, expectedErrors, t.getMessage, checkLineNumbers = true)
      case e: Exception =>
        e.printStackTrace()
        fail(e.getMessage)
    }
  }
}
