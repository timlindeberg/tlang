package tcompiler

import java.io.File

import tcompiler.utils.{CompilationException, Context}

/**
 * Created by Tim Lindeberg on 4/2/2016.
 */
abstract class ErrorTester extends Tester {

  import TestUtils._

  def Seperator = "---------------------------------------------------------------------\n"

  def testFile(file: File): Unit = {
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
      assertCorrect(warningCodes, expectedErrors, warnings)
    } catch {
      case t: CompilationException =>
        if(PrintErrors){
          println(Seperator)
          println(t.getMessage)
        }
        val errorCodes = TestUtils.parseErrorCodes(t.getMessage)
        assertCorrect(errorCodes, expectedErrors, t.getMessage)
    }
  }
}
