package tlang
package compiler

import better.files.File
import org.scalatest.ParallelTestExecution
import tlang.compiler.execution.Compiler
import tlang.testutils.TestConstants._
import tlang.utils.Logging

class ValidProgramsSuite extends CompilerIntegrationTestSpec with ParallelTestExecution with Logging {

  override val suiteName: String = "Valid Programs"

  testFiles(s"$Resources/validtests", testValidProgram)

  def testValidProgram(file: File): Unit = {
    val ctx = testContext(Some(file))
    val fileTester = CompilerFileTester(file, ctx, Compiler.FrontEnd)
    val result = fileTester.execute()
    if (!result.success) {
      fail(result.message)
    }
  }
}
