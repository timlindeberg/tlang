package tlang
package compiler

import better.files.File
import org.scalatest.ParallelTestExecution
import tlang.compiler.execution.Compiler
import tlang.filetester.CompilerFileTester
import tlang.testutils.TestConstants._
import tlang.utils.Logging

class ValidProgramsSuite extends CompilerIntegrationTestSpec with ParallelTestExecution with Logging {

  override val suiteName: String = "Valid Programs"

  testFiles(s"$Resources/validtests", testValidProgram)

  private def testValidProgram(file: File): Unit = testFile(Compiler.FrontEnd, file)
}
