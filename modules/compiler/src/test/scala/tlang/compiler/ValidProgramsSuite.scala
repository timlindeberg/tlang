package tlang
package compiler

import better.files.File
import org.scalatest.ParallelTestExecution
import tlang.compiler.analyzer.Symbols.Symbolic
import tlang.compiler.analyzer.Types.Typed
import tlang.compiler.ast.TreePrinter
import tlang.compiler.ast.Trees.{CompilationUnit, Tree}
import tlang.compiler.execution.Compiler
import tlang.compiler.messages.CompilationException
import tlang.compiler.output.ErrorMessageOutput
import tlang.formatting.grid.{Column, TruncatedColumn}
import tlang.testutils.TestConstants
import tlang.testutils.TestConstants._
import tlang.utils.{DefaultMainMethodExecutor, FileSource, Logging}

import scala.runtime.ScalaRunTime

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
