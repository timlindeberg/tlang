package tlang
package testsuites

import better.files._
import org.scalatest.{BeforeAndAfterAll, Suites}
import tlang.compiler.analyzer.OperatorTypeSpec
import tlang.compiler.ast.PrettyPrinterSpec
import tlang.compiler.{CompilerErrorsSuite, PositionSuite, ValidProgramsSuite}
import tlang.repl.ReplIntegrationSpec

class IntegrationTestSuite extends Suites(
  new CompilerErrorsSuite,
  new ValidProgramsSuite,
  new PositionSuite,
  new PrettyPrinterSpec,
  new ReplIntegrationSpec,
  new OperatorTypeSpec
) with BeforeAndAfterAll {

  import tlang.testutils.TestConstants._

  override def beforeAll: Unit = {
    super.beforeAll()
    val outDir = File(TestOutputDirectory)
    if (outDir.exists)
      outDir.delete()
  }

  override def afterAll: Unit = {
    super.afterAll()
    if (!KeepFilesOnExit) File(TestOutputDirectory).delete(swallowIOExceptions = true)
  }

  override val suiteName: String = "Integration Tests"
}
