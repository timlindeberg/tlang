package tlang.testsuites

import better.files._
import org.scalatest.{BeforeAndAfterAll, Suites}
import tlang.compiler.ast.PrettyPrinterSpec
import tlang.compiler.{CompilerErrorsSuite, PositionSuite, ValidProgramsSuite}
import tlang.messages.MessageSnapshotSuite
import tlang.repl.ReplIntegrationSpec


class IntegrationTestSuite extends Suites(
  new CompilerErrorsSuite,
  new ValidProgramsSuite,
  new MessageSnapshotSuite,
  new PositionSuite,
  new PrettyPrinterSpec,
  new ReplIntegrationSpec
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
    if (!KeepFilesOnExit) File(TestOutputDirectory).delete()
  }

  override val suiteName: String = "Integration Tests"
}
