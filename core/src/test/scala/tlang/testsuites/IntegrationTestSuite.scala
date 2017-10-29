package tlang.testsuites

import better.files._
import org.scalatest.{BeforeAndAfterAll, Suites}
import tlang.compiler.ast.PrettyPrinterSpec
import tlang.compiler.{CompilerErrorsSuite, PositionSuite, ValidProgramsSuite}
import tlang.messages.MessageSnapshotSuite


class IntegrationTestSuite extends Suites(
  new CompilerErrorsSuite,
  new ValidProgramsSuite,
  new MessageSnapshotSuite,
  new PositionSuite,
  new PrettyPrinterSpec
) with BeforeAndAfterAll {

  import tlang.testutils.TestConstants._

  override def beforeAll: Unit = {
    val outDir = File(TestOutputDirectory)
    if (outDir.exists)
      outDir.delete()
  }

  override def afterAll: Unit = if (!KeepFilesOnExit) File(TestOutputDirectory).delete()

  override val suiteName: String = "Integration Tests"
}
