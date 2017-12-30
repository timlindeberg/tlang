package tlang.compiler

import better.files.File
import org.scalatest._
import tlang.Constants
import tlang.compiler.imports.ClassPath
import tlang.compiler.messages.{CompilerMessages, DefaultReporter, MessageFormatter}
import tlang.compiler.utils.DebugOutputFormatter
import tlang.formatting.textformatters.TabReplacer
import tlang.testutils.TestConstants
import tlang.utils.Extensions._

import scala.collection.mutable
import scala.util.matching.Regex


trait TestPath
case class TestDirectory(name: String, children: Array[TestPath]) extends TestPath
case class TestFile(file: File) extends TestPath
case object Empty extends TestPath

object CompilerIntegrationTestSpec {

  val IgnoredFiles : mutable.Map[File, Boolean]    = mutable.Map[File, Boolean]()
  val TestPaths    : mutable.Map[String, TestPath] = mutable.Map[String, TestPath]()
  val RootDirectory: String                        = File(".").pathAsString
  val TestPattern  : Option[Regex]                 =
    sys.env.get("pattern")
      .filter { _.nonEmpty }
      .map { _.replaceAll(" +", "").toLowerCase.r }

}

trait CompilerIntegrationTestSpec extends FreeSpec with Matchers {

  import CompilerIntegrationTestSpec._
  import TestConstants._

  def testContext(file: Option[File] = None): Context = {
    val outDir = file match {
      case Some(f) =>
        val resourceDir = File(Resources)
        val mainName = f.pathAsString.stripPrefix(resourceDir.pathAsString).stripSuffix(Constants.FileEnding)
        File(s"$TestOutputDirectory/$mainName/")
      case None    => File(".")
    }

    val errorFormatter = MessageFormatter(TestFormatter, TabReplacer(2))
    val errorMessages = CompilerMessages(TestFormatter, errorFormatter)
    val debugOutputFormatter = DebugOutputFormatter(TestFormatter)
    Context(
      reporter = DefaultReporter(errorMessages),
      debugOutputFormatter = debugOutputFormatter,
      outDirs = Set(outDir),
      classPath = ClassPath.Default,
      printCodePhase = PrintCodePhases,
      formatter = TestFormatter
    )
  }

  def testFiles(path: String, executeTest: File => Unit): Unit = {
    def testPath(path: TestPath): Unit = {
      path match {
        case TestDirectory(name, children) =>
          if (shouldBeIncludedInTestName(name))
            children foreach testPath
          else
            name - { children foreach testPath }
        case TestFile(file)                =>
          val testName = file.nameWithoutExtension
          val test = testName taggedAs CompilerIntegrationTestTag
          if (shouldBeIgnored(file)) test ignore {} else test in { executeTest(file) }
        case Empty                         =>
      }
    }

    testPath(getTestPath(path))
  }

  private def shouldBeIncludedInTestName(directoryName: String): Boolean = directoryName(0).isLower

  def formatTestFailedMessage(failedTest: Int, result: List[String], solution: List[String]): String = {
    val numbers = (1 to Math.max(result.length, solution.length)).map { i =>
      if (i == failedTest) s"$i" + " " + TestFormatting.LeftArrow else s"$i"
    }
    TestFormatter
      .grid
      .row(3)
      .content("", "Result", "Solution")
      .mapContent(result.zipAll(solution, "", "").zip(numbers)) { case ((res, sol), num) => (num, res, sol) }
      .render()
  }

  def printExecutionTimes(file: File, ctx: Context): Unit = {
    import TestFormatter._
    import TestFormatting._

    grid.header(s"Testing file ${ Magenta(file.nameWithoutExtension) }").print()
    ctx.printExecutionTimes(success = true)
  }

  // Since ParallellTestExecution instantiates the Spec for EACH test we try to cache as
  // much of the calculation as possible.
  private def getTestPath(path: String): TestPath = {
    TestPaths.getOrElseUpdate(path, {
      def testPaths(file: File): TestPath = {
        if (file.isDirectory)
          return TestDirectory(file.name, file.children.map(testPaths).filter(_ != Empty).toArray)

        if (!file.extension.contains(Constants.FileEnding))
          return Empty

        if (!matchesTestPattern(file.pathAsString))
          return Empty

        TestFile(file)
      }

      testPaths(File(path))
    })
  }

  // This is a work around to allow running for instance all flowing tests
  // since it seems IntelliJs test runner cannot handle regex matches
  private def matchesTestPattern(path: String) = TestPattern match {
    case Some(pattern) =>
      val formattedPath =
        path.stripPrefix(RootDirectory)
          .dropWhile(!_.isUpper)
          .replaceAll(" +", "")
          .stripSuffix(Constants.FileEnding)
          .replaceAll("\\\\", "/")
          .toLowerCase
      pattern matches formattedPath
    case None          => true
  }

  // Cached so we don't have to read the value multiple times
  private def shouldBeIgnored(file: File): Boolean =
    IgnoredFiles.getOrElseUpdate(file, {
      val firstLine = file.lineIterator.take(1).toList.headOption
      // Ignore empty files
      firstLine.isEmpty || IgnoreRegex.findFirstIn(firstLine.get).isDefined
    })

}
