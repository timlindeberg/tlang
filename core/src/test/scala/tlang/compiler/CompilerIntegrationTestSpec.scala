package tlang.compiler

import better.files.File
import org.scalatest._
import tlang.compiler.imports.ClassPath
import tlang.formatting.textformatters.TabReplacer
import tlang.messages.{CompilerMessages, DefaultReporter, MessageFormatter}
import tlang.{Constants, Context}

import scala.collection.mutable


trait TestPath
case class TestDirectory(name: String, children: Array[TestPath]) extends TestPath
case class TestFile(file: File) extends TestPath
case object Empty extends TestPath

object CompilerIntegrationTestSpec {

  val IgnoredFiles : mutable.Map[File, Boolean]    = mutable.Map[File, Boolean]()
  val TestPaths    : mutable.Map[String, TestPath] = mutable.Map[String, TestPath]()
  val RootDirectory: String                        = File(".").pathAsString

}

trait CompilerIntegrationTestSpec extends FreeSpec with Matchers {

  import CompilerIntegrationTestSpec._
  import tlang.testsuites.CompilerIntegrationTestSuite._

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
        case TestDirectory(name, children) => name - { children foreach testPath }
        case TestFile(file)                =>
          val testName = file.nameWithoutExtension
          val test = testName taggedAs CompilerIntegrationTestTag
          if (shouldBeIgnored(file)) test ignore {} else test in { executeTest(file) }
        case Empty                         =>
      }
    }

    testPath(getTestPath(path))
  }


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

  // Since ParallellTestExecution instantiates the Spec for EACH test we try to cache as
  // much of the calculation as possible.
  private def getTestPath(path: String): TestPath = {
    TestPaths.getOrElseUpdate(path, {
      def testPaths(file: File): TestPath = {
        if (!matchesTestPattern(file.pathAsString))
          return Empty

        if (file.isDirectory)
          return TestDirectory(file.name, file.children.map(testPaths).filter(_ != Empty).toArray)

        if (!file.extension.contains(Constants.FileEnding))
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
      val formattedPath = path.stripPrefix(RootDirectory).dropWhile(!_.isUpper).replaceAll("\\\\", "/").toLowerCase
      val (longest, shortest) = if (pattern.length > formattedPath.length) (pattern, formattedPath) else (formattedPath, pattern)
      longest startsWith shortest
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
