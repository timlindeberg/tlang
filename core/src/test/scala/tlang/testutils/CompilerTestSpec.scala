package tlang.testutils

import better.files.File
import org.scalatest._
import tlang.compiler.DebugOutputFormatter
import tlang.compiler.imports.ClassPath
import tlang.formatting.{Formatter, Formatting}
import tlang.messages.{CompilerMessages, DefaultReporter, MessageFormatter}
import tlang.{Constants, Context}

import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.util.matching.Regex

object CompilerTestTag extends Tag("compilertest")


trait TestPath
case class TestDirectory(name: String, children: Array[TestPath]) extends TestPath
case class TestFile(file: File) extends TestPath
case object Empty extends TestPath

object CompilerTestSpec {

  val Resources                                   = "core/src/test/resources"
  val IgnoredFiles: mutable.Map[File, Boolean]    = mutable.Map[File, Boolean]()
  val TestPaths   : mutable.Map[String, TestPath] = mutable.Map[String, TestPath]()

}

trait CompilerTestSpec extends FreeSpec with Matchers {

  val Timeout            : Duration       = Duration(2, "sec")
  val SolutionRegex      : Regex          = """.*// *[R|r]es:(.*)""".r
  val IgnoreRegex        : Regex          = """// *[I|i]gnore""".r
  val Resources          : String         = CompilerTestSpec.Resources
  val TestOutputDirectory: String         = "gen"
  val AsciiOnly          : Boolean        = sys.env.get("ascii").contains("true")
  val UseColors          : Boolean        = sys.env.get("colors").contains("true")
  val PrintErrors        : Boolean        = sys.env.get("printerrors").contains("true")
  val PrintCodePhases    : Set[String]    = sys.env.get("printoutput").map(_.split(", *").map(_.trim).toSet).getOrElse(Set())
  val TestPattern        : Option[String] = sys.env.get("pattern").map(_.toLowerCase)

  val TestFormatting = Formatting(80, useColor = UseColors, asciiOnly = AsciiOnly)
  val TestFormatter  = Formatter(TestFormatting)

  def testContext(file: Option[File] = None): Context = {
    val (files, outDir) = file match {
      case Some(f) =>
        val mainName = f.nameWithoutExtension
        val outDir = File(s"$TestOutputDirectory/$mainName/")
        outDir.createDirectories()
        (Set(f), outDir)
      case None    => (Set[File](), File("."))
    }

    val errorFormatter = MessageFormatter(TestFormatter)
    val errorMessages = CompilerMessages(TestFormatter, errorFormatter)
    val debugOutputFormatter = DebugOutputFormatter(TestFormatter)
    Context(
      reporter = DefaultReporter(errorMessages),
      debugOutputFormatter = debugOutputFormatter,
      files = files,
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
          val test = testName taggedAs CompilerTestTag
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
  // much of the calculation as possible (at the time of writing around 600 times).
  private def getTestPath(path: String): TestPath = {
    CompilerTestSpec.TestPaths.getOrElseUpdate(path, {
      def testPaths(file: File): TestPath = {
        if (!matchesTestPattern(file.pathAsString))
          return Empty

        if (file.isDirectory)
          return TestDirectory(file.name, file.listRecursively.map(testPaths).filter(_ != Empty).toArray)

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
      val formattedPath = path.dropWhile(!_.isUpper).replaceAll("\\\\", "/").toLowerCase
      val (longest, shortest) = if (pattern.length > formattedPath.length) (pattern, formattedPath) else (formattedPath, pattern)
      longest startsWith shortest
    case None          => true
  }

  // Cached so we don't have to read the value multiple times
  private def shouldBeIgnored(file: File): Boolean =
    CompilerTestSpec.IgnoredFiles.getOrElseUpdate(file, {
      val firstLine = file.lineIterator.take(1).toList.headOption
      // Ignore empty files
      firstLine.isEmpty || IgnoreRegex.findFirstIn(firstLine.get).isDefined
    })


}
