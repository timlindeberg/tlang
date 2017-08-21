package tlang.testutils

import java.io.File

import org.scalatest._
import tlang.compiler.DebugOutputFormatter
import tlang.compiler.error.{CompilerMessages, DefaultReporter, MessageFormatter}
import tlang.compiler.imports.ClassPath
import tlang.formatting.BoxStyles.{Ascii, Unicode}
import tlang.formatting.grid.Grid
import tlang.formatting.{Formatter, Formatting}
import tlang.utils.Extensions._
import tlang.{Constants, Context}

import scala.concurrent.duration.Duration
import scala.util.matching.Regex

object CompilerTestTag extends Tag("compilertest")

object CompilerTestSpec {
  val Resources = "core/src/test/resources"
}

trait CompilerTestSpec extends FreeSpec with Matchers {

  val Timeout            : Duration       = Duration(2, "sec")
  val SolutionRegex      : Regex          = """.*// *[R|r]es:(.*)""".r
  val IgnoreRegex        : Regex          = """.*// *[I|i]gnore.*""".r
  val Resources          : String         = CompilerTestSpec.Resources
  val TestOutputDirectory: String         = "gen"
  val UseSimpleFormatting: Boolean        = sys.env.get("simple").contains("true")
  val UseColors          : Boolean        = sys.env.get("colors").contains("true")
  val PrintErrors        : Boolean        = sys.env.get("printerrors").contains("true")
  val PrintCodePhases    : Set[String]    = sys.env.get("printoutput").map(_.split(", *").map(_.trim).toSet).getOrElse(Set())
  val TestPattern        : Option[String] = sys.env.get("pattern")

  val TestFormatting = Formatting(if (UseSimpleFormatting) Ascii else Unicode, 80, useColor = UseColors, asciiOnly = UseSimpleFormatting)
  val TestFormatter  = Formatter(TestFormatting)

  def testContext(file: Option[File] = None): Context = {
    val (files, outDir) = file match {
      case Some(f) =>
        val mainName = f.getName.stripSuffix(Constants.FileEnding)
        val outDir = new File(s"$TestOutputDirectory/$mainName/")
        outDir.mkdirs()
        (Set(f), outDir)
      case None    => (Set[File](), new File("."))
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

  def testFiles(path: String, test: File => Unit): Unit = {
    def testFiles(file: File, depth: Int): Unit = {

      def subFiles(files: File): Unit = file.listFiles foreach { testFiles(_, depth + 1) }

      if (file.isDirectory) {
        depth match {
          case 0 => subFiles(file)
          case _ => file.getName - { subFiles(file) } // Nest test suite for sub folders
        }
        return
      }

      if (!file.getName.endsWith(Constants.FileEnding) || !matchesTestPattern(file))
        return

      val testName = file.getName.stripSuffix(Constants.FileEnding)
      if (shouldBeIgnored(file))
        testName taggedAs CompilerTestTag ignore {}
      else
        testName taggedAs CompilerTestTag in { test(file) }
    }
    testFiles(new File(path), 0)
  }


  def formatTestFailedMessage(failedTest: Int, result: List[String], solution: List[String]): String = {
    Grid(TestFormatter)
      .row(3)
      .content("", "Result", "Solution")
      .mapContent(result.zipAll(solution, "", "").zipWithIndex) { case ((res, sol), i) =>
        val n = i + 1
        var num = s"$n"
        if (n == failedTest)
          num += " " + TestFormatting.LeftArrow
        (num, res, sol)
      }
      .render()
  }

  // This is a work around to allow running for instance all errortests.flowing tests
  // since it seems IntelliJs test runner cannot handle regex matches
  private def matchesTestPattern(file: File) = TestPattern match {
    case Some(pattern) => file.getAbsolutePath
      .replaceAll("\\\\", "/")
      .toLowerCase
      .contains(pattern.toLowerCase)
    case None          => true
  }


  private def shouldBeIgnored(file: File): Boolean = {
    val firstLine = using(io.Source.fromFile(file.getPath)) { _.getLines().take(1).toList.headOption }
    if (firstLine.isEmpty)
      return true // Ignore empty files

    IgnoreRegex.findFirstIn(firstLine.get).isDefined
  }


}
