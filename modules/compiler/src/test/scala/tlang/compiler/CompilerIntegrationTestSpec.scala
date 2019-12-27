package tlang
package compiler

import better.files.File
import org.scalatest._
import tlang.filetester.CompilerFileTester
import tlang.testutils.{TestConstants, TestContext}
import tlang.utils.Source

import scala.collection.mutable
import scala.util.matching.Regex

object CompilerIntegrationTestSpec {

  private trait TestPath
  private case object Empty extends TestPath
  private case class TestDirectory(name: String, children: Array[TestPath]) extends TestPath
  private case class TestFile(file: File) extends TestPath

  private val IgnoredFiles: mutable.Map[File, Boolean] = mutable.Map[File, Boolean]()
  private val TestPaths: mutable.Map[String, TestPath] = mutable.Map[String, TestPath]()
  private val RootDirectory: String = File(".").pathAsString

  private val TestPattern: Option[Regex] =
    sys.env.get("pattern")
      .filter { _.nonEmpty }
      .map { _.replaceAll(" +", "").toLowerCase.r }
}

trait CompilerIntegrationTestSpec extends FreeSpec with Matchers with TestContext {

  import CompilerIntegrationTestSpec._
  import TestConstants._

  private val IgnoreRegex: Regex = """// *[I|i]gnore""".r

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

  def testFile(pipeline: CompilerPhase[Source, _], file: File): Unit = {
    val ctx = testContext(Some(file))
    val fileTester = CompilerFileTester(file, ctx, pipeline)
    val result = fileTester.execute()
    if (!result.success) {
      fail(result.description)
    }
  }

  private def shouldBeIncludedInTestName(directoryName: String): Boolean = directoryName(0).isLower

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
