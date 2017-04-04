package tlang.compiler

import java.io.File

import org.scalatest.{BeforeAndAfter, FunSuite, Matchers}
import tlang.compiler.ast.Trees.CompilationUnit
import tlang.compiler.error._
import tlang.compiler.imports.ClassSymbolLocator
import tlang.utils.Extensions._
import tlang.utils.Source

import scala.concurrent._
import scala.util.matching.Regex

object Tester {

  val TestDirectory                    = "gen"
  val Resources                        = "core/src/test/resources/"
  val Timeout                          = duration.Duration(2, "sec")
  val IgnoreRegex        : Regex       = """.*// *[I|i]gnore.*""".r
  val SolutionRegex      : Regex       = """.*// *[R|r]es:(.*)""".r
  val UseSimpleFormatting: Boolean     = sys.env.get("simple").contains("true")
  val PrintCodeStages    : Set[String] = sys.env.get("printoutput").map(_.split(",").toSet).getOrElse(Set())

  def testContext: Context = getTestContext(None, Some(VoidReporter()))

  def getTestContext(file: Option[File], reporter: Option[Reporter] = None): Context = {
    val (files, outDir) = file match {
      case Some(f) =>
        val mainName = f.getName.replaceAll("\\" + Main.FileEnding, "")
        (Set(f), new File(s"$TestDirectory/$mainName/"))
      case None    => (Set[File](), new File("."))
    }

    val formatting = if (UseSimpleFormatting) SimpleFormatting else FancyFormatting
    Context(
      reporter = reporter.getOrElse(DefaultReporter(formatting = formatting)),
      files = files,
      outDirs = Set(outDir),
      printCodeStages = PrintCodeStages,
      formatting = formatting
    )
  }

}

trait Tester extends FunSuite with Matchers with BeforeAndAfter {

  import Tester._

  def Name: String
  def Path: String
  def Pipeline: Pipeline[Source, CompilationUnit]

  def testFile(file: File): Unit


  protected val PrintErrors: Boolean = sys.env.get("printerrors").contains("true")

  private val testPath = sys.env.get("testfile")
    .filter(_.nonEmpty)
    .map { f =>
      val path = s"$Path/$f"
      val file = new File(path)
      if (file.isDirectory || file.exists()) {
        path
      } else {
        val withEnding = path + Main.FileEnding
        if (new File(withEnding).exists())
          withEnding
        else
          fail(s"No such file: $f")
      }
    }
    .getOrElse(Path)


  before {
    ClassSymbolLocator.clearCache()
  }

  testFiles(new File(testPath)).foreach { file =>
    val name = file.getPath
      .replaceAll("\\\\", "/")
      .replaceAll(Path + "/", "")
      .replaceAll("\\" + Main.FileEnding, "")

    val execute = if (shouldBeIgnored(file)) ignore(name)(_) else test(name)(_)
    execute { testFile(file) }
  }

  private def testFiles(file: File): Array[File] = {
    if (file.isFile && file.getName.endsWith(Main.FileEnding))
      return Array(file)

    file.listFiles.flatMap(testFiles)
  }

  protected def formatTestFailedMessage(failedTest: Int, result: List[String], solution: List[String]): String = {
    var res = result
    var sol = solution
    if (res.size < sol.size)
      res = res ::: List.fill(sol.size - res.size)("")
    else if (sol.size < res.size)
      sol = sol ::: List.fill(res.size - sol.size)("")

    var colLength = (result ::: solution).map(_.length).max + 2
    colLength = Math.max(colLength, "Solution:".length)
    val numbers = (1 to res.size).map(_.toString)
    val numbered = flattenTuples(numbers.zip(res).zip(sol).toList)
    val list = ("", "Result:", "Solution:") :: numbered

    val failedLine = failedTest.toString
    list.map { case (i, r, s) =>
      val lineNum = s"$i"
      val sizedStr = s"%-${ colLength }s"
      val format = s"%-4s$sizedStr$sizedStr"
      val line = String.format(format, lineNum, r, s)

      if (i == failedLine) s"$line <<<<<" else line
    }.mkString("\n", "\n", "\n")
  }

  private def shouldBeIgnored(file: File): Boolean = {
    val firstLine = using(io.Source.fromFile(file.getPath)) { _.getLines().take(1).toList.headOption }
    if (firstLine.isEmpty)
      return true

    IgnoreRegex.findFirstIn(firstLine.get).isDefined
  }

  private def flattenTuples[A, B, C](t: List[((A, B), C)]): List[(A, B, C)] = t.map(x => (x._1._1, x._1._2, x._2))
}