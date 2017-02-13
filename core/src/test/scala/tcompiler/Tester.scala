package tcompiler

import java.io.File

import org.scalatest.{BeforeAndAfter, FunSuite, Matchers}
import tcompiler.Flags.LineWidth
import tcompiler.ast.PrettyPrinter
import tcompiler.ast.Trees.CompilationUnit
import tcompiler.error.Boxes.Light
import tcompiler.error.{DefaultReporter, Formatting, Reporter, VoidReporter}
import tcompiler.imports.ClassSymbolLocator
import tcompiler.utils.{Colors, Context, Pipeline}

import scala.concurrent._
import scala.io.Source
import scala.util.matching.Regex

/**
  * Created by Tim Lindeberg on 4/11/2016.
  */
trait Tester extends FunSuite with Matchers with BeforeAndAfter {

  import Tester._

  def Name: String
  def Path: String
  def Pipeline: Pipeline[Set[File], List[CompilationUnit]]

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
          throw new Exception(s"No such file: $f")
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
    execute {testFile(file)}
  }

  private def testFiles(file: File): Array[File] = {
    if (file.isFile && file.getName.endsWith(Main.FileEnding))
      return Array(file)

    val files = file.listFiles
    if (files == null || files.isEmpty)
      Array()
    else
      files.flatMap(testFiles)
  }

  protected def formatTestFailedMessage(failedTest: Int, result: List[String], solution: List[String], errors: String = ""): String = {
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
    val results = list.map { case (i, r, s) =>
      val lineNum = s"$i"
      val sizedStr = s"%-${colLength}s"
      val format = s"%-4s$sizedStr$sizedStr"
      val line = String.format(format, lineNum, r, s)
      if (i == failedLine)
        s"$line <<<<<"
      else
        line
    }.mkString("\n", "\n", "\n")

    if (errors == "") results else results + "\n" + errors
  }

  private def shouldBeIgnored(file: File): Boolean = {
    val firstLine = Source.fromFile(file.getPath).getLines().take(1).toList.headOption
    if (firstLine.isEmpty)
      return true

    IgnoreRegex.findFirstIn(firstLine.get).isDefined
  }

  private def flattenTuples[A, B, C](t: List[((A, B), C)]): List[(A, B, C)] = t.map(x => (x._1._1, x._1._2, x._2))
}

object Tester {

  val TestDirectory                = "gen"
  val Resources                    = "core/src/test/resources/"
  val Timeout                      = duration.Duration(2, "sec")
  val IgnoreRegex    : Regex       = """.*// *[I|i]gnore.*""".r
  val SolutionRegex  : Regex       = """.*// *[R|r]es:(.*)""".r
  val UseColor       : Boolean     = !sys.env.get("usecolor").contains("false")
  val PrintCodeStages: Set[String] = sys.env.get("printoutput").map(_.split(",").toSet).getOrElse(Set())

  def testContext: Context = getTestContext(None, Some(VoidReporter()))

  def getTestContext(
    file: Option[File],
    reporter: Option[Reporter] = None,
    formatting: Option[Formatting] = None
  ): Context = {

    val (files, outDir) = file match {
      case Some(f) =>
        val mainName = f.getName.replaceAll(Main.FileEnding, "")
        (Set(f), Set(getOutDir(mainName)))
      case None    => (Set[File](), Set(new File(".")))
    }

    val colors = Colors(UseColor)
    val f = formatting.getOrElse(Formatting(Light, LineWidth.defaultValue, colors))
    val r = reporter.getOrElse(DefaultReporter(formatting = f))
    Context(
      reporter = r,
      files = files,
      outDirs = outDir,
      printCodeStages = PrintCodeStages,
      formatting = f,
      printer = PrettyPrinter(colors))
  }

  private def getOutDir(name: String) = new File(s"$TestDirectory/$name/")

}