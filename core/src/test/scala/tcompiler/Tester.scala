package tcompiler

import java.io.File

import org.scalatest.{BeforeAndAfter, FunSuite, Matchers}
import tcompiler.ast.PrettyPrinter
import tcompiler.ast.Trees.CompilationUnit
import tcompiler.error.Reporter
import tcompiler.imports.ClassSymbolLocator
import tcompiler.utils.{Colorizer, Context, Pipeline}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, _}
import scala.io.Source
import scala.sys.process._
import scala.util.matching.Regex

/**
  * Created by Tim Lindeberg on 4/11/2016.
  */
trait Tester extends FunSuite with Matchers with BeforeAndAfter {

  import Tester._

  def Name: String
  def Path: String
  def Pipeline: Pipeline[List[File], List[CompilationUnit]]

  def testFile(file: File): Unit


  protected val PrintErrors: Boolean = sys.env.get("printerrors").contains("true")

  private val testPath = sys.env.get("testfile")
    .map(file => s"$Path/$file${Main.FileEnding}")
    .getOrElse(Path)


  testFiles(testPath).foreach(test)

  before {
    ClassSymbolLocator.clearCache()
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

  private def test(file: File): Unit = {
    if (file.isDirectory)
      testFiles(file.getPath).foreach(test)
    else if (shouldBeIgnored(file))
      ignore(file.getName) {testFile(file)}
    else
      test(file.getName) {testFile(file)}
  }

  private def testFiles(dir: String): Array[File] = {
    val f = new File(dir)
    if (!f.isDirectory)
      return Array[File](f)

    if (f.exists())
      return f.listFiles.filter(_.getName.endsWith(Main.FileEnding))

    f.mkdir
    Array[File]()
  }

  private def shouldBeIgnored(file: File): Boolean = {
    val firstLine = Source.fromFile(file.getPath).getLines().take(1).toList.head
    IgnoreRegex.findFirstIn(firstLine).isDefined
  }

  private def flattenTuples[A, B, C](t: List[((A, B), C)]): List[(A, B, C)] = t.map(x => (x._1._1, x._1._2, x._2))
}

object Tester {

  val TestDirectory                 = "gen"
  val Resources                     = "core/src/test/resources/"
  val Timeout                       = duration.Duration(2, "sec")
  val IgnoreRegex    : Regex        = """.*// *[I|i]gnore.*""".r
  val SolutionRegex  : Regex        = """.*// *[R|r]es:(.*)""".r
  val UseColor       : Boolean      = sys.env.get("usecolor").contains("true")
  val PrintCodeStages: List[String] = sys.env.get("printcode").map(_.split(",").toList).getOrElse(Nil)

  def testContext: Context = getTestContext(None)

  def getTestContext(file: File): Context = getTestContext(Some(file))
  def getTestContext(file: Option[File]): Context = {
    val (files, outDir) = file match {
      case Some(f) =>
        val mainName = f.getName.replaceAll(Main.FileEnding, "")
        (List(f), List(getOutDir(mainName)))
      case None    => (Nil, List(new File(".")))
    }

    val colorizer = new Colorizer(UseColor)
    val reporter = new Reporter(colorizer = colorizer)
    val cp = Main.TDirectory
    Context(
      reporter = reporter,
      files = files,
      outDirs = outDir,
      classPaths = List(cp),
      printCodeStages = PrintCodeStages,
      colorizer = colorizer,
      printer = new PrettyPrinter(colorizer))
  }


  def executeTProgram(classPaths: List[String], mainName: String): Option[String] = {
    val cp = formatClassPath(classPaths)
    val command = s"java -cp $cp $mainName"
    val future = Future(blocking(command !!))
    try {
      Some(Await.result(future, Timeout))
    } catch {
      case _: TimeoutException => None
    }
  }

  private def getOutDir(name: String) = new File(s"$TestDirectory/$name/")

  private def formatClassPath(classPaths: List[String]): String =
    if (System.getProperty("os.name").startsWith("Windows"))
      "\"" + classPaths.mkString(";") + "\""
    else
      classPaths.mkString(":")

}