package tcompiler

import java.io.File

import org.scalatest.FlatSpec
import tcompiler.error.Reporter
import tcompiler.utils.Context

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, _}
import scala.io.Source
import scala.sys.process._
import scala.util.matching.Regex

object TestUtils extends FlatSpec {

  val TestDirectory        = "gen"
  val Resources            = "core/src/test/resources/"
  val Interpreter          = new Interpreter
  val Timeout              = duration.Duration(2, "sec")
  val SolutionRegex: Regex = """.*// *[R|r]es:(.*)""".r

  def test(file: File, testFunction: File => Unit): Unit = {
    if (file.isDirectory)
      programFiles(file.getPath).foreach(test(_, testFunction))
    else if (shouldBeIgnored(file))
      ignore should file.getName in testFunction(file)
    else
      it should file.getName in testFunction(file)
  }

  def testContext: Context = getTestContext(None)

  def getTestContext(file: File): Context = getTestContext(Some(file))
  def getTestContext(file: Option[File]): Context = {
    val (files, outDir) = file match {
      case Some(f) =>
        val mainName = f.getName.replaceAll(Main.FileEnding, "")
        (List(f), List(getOutDir(mainName)))
      case None    => (Nil, List(new File(".")))
    }

    val reporter = new Reporter(useColor = true)
    val cp = Main.TDirectory
    val printCodeStage = sys.env.get("printcode").map(_.split(",").toList).getOrElse(Nil)
    Context(reporter = reporter, files = files, outDirs = outDir, classPaths = List(cp), printCodeStages = printCodeStage, useColor = true)
  }

  def executeTProgram(testFile: File): String = {
    val mainName = testFile.getName.replaceAll(Main.FileEnding, "")
    val outDir = getOutDir(mainName)
    executeTProgram(List(outDir.getAbsolutePath, Main.TDirectory), mainName)
  }

  def executeTProgram(classPaths: List[String], mainName: String): String = {
    val cp = formatClassPath(classPaths)
    val command = s"java -cp $cp $mainName"
    val future = Future(blocking(command !!))
    try {
      Await.result(future, Timeout)
    } catch {
      case _: TimeoutException => fail(s"Test timed out after $Timeout s.")
    }
  }

  def formatClassPath(classPaths: List[String]): String =
    if (System.getProperty("os.name").startsWith("Windows"))
      "\"" + classPaths.mkString(";") + "\""
    else
      classPaths.mkString(":")


  def lines(str: String): List[String] = str.split("\\r?\\n").map(_.trim).toList

  def programFiles(dir: String): Array[File] = {
    val f = new File(dir)
    if (!f.isDirectory)
      return Array[File](f)

    if (f.exists()) {
      f.listFiles.filter(_.getName != ".DS_Store")
    } else {
      f.mkdir
      Array[File]()
    }
  }

  def formatTestFailedMessage(failedTest: Int, result: List[String], solution: List[String], errors: String = ""): String = {
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

  private val IgnoreRegex = """.*// *[I|i]gnore.*"""
  def shouldBeIgnored(file: File): Boolean = {
    val firstLine = Source.fromFile(file.getPath).getLines().take(1).toList.head
    firstLine.matches(IgnoreRegex)
  }

  private def getOutDir(name: String) = new File(s"$TestDirectory/$name/")

  private def flattenTuples[A, B, C](t: List[((A, B), C)]): List[(A, B, C)] = t.map(x => (x._1._1, x._1._2, x._2))

}

