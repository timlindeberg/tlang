package tcompiler

import java.io.File

import org.scalatest.FlatSpec
import tcompiler.analyzer.Types.{TUntyped, Typed}
import tcompiler.ast.ForeachTraverser
import tcompiler.ast.Trees.{Empty, CompilationUnit}
import tcompiler.lexer.Token
import tcompiler.utils.{Context, Reporter}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, _}
import scala.io.Source
import scala.sys.process.{ProcessLogger, _}

object TestUtils extends FlatSpec {

  val TestDirectory = "gen"
  val Resources      = "test/resources/"
  val SolutionPrefix = ".kool-solution"
  val Interpreter    = new Interpreter
  val Timeout        = duration.Duration(2, "sec")

  // regexes


  def test(file: File, testFunction: File => Unit): Unit = {
    if (file.isDirectory)
      programFiles(file.getPath).foreach(test(_, testFunction))
    else if (shouldBeIgnored(file))
      ignore should file.getName.toString in testFunction(file)
    else
      it should file.getName.toString in testFunction(file)
  }

  def getTestContext(file: File) = {
    val mainName = file.getName.replaceAll(Main.FileEnding, "")
    val outDir = getOutDir(mainName)
    val reporter = new Reporter(useColor = true)
    val cp = Main.TDirectory
    new Context(reporter = reporter, files = List(file), outDir = Some(outDir), classPaths = List(cp))
  }

  def executeTProgram(testFile: File): String = {
    val mainName = testFile.getName.replaceAll(Main.FileEnding, "")
    val outDir = getOutDir(mainName)
    executeTProgram(List(outDir.getAbsolutePath, Main.TDirectory), mainName)
  }

  def executeTProgram(classPaths: List[String], mainName: String): String = {
    val cp = classPaths.mkString(classPathSeperator)
    val f = Future(blocking(s"java -cp $cp $mainName " !!))
    try {
      Await.result(f, Timeout)
    } catch {
      case _: TimeoutException => fail("Test timed out!")
    }
  }

  private val classPathSeperator = if(System.getProperty("os.name").startsWith("Windows")) ";" else ":"

  def lines(str: String) = str.split("\\r?\\n").toList

  def programFiles(dir: String): Array[File] = {
    val f = new File(dir)
    if (!f.isDirectory)
      return Array[File](f)

    if (f.exists()) {
      f.listFiles.filter(x => !x.getName.contains(SolutionPrefix) && x.getName != ".DS_Store")
    } else {
      f.mkdir
      Array[File]()
    }
  }

  def format(token: Token): String = token + "(" + token.line + ":" + token.col + ")"

  def readOptions(file: File): Map[String, Any] = {
    val progString = Source.fromFile(file).getLines.toList
    var expectedErrors = 1
    var quietReporter = false
    val map = Map
    try {
      val options = progString.head.split(" ").tail.toList
      if (options.nonEmpty) {
        expectedErrors = options(0).toInt
        quietReporter = options.lift(1).getOrElse("false").toBoolean
      }
    } catch {
      case _: Throwable => expectedErrors = 1
    }
    map("expectedErrors" -> expectedErrors, "quietReporter" -> quietReporter)
  }

  private val SolutionRegex = """.*// *[R|r]es:(.*)""".r

  def parseSolutions(file: File): List[(Int, String)] = {
    val fileName = file.getPath
    Source.fromFile(fileName).getLines().zipWithIndex.flatMap {
      case (SolutionRegex(line), lineNumber)             =>
        line.split(",").map(res => (lineNumber + 1, res.trim))
      case _ => Nil
    }.toList
  }

  private val IgnoreRegex = """.*// *[I|i]gnore.*"""
  def shouldBeIgnored(file: File): Boolean = {
    val firstLine = Source.fromFile(file.getPath).getLines().take(1).toList.head
    firstLine.matches(IgnoreRegex)
  }

  private val ErrorRegex = """.*\.kool:(\d+):.+?\n(Fatal|Warning|Error) \((.+?)\).*""".r
  // Parses codes from error messages

  def parseErrorCodes(errorMessages: String): List[(Int, String)] = {
    // First two rows of error messages
    val errors = removeANSIFormatting(errorMessages).split("\n\n").map(msg => {
      msg.split("\n").take(2).mkString("\n")
    })
    errors.collect {
      case ErrorRegex(lineNumber, _, errorCode) => (lineNumber.toInt, errorCode)
    }.toList
  }

  def assertCorrect(res: List[(Int, String)], sol: List[(Int, String)], errors: String, checkLineNumbers: Boolean) = {
    def asString(l: List[(Int, String)]) = l.map { case (lineNumber, msg) =>
      val num = s"$lineNumber:"
      f"$num%-4s $msg"
    }
    val resStrings = asString(res)
    val solStrings = asString(sol)

    flattenTuple(res.zip(sol).zipWithIndex).foreach {
      case ((lineRes, r), (lineSol, s), i) =>
        val extraInfo = resultsVersusSolution(i + 1, resStrings, solStrings, errors)
        assert(r.trim == s.trim, s": error on line $lineRes $extraInfo")
        if(checkLineNumbers)
          assert(lineRes == lineSol, s": Same error message ($r) but the error happened on line $lineRes instead of $lineSol $extraInfo")
    }
    assert(res.length == sol.length, resultsVersusSolution(-1, resStrings, solStrings, errors))
  }

  def hasTypes(cu: CompilationUnit) = {
    var hasTypes = true
    val traverser = new ForeachTraverser({
      case _: Empty    =>
      case node: Typed =>
        if (node.getType == TUntyped)
          hasTypes = false
        assert(node.getType != TUntyped)
      case _ =>
    })
    traverser.traverse(cu)
    hasTypes
  }

  private def getOutDir(name: String) = new File(s"$TestDirectory/$name/")

  private def flattenTuple[A, B, C](t: List[((A, B), C)]): List[(A, B, C)] = t.map(x => (x._1._1, x._1._2, x._2))

  private def resultsVersusSolution(failedTest: Int, result: List[String], solution: List[String], errors: String) = {
    var res = result
    var sol = solution
    if (res.size < sol.size)
      res = res ::: List.fill(sol.size - res.size)("")
    else if (sol.size < res.size)
      sol = sol ::: List.fill(res.size - sol.size)("")

    val colLength = (result ::: solution).map(_.length).max + 2
    val numbers = (1 to res.size).map(_.toString)
    val numbered = flattenTuple(numbers.zip(res).zip(sol).toList)
    val list = ("", "Result:", "Solution:") :: numbered
    val results = list.map { case (i, r, s) =>
      val lineNum = "" + i
      val sizedStr = s"%-${colLength}s"
      val format = s"%-4s$sizedStr$sizedStr"
      val line = String.format(format, lineNum, r, s)
      if (i == failedTest.toString)  line + "<<<<<"
      else line
    }.mkString("\n", "\n", "\n")
    if(errors == "")
      results
    else
      results + "\n" + errors
  }

  private val AnsiRegex = """\x1b[^m]*m""".r
  private def removeANSIFormatting(s: String) = AnsiRegex.replaceAllIn(s, "")

  object IgnoreErrorOutput extends ProcessLogger {
    def buffer[T](f: ⇒ T): T = f
    def err(s: ⇒ String): Unit = {}
    def out(s: ⇒ String): Unit = {}
  }
}

