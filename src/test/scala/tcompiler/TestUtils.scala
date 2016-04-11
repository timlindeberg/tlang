package tcompiler

import java.io.File

import org.scalatest.FlatSpec
import tcompiler.analyzer.Types._
import tcompiler.ast.Trees
import tcompiler.ast.Trees._
import tcompiler.lexer.Token

import scala.io.Source

import scala.sys.process.{ProcessLogger, _}

object TestUtils extends FlatSpec {

  val Resources      = "./src/test/resources/"
  val SolutionPrefix = ".kool-solution"
  val Interpreter    = new Interpreter

  def test(file: File, testFunction: File => Unit): Unit = {
    if (file.isDirectory) {
      programFiles(file.getPath).foreach(test(_, testFunction))
    } else {
      if (shouldBeIgnored(file))
        ignore should file.getName.toString in testFunction(file)
      else
        it should file.getName.toString in testFunction(file)
    }
  }

  def executeTProgram(f: File, prefix: String): String =
    executeTProgram(prefix + f.getName, f.getName.dropRight(Main.FileEnding.length))

  def executeTProgram(classPath: String, mainName: String): String =
    s"java -cp $classPath $mainName" !!

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


  def parseSolutions(file: File): List[(Int, String)] = {
    val SolutionRegex = """.*// *[R|r]es:(.*)""".r
    val SolutionOrderedRegex = """.*// *[R|r]es(\d+):(.*)""".r

    val fileName = file.getPath
    var i = -1
    val answers = Source.fromFile(fileName).getLines().zipWithIndex.map {
      case (SolutionOrderedRegex(num, result), line) => (num.toInt, (line + 1, result))
      case (SolutionRegex(result), line)             =>
        i += 1
        (i, (line + 1, result))
      case _                                         => (-1, (0, ""))
    }.toList
    answers.filter(_._1 >= 0).sortWith(_._1 < _._1).map(_._2)
  }

  def shouldBeIgnored(file: File): Boolean = {
    val firstLine = Source.fromFile(file.getPath).getLines().take(1).toList.head
    firstLine.matches(""".*// *[I|i]gnore.*""")
  }

  // Parses codes from error messages
  def parseErrorCodes(errorMessages: String) = {
    val ErrorRegex = """(Fatal|Warning|Error) \((.+?)\).*""".r

    removeANSIFormatting(errorMessages).split("\n\n\n").map(_.split("\n")(1)).collect {
      case ErrorRegex(_, errorCode) => errorCode
    }.toList
  }

  def assertCorrect(res: List[String], sol: List[(Int, String)]) = {
    assert(res.length == sol.length, resultsVersusSolution(-1, res, sol.map(_._2)))

    flattenTuple(res.zip(sol).zipWithIndex).foreach {
      case (r, (line, s), i) =>
        assert(r.trim == s.trim, s": error on line $line ${resultsVersusSolution(i + 1, res, sol.map(_._2))}")
    }
  }

  private def flattenTuple[A, B, C](t: List[((A, B), C)]): List[(A, B, C)] = t.map(x => (x._1._1, x._1._2, x._2))

  private def resultsVersusSolution(failedTest: Int, result: List[String], solution: List[String]) = {
    var res = result
    var sol = solution
    if(res.size < sol.size)
      res = res ::: List.fill(sol.size - res.size)("")
    else if(sol.size < res.size)
      sol = sol ::: List.fill(res.size - sol.size)("")

    val colLength = (result ::: solution).map(_.length).max + 2
    val numbers = (1 to res.size).map(_.toString)
    val numbered = flattenTuple(numbers.zip(res).zip(sol).toList)
    val list = ("", "Result:", "Solution:") :: numbered
    list.map { case (i, r, s) =>
      val lineNum = "" + i
      val size = s"%-${colLength}s"
      val format = s"%-4s$size$size"
      val line = String.format(format, lineNum, r, s)
      if (i == failedTest.toString) Console.UNDERLINED + line + Console.RESET
      else line
    }.mkString("\n", "\n", "\n")
  }

  def hasTypes(prog: Program) = {
    var hasTypes = true
    Trees.traverse(prog, (_, curr) => Some(curr) collect {
      case _: Empty    =>
      case node: Typed =>
        if (node.getType == TUntyped)
          hasTypes = false
        assert(node.getType != TUntyped)

    })
    hasTypes && correctTypes(prog)
  }

  private def removeANSIFormatting(s: String) = """\x1b[^m]*m""".r.replaceAllIn(s, "")


  private def correctTypes(t: Tree): Boolean = {
    var types = true
    Trees.traverse(t, (_, x) => {
      types = x match {
        case x: IntLit          => x.getType == TInt
        case x: StringLit       => x.getType == TString
        case x: Identifier      => x.getType != TUntyped && x.getType != TError
        case x: ClassIdentifier => x.getType != TUntyped && x.getType != TError
        case x: IntType         => x.getType == TInt
        case x: ArrayType       => x.getType == tArray
        case x: BooleanType     => x.getType == TBool
        case x: StringType      => x.getType == TString
        case x: True            => x.getType == TBool
        case x: False           => x.getType == TBool
        case x: This            => x.getType != TUntyped && x.getType != TError
        case _                  => true
      }
    })
    types
  }

  object IgnoreErrorOutput extends ProcessLogger {
    def buffer[T](f: ⇒ T): T = f
    def err(s: ⇒ String): Unit = {}
    def out(s: ⇒ String): Unit = {}
  }
}

