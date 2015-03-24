package koolc.ast

import org.scalatest._
import java.io.File
import koolc.utils._
import scala.io.Source
import koolc.ast._
import koolc.ast.Trees._
import koolc.lexer._
import scala.collection.JavaConversions._
import java.io._

class ParserSpec extends FlatSpec with Matchers {

  def files(dir: String) = new File(dir).listFiles.filter(_.toString.endsWith(".kool"))
  
  val testResource = "./src/test/resources/ast/"
  val valid = files(testResource + "valid/")
  val invalid = files(testResource + "invalid/")
  val operator = new File(testResource + "operatortest.kool")
  val operatorSolutions = new File(testResource + "operatortest-solution.kool")

  // Test cases
  
  valid.zipWithIndex.foreach {
    case (file, i) =>
      it should "parse valid program" + (i + 1) in testValidProgram(file)
  }

  invalid.zipWithIndex.foreach {
    case (file, i) =>
      it should "not parse invalid program" + (i + 1) in testInvalidProgram(file)
  }

  testOperator(operator, operatorSolutions)

  // Helper functions

  def parseCtx(ctx: Context, p: String): Program = {
    val tokens = Lexer.run(p.toList, ctx.file)
    Parser.run(ctx)(tokens)
  }

  def print(p: Program) = Printer(p)

  def testValidProgram(file: File) = {
    val (parse, p) = program(file)
    assert(print(parse(p)) === print(parse(print(parse(p)))))
  }

  def testInvalidProgram(file: File) = {
    val (parse, p) = program(file)
    intercept[ParsingException] {
      (parse(p))
    }
  }

  def testOperator(file: File, solution: File) : Unit = {
    val sol = Source.fromFile(solution).getLines.toList.map { _.replace(" ", "") }
    val (parse, expr) = program(file)
    expr.split("\n").zip(sol).zipWithIndex.foreach { case ((a,b), i) => 
      it should "have correct operator precedence " + (i + 1) in testOperator(parse, a, b)
    }
  }
  
  def testOperator(parse: (String) => Program, expr: String, sol: String) : Unit = {
    val p = addDefaultProgram(expr)
    assert(extractExpression(parse(p).toString) === sol)  
  }

  def program(file: File): ((String) => Program, String) = {
    val ctx = new Context(reporter = new koolc.utils.Reporter, file = file, outDir = None)
    (parseCtx(ctx, _), Source.fromFile(file).mkString)
  }

  def addDefaultProgram(expression: String): String = {
    """
    object P { def main(): Unit = { } }

    class A {
      def T() : Int = {
        println(
        """ + expression + """
        ); 
        return 0;
      }
    }
    """  
  }
  
  def extractExpression(s: String): String = {
     val pattern = "Println\\((.*)\\)\\),IntLit".r
     val res = pattern.findFirstMatchIn(s)
     res match {
       case Some(_) => res.get.group(1).replace(" ", "")
       case None => ""
     }
  }

}