package koolc.parser

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
  
  val testResource = "./src/test/resources/parser/"
  val valid = files(testResource + "valid/")
  val invalid = files(testResource + "invalid/")
  
  valid.zipWithIndex.foreach{ case (file, i) => 
     it should "parse valid program " + (i + 1) in testValidProgram(file)
  }
  
  invalid.zipWithIndex.foreach{ case (file, i) => 
     it should "not parse invalid program " + (i + 1) in testInvalidProgram(file)
  }

  // Helper functions

  def parseCtx(ctx: Context, p: String): Program = {
    val tokens = Lexer.run(p.toList, ctx.file)
    Parser.run(ctx)(tokens)
  }

  def print(p: Program) = Printer(p)

  def testValidProgram(file: File) {
    val (parse, p) = program(file)

    assert(print(parse(p)) === print(parse(print(parse(p)))))
  }

  def testInvalidProgram(file: File) {
    val (parse, p) = program(file)
    intercept[ParsingException] {
      (parse(p))
    }
  }

  def program(file: File): ((String) => Program, String) = {
    val ctx = new Context(reporter = new koolc.utils.Reporter, file = file, outDir = None)
    (parseCtx(ctx, _), Source.fromFile(file).mkString)
  }

}