package koolc.ast

import org.scalatest._
import scala.sys.process._
import koolc.utils.Context
import java.io.File
import koolc.lexer.Token
import koolc.lexer.Lexer
import koolc.ast._
import koolc.TestUtils
import koolc.utils.ParsingException
import scala.io.Source
import koolc.ast.Trees.Program

class ParserSpec extends FlatSpec with Matchers {
  val flag = "--ast"

  behavior of "Created tests"
  TestUtils.programFiles(TestUtils.resources + "ast/valid/").foreach { file =>
    it should "parse valid program " + file.toPath() in test(file)
  }

  TestUtils.programFiles(TestUtils.resources + "ast/invalid/").foreach { file =>
    it should "parse invalid program " + file.toPath() in test(file, true)
  }

  behavior of "Given tests"
  TestUtils.programFiles(TestUtils.resources + "given/ast/valid/").foreach { file =>
    it should "parse valid program " + file.toPath() in test(file)
  }

  TestUtils.programFiles(TestUtils.resources + "given/ast/invalid/").foreach { file =>
    it should "parse invalid program " + file.toPath() in test(file, true)
  }

  def test(file: File, exception: Boolean = false) = {
    val program = Source.fromFile(file).mkString
    val ctx = new Context(reporter = new koolc.utils.Reporter, file = file, outDir = None)
    def parse(p: String) = Parser.run(ctx)(Lexer.run(p.toList, ctx.file))
    def print(p: Program) = Printer(p)
    if (exception) {
      intercept[ParsingException] { parse(program) }
    } else {
      assert(print(parse(program)) === print(parse(print(parse(program)))))
      assert(parse(program) === parse(print(parse(program))))
      assert(parse(program).toString + "\n" === getAnswer(file))
    }
  }
  
  def getAnswer(file: File) = Seq(TestUtils.runScript, flag + " " + file.toPath()) !!

}