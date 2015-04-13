package koolc.analyzer

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

class NameAnalysisSpec extends FlatSpec with Matchers {

  behavior of "Created tests"
  TestUtils.programFiles(TestUtils.resources + "analyzer/name/valid/").foreach { file =>
    it should "name analyse program " + file.toPath() in test(file)
  }

  TestUtils.programFiles(TestUtils.resources + "analyzer/name/invalid/").foreach { file =>
    it should "name analyse program " + file.toPath() in test(file, true)
  }

  def test(file: File, exception: Boolean = false) = {
    val program = Source.fromFile(file).mkString
    val ctx = new Context(reporter = new koolc.utils.Reporter, file = file, outDir = None)
    def analysis(p: Program) = NameAnalysis.run(ctx)(p)
    def parse(p: String) = Parser.run(ctx)(Lexer.run(p.toList, ctx.file))
    def print(p: Program) = Printer(p, true)
    if (exception) {
      (Lexer andThen Parser andThen NameAnalysis).run(ctx)(file)
      assert(ctx.reporter.hasErrors)
    } else {
      print(analysis(parse(program)))
    }
  }

}