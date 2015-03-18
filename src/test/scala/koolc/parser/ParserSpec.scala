package koolc.parser

import org.scalatest._
import java.io.File
import koolc.utils._
import scala.io.Source
import koolc.ast._
import koolc.ast.Trees._
import koolc.lexer._

class ParserSpec extends FlatSpec with Matchers {
  val testResource = "./src/test/resources/parser/"
  val validPrograms = 4

  for (i <- 1 to validPrograms) {
    it should "parse valid program " + i in testProgram("valid-" + i)
  }

  def testProgram(p: String) {
    val program = testResource + p + ".kool"
    val ctx = new Context(reporter = new koolc.utils.Reporter, file = new File(program), outDir = None)
    val P = Source.fromFile(new File(program)).mkString

    def parse(p: String): Program = {
      val tokens = Lexer.run(p.toList, ctx.file)
      Parser.run(ctx)(tokens)
    }
    def print(p: Program) = Printer(p)
    println(print(parse(P)))
    assert(print(parse(P)) === print(parse(print(parse(P)))))
  }

}