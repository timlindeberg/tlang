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

  val program = useParser(testResource + "valid-1" + ".kool")

  def useParser(fileName: String): Program = {
    val ctx = new Context(reporter = new koolc.utils.Reporter, file = new File(fileName), outDir = None)
    (Lexer andThen Parser).run(ctx)(ctx.file)
  }

  println(Printer(program))

}