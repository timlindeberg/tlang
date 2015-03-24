package koolc.ast

import org.scalatest._
import scala.sys.process._
import koolc.utils.Context
import java.io.File
import koolc.lexer.Token
import koolc.lexer.Lexer
import koolc.ast._
import koolc.TestUtils

class ParserReferenceSpec extends FlatSpec with Matchers {
  val runScript = "./reference/run.sh"
  val resources = "./src/test/resources/given/ast/"
  val flag = "--ast"

  val valid = TestUtils.programFiles(resources + "valid/")
  val invalid = TestUtils.programFiles(resources + "invalid/")

  def useParser(file: File): String = {
    val ctx = new Context(reporter = new koolc.utils.Reporter, file = file, outDir = None)
    Parser.run(ctx)(Lexer.run(ctx)(ctx.file)).toString
  }

  valid.foreach { file =>
    it should "parse valid program " + file.toPath() in test(file)
  }

  // TODO invalid programs

  def test(file: File) = useParser(file).zip(getAnswer(file)).foreach(x => assert(x._1 === x._2))
  def getAnswer(file: File) = Seq(runScript, flag + " " + file.toPath()) !!
}