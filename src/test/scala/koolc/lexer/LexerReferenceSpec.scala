package koolc.lexer

import org.scalatest._
import scala.sys.process._
import koolc.utils.Context
import java.io.File
import koolc.ast._
import koolc.TestUtils

class LexerReferenceSpec extends FlatSpec with Matchers {
  val runScript = "./reference/run.sh"
  val resources = "./src/test/resources/given/lexer/"
  val flag = "--tokens"

  val valid = TestUtils.programFiles(resources + "valid/")
  val invalid = TestUtils.programFiles(resources + "invalid/")

  def useLexer(file: File): Iterator[Token] = {
    val ctx = new Context(reporter = new koolc.utils.Reporter, file = file, outDir = None)
    Lexer.run(ctx)(ctx.file)
  }

  valid.foreach { file =>
    it should "lex valid program " + file.toPath() in test(file)
  }

  def getAnswer(file: File) = Seq(runScript, flag + " " + file.toPath()).!!.split('\n').map(_.trim)
  def test(file: File) = useLexer(file).toList.map(TestUtils.format).zip(getAnswer(file)).foreach(x => assert(x._1 === x._2))
}