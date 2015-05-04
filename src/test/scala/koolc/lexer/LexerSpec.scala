package koolc.lexer

import org.scalatest._
import scala.sys.process._
import koolc.utils.Context
import java.io.File
import koolc.ast._
import koolc.TestUtils
import scala.io.Source

class LexerSpec extends FlatSpec with Matchers {
  val flag = "--tokens"

  behavior of "Given tests"
  TestUtils.programFiles(TestUtils.resources + "/given/lexer/valid/").foreach { file =>
    it should "lex valid program " + file.toPath() in test(file)
  }

  behavior of "Created tests"
  TestUtils.programFiles(TestUtils.resources + "/lexer/valid/").foreach { file =>
    it should "lex valid program " + file.toPath() in test(file)
  }

  TestUtils.programFiles(TestUtils.resources + "/lexer/invalid/").foreach { file =>
    it should "lex invalid program " + file.toPath() in test(file, true)
  }

  behavior of "Created tests without reference"

  def lexer(file: File): Iterator[Token] = Lexer.run(new Context(reporter = new koolc.utils.Reporter, file = file, outDir = None))(file)
  def test(file: File, exception: Boolean = false) = {
    def getAnswer(file: File) = Seq(TestUtils.runScript, flag + " " + file.toPath()).!!.split('\n').map(_.trim)
    def readSolution(fileName: String): Iterator[String] = Source.fromFile(fileName).getLines()
    val lex = lexer(file).toList.map(TestUtils.format)
    if (exception) {
      val sol = readSolution(file + "-solution").toList.map(_.trim)
      println(lex)
      assert(lex.length === sol.length)
      lex.zip(sol).foreach(x => assert(x._1 === x._2))
    } else {
      lex.zip(getAnswer(file)).foreach(x => assert(x._1 === x._2))
    }
  }
}