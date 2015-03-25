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

  behavior of "Created tests without reference"
  for (i <- 1 to 3) {
    it should "lex invalid " + i in compare(TestUtils.resources + "/lexer/invalid/invalid-" + i)
  }

  def lexer(file: File): Iterator[Token] = Lexer.run(new Context(reporter = new koolc.utils.Reporter, file = file, outDir = None))(file)
  def test(file: File) = {
    def getAnswer(file: File) = Seq(TestUtils.runScript, flag + " " + file.toPath()).!!.split('\n').map(_.trim)
    lexer(file).toList.map(TestUtils.format).zip(getAnswer(file)).foreach(x => assert(x._1 === x._2))
  }
  def compare(file: String) {
    def readSolution(fileName: String): Iterator[String] = Source.fromFile(fileName).getLines()
    val it = lexer(new File(file + ".kool")).toList.map(TestUtils.format)
    val sol = readSolution(file + "-solution.kool").toList.map(_.trim)
    assert(it.length === sol.length)
    it.zip(sol).foreach(x => assert(x._1 === x._2))
  }

}