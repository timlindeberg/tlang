package koolc.lexer

import org.scalatest._
import java.io.File

import koolc.utils._
import koolc.TestUtils
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

class LexerSpec extends FlatSpec with Matchers {
  val testResource = "./src/test/resources/lexer/"
  val validPrograms = 8
  val invalidPrograms = 3

  for (i <- 1 to validPrograms) {
    it should "lex valid program " + i in testFile("valid-" + i)
  }

  for (i <- 1 to invalidPrograms) {
    it should "lex invalid program " + i in testFile("invalid-" + i)
  }

  def useLexer(fileName: String): Iterator[Token] = {
    val ctx = new Context(reporter = new koolc.utils.Reporter, file = new File(fileName), outDir = None)
    Lexer.run(ctx)(ctx.file)
  }

  def testFile(file: String) {
    val it = useLexer(testResource + file + ".kool").toList.map(TestUtils.format)
    val sol = readSolution(testResource + file + "-solution.kool").toList.map(_.trim)
    assert(it.length === sol.length)
    it.zip(sol).foreach(x => assert(x._1 === x._2))
  }

  def readSolution(fileName: String): Iterator[String] = Source.fromFile(fileName).getLines()

}