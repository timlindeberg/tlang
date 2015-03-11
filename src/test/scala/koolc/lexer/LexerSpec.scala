package koolc.lexer

import org.scalatest._
import java.io.File

import koolc.utils._
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

class LexerSpec extends FlatSpec with Matchers {
  val testResource = "./src/test/resources/"

  it should "lex valid program 1" in testFile("valid-program-1")

  it should "lex valid program 2" in testFile("valid-program-2")

  def useLexer(fileName: String): Iterator[Token] = {
    val ctx = new Context(reporter = new koolc.utils.Reporter, file = new File(fileName), outDir = None)
    Lexer.run(ctx)(ctx.file)
  }

  def testFile(file: String) {
    val it = useLexer(testResource + file + ".kool").toList
    val sol = readSolution(testResource + file + "-solution.kool").toList
    assert(it.length === sol.length)
    it.zip(sol).foreach(x => assert(format(x._1) === x._2))
  }

  def readSolution(fileName: String): Iterator[String] = {
    Source.fromFile(fileName).getLines()
  }

  def format(token: Token): String = {
    token + "(" + token.line + ":" + token.col + ") "
  }

}