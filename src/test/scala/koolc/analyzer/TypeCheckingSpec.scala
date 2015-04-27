package koolc.analyzer

import org.scalatest._
import scala.sys.process._
import koolc.utils.Context
import java.io.File
import koolc.lexer.Token
import koolc.lexer.Lexer
import koolc.ast._
import koolc.TestUtils
import koolc.utils.CompilationException
import scala.io.Source
import koolc.ast.Trees.Program
import scala.collection.mutable.HashMap._
import scala.collection.mutable.HashMap
import koolc.analyzer.Types.TUntyped

class TypeCheckingSpec extends FlatSpec with Matchers with BeforeAndAfter {
  val flag = "--ast --symid"

  before {
    Symbols.ID.reset
  }

  behavior of "Positive tests"
  TestUtils.programFiles(TestUtils.resources + "analyzer/type/valid/").foreach { file =>
    it should "type check program " + file.toPath() in test(file)
  }
  TestUtils.programFiles(TestUtils.resources + "given/ast/valid/").foreach { file =>
    it should "type check given program " + file.toPath() in test(file)
  }

  behavior of "Negative tests"
  TestUtils.programFiles(TestUtils.resources + "analyzer/type/invalid/").foreach { file =>
    it should "type check invalid program " + file.toPath() in test(file, true)
  }
  TestUtils.programFiles(TestUtils.resources + "given/ast/invalid/").foreach { file =>
    it should "type check invalid given program" + file.toPath() in test(file)
  }

  def test(file: File, exception: Boolean = false) = {
    // TODO
    ???
  }

  def getAnswer(file: File) = Seq(TestUtils.runScript, flag + " " + file.toPath()) !! TestUtils.IgnoreErrorOutput

}