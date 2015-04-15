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
import scala.collection.mutable.HashMap._
import scala.collection.mutable.HashMap

class NameAnalysisSpec extends FlatSpec with Matchers with BeforeAndAfter {
  val flag = "--ast --symid"

  before {
    Symbols.ID.reset
  }

  behavior of "Positive tests"
  TestUtils.programFiles(TestUtils.resources + "analyzer/name/valid/").foreach { file =>
    it should "name analyse program " + file.toPath() in test(file)
  }
  TestUtils.programFiles(TestUtils.resources + "given/analyzer/valid/").foreach { file =>
    it should "analyse program " + file.toPath() in test(file)
  }

  behavior of "Negative tests"
  TestUtils.programFiles(TestUtils.resources + "analyzer/name/invalid/").foreach { file =>
    it should "name analyse program " + file.toPath() in test(file, true)
  }
  TestUtils.programFiles(TestUtils.resources + "given/analyzer/invalid/").foreach { file =>
    it should "analyse program " + file.toPath() in test(file, true)
  }

  def test(file: File, exception: Boolean = false) = {
    val program = Source.fromFile(file).mkString
    val ctx = new Context(reporter = new koolc.utils.Reporter, file = file, outDir = None)
    def analysis(p: Program) = (NameAnalysis andThen TypeChecking).run(ctx)(p)
    def parse(p: String) = Parser.run(ctx)(Lexer.run(p.toList, ctx.file))
    def print(p: Program) = Printer(p, true)
    if (exception) {
      (Lexer andThen Parser andThen NameAnalysis andThen TypeChecking).run(ctx)(file)
      assert(ctx.reporter.hasErrors)
    } else {
      val res = ASTPrinterWithSymbols(analysis(parse(program)))
      val correct = getAnswer(file)
      val resList = getSymbolIDs(res)
      val correctList = getSymbolIDs(correct)

      assert(getSymbolCount(resList) == getSymbolCount(correctList))
      assert(resList.size == correctList.size)
      assert(resList.distinct == correctList.distinct)
    }
  }

  def getSymbolCount(res: List[Int]) = {
    val resMap: HashMap[Int, Int] = HashMap()
    res.distinct.foreach { x =>
      val count = res.count(_ == x)
      resMap(count) = resMap.getOrElse(count, 0) + 1
    }
    resMap
  }

  def getSymbolIDs(ast: String) = "#(\\d*)".r.findAllIn(ast).matchData.map(_.group(1).toInt).toList.sortBy(+_)

  def getAnswer(file: File) = Seq(TestUtils.runScript, flag + " " + file.toPath()) !!
}