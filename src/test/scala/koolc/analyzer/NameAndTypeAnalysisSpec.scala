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

class NameAndTypeAnalysisSpec extends FlatSpec with Matchers with BeforeAndAfter {
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
    def analysis(p: Program) = NameAnalysis.run(ctx)(p)
    def tanalysis(p: Program) = TypeChecking.run(ctx)(p)
    def parse(p: String) = Parser.run(ctx)(Lexer.run(p.toList, ctx.file))
    def print(p: Program) = Printer(p, true)
    if (exception) {
      tanalysis(analysis(parse(program)))
      assert(ctx.reporter.hasErrors)
    } else {
      val prog = tanalysis(analysis(parse(program)))
      val res = ASTPrinterWithSymbols(prog)
      val correct = getAnswer(file)
      val resList = getSymbolIDs(res)
      val correctList = getSymbolIDs(correct)
      assert(hasTypes(prog))
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

  def hasTypes(prog: Program) =
    
    flatten(prog.classes.map(_.getSymbol).map(klass => {
      List(
        klass.getType,
        klass.members.map(_._2.getType),
        klass.methods.map(_._2.getType),
        klass.methods.map(_._2).flatMap(meth => {
          List(
            meth.argList.map(_.getType),
            meth.members.map(_._2.getType),
            meth.params.map(_._2.getType))
        }))
    })).forall(_ != TUntyped)

  def flatten(l: List[_]): List[_] = l flatMap {
    case l1: List[_] => flatten(l1)
    case otherwise   => List(otherwise)
  }

  def getSymbolIDs(ast: String) = "#(\\d*)".r.findAllIn(ast).matchData.map(_.group(1)).filter(_ != "").map(_.toInt).toList.sortBy(+_)

  def getAnswer(file: File) = Seq(TestUtils.runScript, flag + " " + file.toPath()) !! (TestUtils.IgnoreErrorOutput)

}