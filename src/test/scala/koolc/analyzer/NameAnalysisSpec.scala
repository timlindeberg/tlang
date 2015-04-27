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

class NameAnalysisSpec extends FlatSpec with Matchers with BeforeAndAfter {
  val flag = "--ast --symid"

  before {
    Symbols.ID.reset
  }

  behavior of "Positive tests"
  TestUtils.programFiles(TestUtils.resources + "analyzer/name/valid/").foreach { file =>
    it should "name analyse program " + file.toPath() in test(file)
  }

  behavior of "Negative tests"
  TestUtils.programFiles(TestUtils.resources + "analyzer/name/invalid/").foreach { file =>
    it should "name analyse program " + file.toPath() in test(file, true)
  }

  def test(file: File, exception: Boolean = false) = {
    val program = Source.fromFile(file).mkString
    val ctx = new Context(reporter = new koolc.utils.Reporter, file = file, outDir = None)
    def analysis(p: Program) = NameAnalysis.run(ctx)(p)
    def parse(p: String) = Parser.run(ctx)(Lexer.run(p.toList, ctx.file))
    def print(p: Program) = Printer(p, true)
    if (exception) {
      analysis(parse(program))
      assert(ctx.reporter.hasErrors)
    } else {
      val prog = analysis(parse(program))
      val res = ASTPrinterWithSymbols(prog)
      val correct = replaceIDNumbers(getAnswer(file), res)

      assert(hasTypes(prog))
      assert(res + "\n" == correct)
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

  val idRegex = """#(\d+)""".r

  def replaceIDNumbers(ast1: String, ast2: String): String = {
    val idMap = getSymbolIDMap(ast1, ast2)
    idRegex.replaceAllIn(ast1, m => "#" + idMap(m.group(1).toInt))
  }

  def getSymbolIDMap(ast1: String, ast2: String) = {
    val map: HashMap[Int, Int] = HashMap()
    getSymbolIDs(ast1).zip(getSymbolIDs(ast2)).foreach {
      case (x, y) =>
        if (map.contains(x))
          assert(map(x) == y)
        map(x) = y
    }
    map
  }

  def getSymbolIDs(ast: String) = idRegex.findAllIn(ast).matchData.map(_.group(1).toInt).toList

  def getAnswer(file: File) = Seq(TestUtils.runScript, flag + " " + file.toPath()) !! TestUtils.IgnoreErrorOutput

}