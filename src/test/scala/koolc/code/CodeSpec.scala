package koolc.code

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
import koolc.analyzer.Types._
import koolc.analyzer.Symbols.ClassSymbol
import koolc.analyzer.NameAnalysis
import koolc.analyzer.TypeChecking
import koolc.ast.Trees.Program
import koolc.analyzer.Symbols
import java.io.FileNotFoundException

class CodeSpec extends FlatSpec with Matchers with BeforeAndAfter {
  val flag = "--eval"

  before {
    Symbols.ID.reset
  }

  behavior of "Programs"
  TestUtils.programFiles(TestUtils.resources + "programs").foreach { file =>
    it should "code gen program " + file.toPath() in test(file)
  }
  TestUtils.programFiles(TestUtils.resources + "given/programs").foreach { file =>
    it should "code gen program " + file.toPath() in test(file)
  }

  def test(file: File, exception: Boolean = false) = {
    val ctx = new Context(reporter = new koolc.utils.Reporter, file = file, outDir = Some(new File("./gen/" + file.getName + "/")))
    val program = (Lexer andThen Parser andThen NameAnalysis andThen TypeChecking).run(ctx)(ctx.file)

    println(Printer(program))
    TestUtils.HasTypes(program) should be(true)
    ctx.reporter.hasErrors should be(false)

    CodeGeneration.run(ctx)(program)

    val res = execute(program, file)
    // Try and compare result with solution file
    try {
      val sol = readSolution(file + "-solution").toList
      val r = TestUtils.lines(res)
      r.length should be(sol.length)
      r.zip(sol).foreach{ case (res, sol) => res should be(sol) }
    } catch {
      case t: FileNotFoundException =>
    }

    // res should be (getAnswer(file))

  }

  def flatten(l: List[_]): List[_] = l flatMap {
    case l1: List[_] => flatten(l1)
    case otherwise   => List(otherwise)
  }

  def execute(prog: Program, f: File) = "java -cp ./gen/" + f.getName + " " + prog.main.id.value !!
  def getAnswer(file: File) = Seq(TestUtils.runScript, flag + " " + file.toPath()) !!
  def readSolution(fileName: String): Iterator[String] = Source.fromFile(fileName).getLines()

}