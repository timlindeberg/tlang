package tcompiler.code

import org.scalatest._
import scala.sys.process._
import tcompiler.utils.Context
import java.io.File
import tcompiler.lexer.Token
import tcompiler.lexer.Lexer
import tcompiler.ast._
import tcompiler.TestUtils
import tcompiler.utils.CompilationException
import scala.io.Source
import tcompiler.ast.Trees.Program
import scala.collection.mutable.HashMap._
import tcompiler.analyzer.Types._
import tcompiler.analyzer.Symbols.ClassSymbol
import tcompiler.analyzer.NameAnalysis
import tcompiler.analyzer.TypeChecking
import tcompiler.ast.Trees.Program
import tcompiler.analyzer.Symbols
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
    val ctx = new Context(reporter = new tcompiler.utils.Reporter, file = file, outDir = Some(new File("./gen/" + file.getName + "/")))
    val program = (Lexer andThen Parser andThen NameAnalysis andThen TypeChecking).run(ctx)(ctx.file)

    println(Printer(program))
    TestUtils.HasTypes(program) should be(true)
    ctx.reporter.hasErrors should be(false)

    CodeGeneration.run(ctx)(program)

    val res = execute(program, file)
    // Try and compare result with solution file
    try {
      val sol = readSolution(file + "-solution").toList
      println("res: \n" + res)
      println("sol: \n" + sol.mkString("\n"))
      val r = TestUtils.lines(res)
      r.length should be(sol.length)
      r.zip(sol).foreach{ case (res, sol) => res.trim should be(sol.trim) }
    } catch {
      case t: FileNotFoundException =>
    }

    // res should be (getAnswer(file))

  }

  def flatten(l: List[_]): List[_] = l flatMap {
    case l1: List[_] => flatten(l1)
    case otherwise   => List(otherwise)
  }

  def execute(prog: Program, f: File) = "java -cp ./gen/" + f.getName + " " + prog.main.get.id.value !!
  def getAnswer(file: File) = Seq(TestUtils.runScript, flag + " " + file.toPath()) !!
  def readSolution(fileName: String): Iterator[String] = Source.fromFile(fileName).getLines()

}