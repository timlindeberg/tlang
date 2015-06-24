package koolc

import org.scalatest._
import scala.sys.process._
import koolc.utils.Context
import java.io.File
import koolc.lexer.Lexer
import koolc.ast._
import koolc.utils.CompilationException
import scala.io.Source
import koolc.analyzer.NameAnalysis
import koolc.modification.{Imports, Templates}
import koolc.analyzer.TypeChecking
import koolc.code.CodeGeneration
import koolc.ast.Trees.Program
import koolc.analyzer.Symbols
import java.io.FileNotFoundException

class TemplateSpec extends FlatSpec with Matchers with BeforeAndAfter {

  before {
    Symbols.ID.reset
  }

  behavior of "Incorrect Templates"
  TestUtils.programFiles(TestUtils.resources + "templates/invalid/").foreach { file =>
    it should "not " + file.toPath() in test(file, true)
  }

  behavior of "Correct Templates"
  TestUtils.programFiles(TestUtils.resources + "templates/valid/").foreach { file =>
    it should "yes " + file.toPath() in test(file)
  }
  
  def test(file: File, exception: Boolean = false) = {
    val ctx = new Context(reporter = new koolc.utils.Reporter, file = file, outDir = Some(new File("./gen/" + file.getName + "/")))
    val quietCtx = ctx.copy(reporter = new koolc.utils.Reporter(quiet = true))
    def exec = Lexer andThen Parser andThen Imports andThen Templates andThen NameAnalysis andThen TypeChecking
    if (exception) {
      intercept[CompilationException] {
        exec.run(quietCtx)(file)
      }
    } else {
      val program = exec.run(ctx)(file)
      println(Printer(program))
      ctx.reporter.hasErrors should be(false)
      CodeGeneration.run(ctx)(program)
      val res = execute(program, file)
      try {
        val sol = readSolution(file + "-solution").toList
        val r = TestUtils.lines(res)
        r.length should be(sol.length)
        r.zip(sol).foreach { case (res, sol) => res should be(sol)}
      } catch {
        case t: FileNotFoundException =>
      }
    }

  }

  def execute(prog: Program, f: File) = "java -cp ./gen/" + f.getName + " " + prog.main.get.id.value !!
  def readSolution(fileName: String): Iterator[String] = Source.fromFile(fileName).getLines()

}