package tcompiler

import java.io.{File, FileNotFoundException}

import org.scalatest._
import tcompiler.analyzer.{NameAnalysis, Symbols, TypeChecking}
import tcompiler.ast._
import tcompiler.code.CodeGeneration
import tcompiler.lexer.Lexer
import tcompiler.modification.{Imports, Templates}
import tcompiler.utils.{CompilationException, Context}

import scala.io.Source

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
    val ctx = new Context(reporter = new tcompiler.utils.Reporter, file = file, outDir = Some(new File("./gen/" + file.getName + "/")))
    val quietCtx = ctx.copy(reporter = new tcompiler.utils.Reporter(quiet = true))
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
      val res = execute(ctx)
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

  def execute(ctx: Context) = "java -cp ./gen/" + ctx.file.getName + " " + Main.fileName(ctx)
  def readSolution(fileName: String): Iterator[String] = Source.fromFile(fileName).getLines()

}