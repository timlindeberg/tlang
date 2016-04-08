package tcompiler.code

import java.io._

import org.scalatest._
import tcompiler.TestUtils
import tcompiler.analyzer.{NameAnalysis, TypeChecking}
import tcompiler.ast._
import tcompiler.lexer.Lexer
import tcompiler.utils.{CompilationException, Context}


class CodeSpec extends FlatSpec with Matchers {

  import TestUtils._

  behavior of "Correct Programs"
  programFiles(Resources + "programs/valid").foreach(test(_, testPositive))

  def test(file: File, testFunction: File => Unit): Unit = {
    if (file.isDirectory){
      programFiles(file.getPath).foreach(test(_, testFunction))
    } else{
      if(shouldBeIgnored(file))
      ignore should file.getName.toString in testFunction(file)
        else
      it should file.getName.toString in testFunction(file)
    }
  }

  def testPositive(file: File): Unit = {
    val ctx = new Context(reporter = new tcompiler.utils.Reporter, file = file, outDir = Some(new File("./gen/" + file.getName + "/")))

    try {
      val program = (Lexer andThen Parser andThen NameAnalysis andThen TypeChecking).run(ctx)(ctx.file)

      //hasTypes(program) should be(true)
      ctx.reporter.hasErrors should be(false)

      CodeGeneration.run(ctx)(program)

      val res = lines(executeTProgram(file, "./gen/"))
      val sol = parseSolutions(file)
      assertCorrect(res, sol)
    } catch {
      case t: CompilationException  => fail("Compilation failed:\n" + t.getMessage)
      case t: FileNotFoundException => fail("Invalid test, file not found: " + file.getPath)
    }
  }
}
