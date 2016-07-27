package tcompiler

import java.io.{File, FileNotFoundException}

import tcompiler.analyzer.{NameAnalysis, TypeChecking}
import tcompiler.ast.Parser
import tcompiler.code.{CodeGeneration, Desugaring}
import tcompiler.lexer.Lexer
import tcompiler.modification.Templates
import tcompiler.utils.CompilationException

/**
  * Created by Tim Lindeberg on 4/11/2016.
  */
trait ValidTester extends Tester {

  import TestUtils._

  override def Pipeline = Lexer andThen Parser andThen Templates andThen NameAnalysis andThen TypeChecking

  def testFile(file: File): Unit = {
    val ctx = getTestContext(file)

    try {
      val cus = Pipeline.run(ctx)(List(file))

      //hasTypes(program) should be(true)
      ctx.reporter.hasErrors should be(false)

      val compilation = Desugaring andThen CodeGeneration
      compilation.run(ctx)(cus)
      val res = lines(executeTProgram(file))
      val sol = parseSolutions(file)
      assertCorrect(res.map((0, _)), sol, "", checkLineNumbers = false)
    } catch {
      case t: CompilationException  => fail(s"Compilation failed:\n ${t.getMessage}")
      case t: FileNotFoundException => fail(s"Invalid test, file not found: ${file.getPath}")
    }
  }

}
