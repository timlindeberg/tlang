package tcompiler

import java.io.{File, FileNotFoundException}

import tcompiler.analyzer.{NameAnalysis, TypeChecking}
import tcompiler.ast.Parser
import tcompiler.code.CodeGeneration
import tcompiler.lexer.Lexer
import tcompiler.modification.{Imports, Templates}
import tcompiler.utils.{CompilationException, Context}

/**
  * Created by Tim Lindeberg on 4/11/2016.
  */
trait ValidTester extends Tester {

  import TestUtils._

  override def Pipeline = Lexer andThen Parser andThen Templates andThen Imports andThen NameAnalysis andThen TypeChecking

  def testFile(file: File): Unit = {
    val ctx = new Context(reporter = new tcompiler.utils.Reporter, file = file, outDir = Some(new File("./gen/" + file.getName + "/")))

    try {
      val program = Pipeline.run(ctx)(ctx.file)

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
