package koolc

import org.scalatest._
import scala.sys.process._
import koolc.utils.Context
import java.io.File
import koolc.lexer.Token
import koolc.lexer.Lexer
import koolc.ast._
import koolc.utils.CompilationException
import scala.io.Source
import koolc.ast.Trees.Program
import scala.collection.mutable.HashMap._
import koolc.analyzer.Types._
import koolc.analyzer.Symbols.ClassSymbol
import koolc.analyzer.NameAnalysis
import koolc.analyzer.TypeChecking
import koolc.code.CodeGeneration
import koolc.ast.Trees.Program
import koolc.analyzer.Symbols
import java.io.FileNotFoundException

class TemplateSpec extends FlatSpec with Matchers with BeforeAndAfter {

  before {
    Symbols.ID.reset
  }

  behavior of "Templates"
  TestUtils.programFiles(TestUtils.resources + "templates").foreach { file =>
    it should "yes" + file.toPath() in test(file)
  }

  def test(file: File, exception: Boolean = false) = {
    val options = TestUtils.readOptions(file)
    val ctx = new Context(reporter = new koolc.utils.Reporter(options.contains("quietReporter")), file = file, outDir = Some(new File("./gen/" + file.getName + "/")))
    val program = (Lexer andThen Parser andThen Templates).run(ctx)(ctx.file)
    //  s andThen TypeChecking
    //println(Printer(program, true))
    //println(ASTPrinterWithSymbols(program))
    TestUtils.HasTypes(program) should be(true)
    //    ctx.reporter.hasErrors should be(false)
    //
    CodeGeneration.run(ctx)(program)
    
    //
    //    val res = execute(program, file)
    //
    //    println(res)

    // Try and compare result with solution file
    //    try {
    //      val sol = readSolution(file + "-solution").toList
    //      val r = res.split("\n").toList
    //      r.length should be(sol.length)
    //      r.zip(sol).foreach(x => x._1 should be(x._2))
    //    } catch {
    //      case t: FileNotFoundException =>
    //    }

  }

  def execute(prog: Program, f: File) = "java -cp ./gen/" + f.getName + " " + prog.main.id.value !!
  def readSolution(fileName: String): Iterator[String] = Source.fromFile(fileName).getLines()

}