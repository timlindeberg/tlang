package koolc.lexer.code

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
import koolc.code.CodeGeneration
import koolc.analyzer.TypeChecking
import koolc.ast.Trees.Program
import koolc.analyzer.Symbols
import koolc.Typez

class CodeSpec extends FlatSpec with Matchers with BeforeAndAfter {
  val flag = "--eval"

  before {
    Symbols.ID.reset
  }

  behavior of "Positive tests"
  //var file = new File(TestUtils.resources + "/given/ast/valid/VehicleRent.kool")
  //it should "code gen program " + file.toPath() in test(file)
  TestUtils.programFiles(TestUtils.resources + "/programs/").foreach { file =>
    it should "code gen program " + file.toPath() in test(file)
  }

  def test(file: File, exception: Boolean = false) = {
    val program = Source.fromFile(file).mkString
    val ctx = new Context(reporter = new koolc.utils.Reporter, file = file, outDir = Some(new File("./gen/" + file.getName + "/")))
    def analysis(p: Program) = NameAnalysis.run(ctx)(p)
    def tcheck(p: Program) = TypeChecking.run(ctx)(p)
    def code(p: Program) = CodeGeneration.run(ctx)(p)
    def parse(p: String) = Parser.run(ctx)(Lexer.run(p.toList, ctx.file))
    def print(p: Program) = println(Printer(p, true))
    val prog = tcheck(analysis(parse(program)))
    assert(!ctx.reporter.hasErrors)
    print(prog)
    code(prog)
    val res = execute(prog, file)
    println(res)
    //assert(Typez(prog))
    //assert(res == getAnswer(file))
  }

  def flatten(l: List[_]): List[_] = l flatMap {
    case l1: List[_] => flatten(l1)
    case otherwise   => List(otherwise)
  }

  def execute(prog: Program, f: File) = "java -cp ./gen/" + f.getName + " " + prog.main.id.value !!
  def getAnswer(file: File) = Seq(TestUtils.runScript, flag + " " + file.toPath()) !!

}