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
import koolc.analyzer.Types._
import koolc.analyzer.Symbols.ClassSymbol
import koolc.analyzer.Symbols.ClassSymbol

class TypeCheckingSpec extends FlatSpec with Matchers with BeforeAndAfter {
  val flag = "--ast --symid"

  before {
    Symbols.ID.reset
  }

  behavior of "Positive tests"
  TestUtils.programFiles(TestUtils.resources + "analyzer/type/valid/").foreach { file =>
    it should "type check program " + file.toPath() in test(file)
  }
  TestUtils.programFiles(TestUtils.resources + "given/ast/valid/").foreach { file =>
    it should "type check given program " + file.toPath() in test(file)
  }

  behavior of "Negative tests"
  TestUtils.programFiles(TestUtils.resources + "analyzer/type/invalid/").foreach { file =>
    it should "type check invalid program " + file.toPath() in test(file, true)
  }

  def test(file: File, exception: Boolean = false) = {
    var progString = Source.fromFile(file).getLines.toList
    var expectedErrors = 1
    var ignoreFirstLine = false
    try {
      expectedErrors = progString.head.toInt
      progString = progString.tail
      ignoreFirstLine = true
    } catch {
      case _: Throwable => expectedErrors = 1
    }

    val program = progString.mkString("\n")
    val ctx = new Context(reporter = new koolc.utils.Reporter(false, ignoreFirstLine), file = file, outDir = None)
    def nameAnalysis(p: Program) = NameAnalysis.run(ctx)(p)
    def typeChecking(p: Program) = TypeChecking.run(ctx)(p)
    def parse(p: String) = Parser.run(ctx)(Lexer.run(p.toList, ctx.file))
    def print(p: Program) = Printer(p, true)
    if (exception) {
      typeChecking(nameAnalysis(parse(program)))
      assert(ctx.reporter.errors === expectedErrors)
    } else {
      var tree = typeChecking(nameAnalysis(parse(program)))
      assert(!ctx.reporter.hasErrors)
    }
  }

  behavior of "Relations"

  it should "work with primitive types" in {
    val primitives = List(TInt, TString, TBool, TIntArray)
    val others = List(TError, Types.anyObject, TObject(new ClassSymbol("")), TUntyped)

    for (t1 <- primitives) {
      for (t2 <- primitives) {
        if (t1 == t2) {
          assert(t1.isSubTypeOf(t2))
        } else {
          assert(!t1.isSubTypeOf(t2))
        }
      }
      for (t2 <- others) {
        assert(!t1.isSubTypeOf(t2))
      }
    }
  }

  it should "should work with class types" in {
    val C1 = Types.anyObject.classSymbol;
    val C2 = new ClassSymbol("C2")
    val C3 = new ClassSymbol("C3")
    val C4 = new ClassSymbol("C4")
    val C5 = new ClassSymbol("C5")
    C2.parent = Some(C1)
    C3.parent = Some(C2)
    C4.parent = Some(C3)
    C5.parent = Some(C4)

    val T1 = Types.anyObject;
    val T2 = TObject(C2);
    val T3 = TObject(C3);
    val T4 = TObject(C4);
    val T5 = TObject(C5);

    C2.setType(T2)
    C3.setType(T3)
    C4.setType(T4)
    C5.setType(T5)

    val types = List(T1, T2, T3, T4, T5).zipWithIndex;
    for ((t1, i) <- types) {
      for ((t2, j) <- types) {
        if (j >= i) {
          assert(t2.isSubTypeOf(t1), t2 + " is subtype of " + t1)
          if (j > i) assert(!t1.isSubTypeOf(t2), t1 + " is not subtype of " + t2)
        }
      }
    }

  }

  def getAnswer(file: File) = Seq(TestUtils.runScript, flag + " " + file.toPath()) !! TestUtils.IgnoreErrorOutput

}