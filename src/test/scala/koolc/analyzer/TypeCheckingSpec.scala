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
import scala.collection.mutable.HashMap
import koolc.analyzer.Types._
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
  TestUtils.programFiles(TestUtils.resources + "programs/").foreach { file =>
    it should "type check program " + file.toPath() in test(file)
  }
  TestUtils.programFiles(TestUtils.resources + "given/programs/").foreach { file =>
    it should "type check given program " + file.toPath() in test(file)
  }

  behavior of "Negative tests"
  TestUtils.programFiles(TestUtils.resources + "analyzer/type/invalid/").foreach { file =>
    it should "type check invalid program " + file.toPath() in test(file, true)
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

  def test(file: File, exception: Boolean = false) = {
    val options = TestUtils.readOptions(file)
    val ctx = new Context(reporter = new koolc.utils.Reporter(exception || options.contains("quietReporter")), file = file, outDir = None)
    val program = (Lexer andThen Parser andThen NameAnalysis andThen TypeChecking).run(ctx)(ctx.file)
    if (exception) {
      ctx.reporter.errors should be(options("expectedErrors"))
    } else {
      ctx.reporter.hasErrors should be(false)
      TestUtils.HasTypes(program) should be(true)
    }
  }

}