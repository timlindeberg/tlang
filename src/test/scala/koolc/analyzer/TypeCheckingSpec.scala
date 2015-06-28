package koolc.analyzer

import java.io.File

import koolc.TestUtils
import koolc.analyzer.Symbols.{VariableSymbol, ClassSymbol, MethodSymbol}
import koolc.analyzer.Types._
import koolc.ast.Trees._
import koolc.ast._
import koolc.lexer.Lexer
import koolc.utils.{CompilationException, Context}
import org.scalatest._

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
    val primitives = List(TInt, TString, TBool)
    val others = List(TError, anyObject, TObject(new ClassSymbol("")), TUntyped)

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
    val C1 = anyObject.classSymbol
    val C2 = new ClassSymbol("C2")
    val C3 = new ClassSymbol("C3")
    val C4 = new ClassSymbol("C4")
    val C5 = new ClassSymbol("C5")
    C2.parent = Some(C1)
    C3.parent = Some(C2)
    C4.parent = Some(C3)
    C5.parent = Some(C4)

    val T1 = anyObject
    val T2 = TObject(C2)
    val T3 = TObject(C3)
    val T4 = TObject(C4)
    val T5 = TObject(C5)

    C2.setType(T2)
    C3.setType(T3)
    C4.setType(T4)
    C5.setType(T5)

    val types = List(T1, T2, T3, T4, T5).zipWithIndex
    for ((t1, i) <- types) {
      for ((t2, j) <- types) {
        if (j >= i) {
          assert(t2.isSubTypeOf(t1), t2 + " is subtype of " + t1)
          if (j > i) assert(!t1.isSubTypeOf(t2), t1 + " is not subtype of " + t2)
        }
      }
    }

  }

  val classSymbol = new ClassSymbol("obj")
  val varSymbol   = new VariableSymbol("var")
  val mainMethod  = new MethodSymbol("main", classSymbol, Private).setType(TUnit)
  val testContext = Context(new koolc.utils.Reporter(), None, new File(""))
  val typeChecker = new TypeChecker(testContext, mainMethod)


  def int()    = createIdentifier(TInt)
  def bool()   = createIdentifier(TBool)
  def long()   = createIdentifier(TLong)
  def float()  = createIdentifier(TFloat)
  def double() = createIdentifier(TDouble)
  def char()   = createIdentifier(TChar)
  def string() = createIdentifier(TString)
  def array()  = createIdentifier(TArray(TInt))
  def obj()    = Identifier("").setSymbol(classSymbol).setType(TObject(classSymbol))

  def createIdentifier(tpe: Type) = Identifier("").setSymbol(new VariableSymbol("")).setType(tpe)



  behavior of "Operators"

  it should "Plus" in testPlusOperator
  it should "Minus" in testBinaryOperator(Minus)
  it should "Times" in testBinaryOperator(Times)
  it should "Div" in testBinaryOperator(Div)
  it should "Modulo" in testBinaryOperator(Modulo)

  it should "LogicAnd" in testLogicOperator(LogicAnd)
  it should "LogicOr" in testLogicOperator(LogicOr)
  it should "LogicXor" in testLogicOperator(LogicXor)
  it should "LeftShift" in testShiftOperator(LeftShift)
  it should "RightShift" in testShiftOperator(RightShift)

  it should "Assign" in testAssignOperator
  it should "PlusAssign" in testPlusAssignOperator
  it should "MinusAssign" in testAssignmentOperator(MinusAssign)
  it should "MulAssign" in testAssignmentOperator(MulAssign)
  it should "DivAssign" in testAssignmentOperator(DivAssign)
  it should "ModAssign" in testAssignmentOperator(ModAssign)

  it should "AndAssign" in testLogicalAssignmentOperator(AndAssign)
  it should "OrAssign" in testLogicalAssignmentOperator(OrAssign)
  it should "XorAssign" in testLogicalAssignmentOperator(XorAssign)
  it should "LeftShiftAssign" in testShiftAssignmentOperator(LeftShiftAssign)
  it should "RightShiftAssign" in testShiftAssignmentOperator(RightShiftAssign)
  it should "ArrayAssign" in testArrayAssignOperator

  it should "LessThan" in testComparisonOperator(LessThan)
  it should "LessThanEquals" in testComparisonOperator(LessThanEquals)
  it should "GreaterThan" in testComparisonOperator(GreaterThan)
  it should "GreaterThanEquals" in testComparisonOperator(GreaterThanEquals)
  it should "Equals" in testEqualsOperator(Equals)
  it should "NotEquals" in testEqualsOperator(NotEquals)

  def testPlusOperator() = {
    val asserter = new BinaryExpressionAsserter(Plus)
    import asserter._
    valid(string, string, TString)
    valid(string, int, TString)
    valid(string, long, TString)
    valid(string, float, TString)
    valid(string, double, TString)
    valid(string, char, TString)
    valid(string, bool, TString)

    valid(int, int, TInt)
    valid(int, long, TLong)
    valid(int, float, TFloat)
    valid(int, double, TDouble)
    valid(int, char, TInt)

    valid(long, long, TLong)
    valid(long, float, TFloat)
    valid(long, double, TDouble)
    valid(long, char, TLong)

    valid(float, float, TFloat)
    valid(float, double, TDouble)
    valid(float, char, TFloat)

    valid(double, double, TDouble)
    valid(double, char, TDouble)

    invalid(bool, bool)
    invalid(bool, int)
    invalid(bool, long)
    invalid(bool, float)
    invalid(bool, double)
    invalid(bool, char)

    invalid(obj, string)
    invalid(obj, int)
    invalid(obj, long)
    invalid(obj, float)
    invalid(obj, double)
    invalid(obj, char)
    invalid(obj, array)

    invalid(array, array)
    invalid(array, string)
    invalid(array, obj)
    invalid(array, int)
    invalid(array, long)
    invalid(array, float)
    invalid(array, double)
    invalid(array, char)
  }

  def testBinaryOperator(expressionType: (ExprTree, ExprTree) => ExprTree) = {
    val asserter = new BinaryExpressionAsserter(expressionType)
    import asserter._

    valid(int, int, TInt)
    valid(int, long, TLong)
    valid(int, float, TFloat)
    valid(int, double, TDouble)
    valid(int, char, TInt)

    valid(long, long, TLong)
    valid(long, float, TFloat)
    valid(long, double, TDouble)
    valid(long, char, TLong)

    valid(float, float, TFloat)
    valid(float, double, TDouble)
    valid(float, char, TFloat)

    valid(double, double, TDouble)
    valid(double, char, TDouble)

    invalid(bool, string)
    invalid(bool, bool)
    invalid(bool, int)
    invalid(bool, long)
    invalid(bool, float)
    invalid(bool, double)
    invalid(bool, char)

    invalid(obj, string)
    invalid(obj, obj)
    invalid(obj, int)
    invalid(obj, long)
    invalid(obj, float)
    invalid(obj, double)
    invalid(obj, char)

    invalid(string, string)
    invalid(string, obj)
    invalid(string, int)
    invalid(string, long)
    invalid(string, float)
    invalid(string, double)
    invalid(string, char)

    invalid(array, array)
    invalid(array, string)
    invalid(array, obj)
    invalid(array, int)
    invalid(array, long)
    invalid(array, float)
    invalid(array, double)
    invalid(array, char)
  }

  def testLogicOperator(expressionType: (ExprTree, ExprTree) => ExprTree) = {
    val asserter = new BinaryExpressionAsserter(expressionType)
    import asserter._

    valid(int, int, TInt)
    valid(int, long, TLong)
    valid(int, char, TInt)

    valid(long, long, TLong)
    valid(long, char, TLong)
    valid(bool, bool, TBool)

    invalid(int, float)
    invalid(int, double)

    invalid(long, float)
    invalid(long, double)

    invalid(float, float)
    invalid(float, double)
    invalid(float, char)

    invalid(char, char)

    invalid(double, double)
    invalid(double, char)

    invalid(bool, string)
    invalid(bool, int)
    invalid(bool, long)
    invalid(bool, float)
    invalid(bool, double)
    invalid(bool, char)

    invalid(obj, string)
    invalid(obj, int)
    invalid(obj, long)
    invalid(obj, float)
    invalid(obj, double)
    invalid(obj, char)

    invalid(string, int)
    invalid(string, long)
    invalid(string, float)
    invalid(string, double)
    invalid(string, char)

    invalid(array, array)
    invalid(array, string)
    invalid(array, obj)
    invalid(array, int)
    invalid(array, long)
    invalid(array, float)
    invalid(array, double)
    invalid(array, char)
  }

  def testShiftOperator(expressionType: (ExprTree, ExprTree) => ExprTree) = {
    val asserter = new BinaryExpressionAsserter(expressionType)
    import asserter._

    valid(int, int, TInt)
    valid(int, long, TLong)
    valid(int, char, TInt)

    valid(long, long, TLong)
    valid(long, char, TLong)

    invalid(int, float)
    invalid(int, double)

    invalid(long, float)
    invalid(long, double)

    invalid(float, float)
    invalid(float, double)
    invalid(float, char)

    invalid(double, double)
    invalid(double, char)

    invalid(bool, bool)
    invalid(bool, string)
    invalid(bool, int)
    invalid(bool, long)
    invalid(bool, float)
    invalid(bool, double)
    invalid(bool, char)

    invalid(obj, string)
    invalid(obj, int)
    invalid(obj, long)
    invalid(obj, float)
    invalid(obj, double)
    invalid(obj, char)

    invalid(string, int)
    invalid(string, long)
    invalid(string, float)
    invalid(string, double)
    invalid(string, char)

    invalid(array, array)
    invalid(array, string)
    invalid(array, obj)
    invalid(array, int)
    invalid(array, long)
    invalid(array, float)
    invalid(array, double)
    invalid(array, char)
  }


  def testAssignOperator() = {
    val asserter = new AssignmentAsserter(Assign)
    import asserter._
    valid(bool, bool, TBool)

    valid(char, char, TChar)
    valid(char, int, TChar)

    valid(int, int, TInt)
    valid(int, char, TInt)

    valid(long, long, TLong)
    valid(long, int, TLong)
    valid(long, char, TLong)

    valid(float, float, TFloat)
    valid(float, int, TFloat)
    valid(float, char, TFloat)
    valid(float, long, TFloat)

    valid(double, double, TDouble)
    valid(double, float, TDouble)
    valid(double, int, TDouble)
    valid(double, long, TDouble)
    valid(double, char, TDouble)

    valid(string, string, TString)

    valid(obj, obj, obj.getType)

    valid(array, array, array.getType)


    invalid(bool, int)
    invalid(bool, long)
    invalid(bool, float)
    invalid(bool, double)
    invalid(bool, char)
    invalid(bool, string)
    invalid(bool, obj)
    invalid(bool, array)

    invalid(char, long)
    invalid(char, bool)
    invalid(char, float)
    invalid(char, double)
    invalid(char, string)
    invalid(char, obj)
    invalid(char, array)

    invalid(int, float)
    invalid(int, bool)
    invalid(int, double)
    invalid(int, long)
    invalid(int, string)
    invalid(int, obj)
    invalid(int, array)

    invalid(long, float)
    invalid(long, bool)
    invalid(long, double)
    invalid(long, string)
    invalid(long, obj)
    invalid(long, array)

    invalid(float, double)
    invalid(float, bool)
    invalid(float, string)
    invalid(float, obj)
    invalid(float, array)

    invalid(double, string)
    invalid(double, bool)
    invalid(double, obj)
    invalid(double, array)

    invalid(string, int)
    invalid(string, bool)
    invalid(string, long)
    invalid(string, float)
    invalid(string, double)
    invalid(string, char)
    invalid(string, obj)

    invalid(obj, int)
    invalid(obj, bool)
    invalid(obj, long)
    invalid(obj, float)
    invalid(obj, double)
    invalid(obj, char)
    invalid(obj, string)

    invalid(array, string)
    invalid(array, bool)
    invalid(array, obj)
    invalid(array, int)
    invalid(array, long)
    invalid(array, float)
    invalid(array, double)
    invalid(array, char)
  }

  def testPlusAssignOperator() = {
    val asserter = new AssignmentAsserter(PlusAssign)
    import asserter._
    valid(int, int, TInt)
    valid(int, char, TInt)

    valid(long, long, TLong)
    valid(long, int, TLong)
    valid(long, char, TLong)

    valid(float, float, TFloat)
    valid(float, int, TFloat)
    valid(float, char, TFloat)
    valid(float, long, TFloat)

    valid(double, double, TDouble)
    valid(double, float, TDouble)
    valid(double, int, TDouble)
    valid(double, long, TDouble)
    valid(double, char, TDouble)

    valid(string, string, TString)
    valid(string, int, TString)
    valid(string, long, TString)
    valid(string, float, TString)
    valid(string, double, TString)
    valid(string, char, TString)


    invalid(bool, bool)
    invalid(bool, int)
    invalid(bool, long)
    invalid(bool, float)
    invalid(bool, double)
    invalid(bool, char)
    invalid(bool, string)
    invalid(bool, obj)
    invalid(bool, array)

    invalid(char, char)
    invalid(char, bool)
    invalid(char, int)
    invalid(char, long)
    invalid(char, float)
    invalid(char, double)
    invalid(char, string)
    invalid(char, obj)
    invalid(char, array)

    invalid(int, float)
    invalid(int, bool)
    invalid(int, double)
    invalid(int, long)
    invalid(int, string)
    invalid(int, obj)
    invalid(int, array)

    invalid(long, float)
    invalid(long, bool)
    invalid(long, double)
    invalid(long, string)
    invalid(long, obj)
    invalid(long, array)

    invalid(float, double)
    invalid(float, bool)
    invalid(float, string)
    invalid(float, obj)
    invalid(float, array)

    invalid(double, string)
    invalid(double, bool)
    invalid(double, obj)
    invalid(double, array)

    invalid(obj, obj)
    invalid(obj, bool)
    invalid(obj, int)
    invalid(obj, long)
    invalid(obj, float)
    invalid(obj, double)
    invalid(obj, char)
    invalid(obj, string)
    invalid(obj, array)

    invalid(array, array)
    invalid(array, bool)
    invalid(array, string)
    invalid(array, obj)
    invalid(array, int)
    invalid(array, long)
    invalid(array, float)
    invalid(array, double)
    invalid(array, char)
  }

  def testAssignmentOperator(expressionType: (Identifier, ExprTree) => ExprTree) = {
    val asserter = new AssignmentAsserter(expressionType)
    import asserter._
    valid(int, int, TInt)
    valid(int, char, TInt)

    valid(long, long, TLong)
    valid(long, int, TLong)
    valid(long, char, TLong)

    valid(float, float, TFloat)
    valid(float, int, TFloat)
    valid(float, char, TFloat)
    valid(float, long, TFloat)

    valid(double, double, TDouble)
    valid(double, float, TDouble)
    valid(double, int, TDouble)
    valid(double, long, TDouble)
    valid(double, char, TDouble)

    invalid(char, char)
    invalid(char, int)
    invalid(char, long)
    invalid(char, float)
    invalid(char, double)
    invalid(char, string)
    invalid(char, obj)
    invalid(char, array)

    invalid(int, float)
    invalid(int, double)
    invalid(int, long)
    invalid(int, string)
    invalid(int, obj)

    invalid(long, float)
    invalid(long, double)
    invalid(long, string)
    invalid(long, obj)

    invalid(float, double)
    invalid(float, string)
    invalid(float, obj)

    invalid(double, string)
    invalid(double, obj)

    invalid(string, string)
    invalid(string, int)
    invalid(string, long)
    invalid(string, float)
    invalid(string, double)
    invalid(string, char)
    invalid(string, obj)

    invalid(obj, obj)
    invalid(obj, int)
    invalid(obj, long)
    invalid(obj, float)
    invalid(obj, double)
    invalid(obj, char)
    invalid(obj, string)

    invalid(array, array)
    invalid(array, string)
    invalid(array, obj)
    invalid(array, int)
    invalid(array, long)
    invalid(array, float)
    invalid(array, double)
    invalid(array, char)
  }

  def testLogicalAssignmentOperator(expressionType: (Identifier, ExprTree) => ExprTree) = {
    val asserter = new AssignmentAsserter(expressionType)
    import asserter._
    valid(int, int, TInt)
    valid(int, char, TInt)

    valid(long, long, TLong)
    valid(long, int, TLong)
    valid(long, char, TLong)

    valid(bool, bool, TBool)

    invalid(bool, int)
    invalid(bool, long)
    invalid(bool, float)
    invalid(bool, double)
    invalid(bool, char)
    invalid(bool, string)
    invalid(bool, obj)
    invalid(bool, array)

    invalid(char, char)
    invalid(char, bool)
    invalid(char, int)
    invalid(char, long)
    invalid(char, float)
    invalid(char, double)
    invalid(char, string)
    invalid(char, obj)
    invalid(char, array)

    invalid(int, float)
    invalid(int, bool)
    invalid(int, double)
    invalid(int, long)
    invalid(int, string)
    invalid(int, obj)

    invalid(long, float)
    invalid(long, bool)
    invalid(long, double)
    invalid(long, string)
    invalid(long, obj)

    invalid(float, float)
    invalid(float, bool)
    invalid(float, int)
    invalid(float, char)
    invalid(float, long)
    invalid(float, double)
    invalid(float, string)
    invalid(float, obj)

    invalid(double, double)
    invalid(double, bool)
    invalid(double, float)
    invalid(double, int)
    invalid(double, long)
    invalid(double, char)
    invalid(double, string)
    invalid(double, obj)

    invalid(string, string)
    invalid(string, bool)
    invalid(string, int)
    invalid(string, long)
    invalid(string, float)
    invalid(string, double)
    invalid(string, char)
    invalid(string, obj)

    invalid(obj, obj)
    invalid(obj, bool)
    invalid(obj, int)
    invalid(obj, long)
    invalid(obj, float)
    invalid(obj, double)
    invalid(obj, char)
    invalid(obj, string)

    invalid(array, array)
    invalid(array, bool)
    invalid(array, string)
    invalid(array, obj)
    invalid(array, int)
    invalid(array, long)
    invalid(array, float)
    invalid(array, double)
    invalid(array, char)
  }

  def testShiftAssignmentOperator(expressionType: (Identifier, ExprTree) => ExprTree) = {
    val asserter = new AssignmentAsserter(expressionType)
    import asserter._
    valid(int, int, TInt)
    valid(int, char, TInt)

    valid(long, long, TLong)
    valid(long, int, TLong)
    valid(long, char, TLong)

    invalid(bool, bool)
    invalid(bool, int)
    invalid(bool, long)
    invalid(bool, float)
    invalid(bool, double)
    invalid(bool, char)
    invalid(bool, string)
    invalid(bool, obj)
    invalid(bool, array)

    invalid(char, char)
    invalid(char, bool)
    invalid(char, int)
    invalid(char, long)
    invalid(char, float)
    invalid(char, double)
    invalid(char, string)
    invalid(char, obj)
    invalid(char, array)

    invalid(int, float)
    invalid(int, bool)
    invalid(int, double)
    invalid(int, long)
    invalid(int, string)
    invalid(int, obj)

    invalid(long, float)
    invalid(long, bool)
    invalid(long, double)
    invalid(long, string)
    invalid(long, obj)

    invalid(float, float)
    invalid(float, bool)
    invalid(float, int)
    invalid(float, char)
    invalid(float, long)
    invalid(float, double)
    invalid(float, string)
    invalid(float, obj)

    invalid(double, double)
    invalid(double, bool)
    invalid(double, float)
    invalid(double, int)
    invalid(double, long)
    invalid(double, char)
    invalid(double, string)
    invalid(double, obj)

    invalid(string, string)
    invalid(string, bool)
    invalid(string, int)
    invalid(string, long)
    invalid(string, float)
    invalid(string, double)
    invalid(string, char)
    invalid(string, obj)

    invalid(obj, obj)
    invalid(obj, bool)
    invalid(obj, int)
    invalid(obj, long)
    invalid(obj, float)
    invalid(obj, double)
    invalid(obj, char)
    invalid(obj, string)

    invalid(array, array)
    invalid(array, bool)
    invalid(array, string)
    invalid(array, obj)
    invalid(array, int)
    invalid(array, long)
    invalid(array, float)
    invalid(array, double)
    invalid(array, char)
  }

  def testArrayAssignOperator() = {
    def valid(identifier: () => Identifier, expr: () => ExprTree, tpe: Type) = {
      testContext.reporter.clearErrors()
      val id = createIdentifier(TArray(identifier().getType))
      typeChecker.tcExpr(ArrayAssign(id, IntLit(0), expr())) should be(tpe)
      testContext.reporter.hasErrors should be(false)
    }

    def invalid(identifier: () => Identifier, expr: () => ExprTree) = {
      val id = createIdentifier(TArray(identifier().getType))

      typeChecker.tcExpr(ArrayAssign(id, IntLit(0), expr()))
      assertErrors()
    }

    valid(char, char, TChar)
    valid(char, int, TChar)

    valid(int, int, TInt)
    valid(int, char, TInt)

    valid(long, long, TLong)
    valid(long, int, TLong)
    valid(long, char, TLong)

    valid(float, float, TFloat)
    valid(float, int, TFloat)
    valid(float, char, TFloat)
    valid(float, long, TFloat)

    valid(double, double, TDouble)
    valid(double, float, TDouble)
    valid(double, int, TDouble)
    valid(double, long, TDouble)
    valid(double, char, TDouble)

    valid(string, string, TString)

    valid(obj, obj, obj.getType)


    invalid(char, long)
    invalid(char, float)
    invalid(char, double)
    invalid(char, string)
    invalid(char, obj)
    invalid(char, array)

    invalid(int, float)
    invalid(int, double)
    invalid(int, long)
    invalid(int, string)
    invalid(int, obj)
    invalid(int, array)

    invalid(long, float)
    invalid(long, double)
    invalid(long, string)
    invalid(long, obj)
    invalid(long, array)

    invalid(float, double)
    invalid(float, string)
    invalid(float, obj)
    invalid(float, array)

    invalid(double, string)
    invalid(double, obj)
    invalid(double, array)

    invalid(string, int)
    invalid(string, long)
    invalid(string, float)
    invalid(string, double)
    invalid(string, char)
    invalid(string, obj)

    invalid(obj, int)
    invalid(obj, long)
    invalid(obj, float)
    invalid(obj, double)
    invalid(obj, char)
    invalid(obj, string)

    invalid(array, string)
    invalid(array, array)
    invalid(array, obj)
    invalid(array, int)
    invalid(array, long)
    invalid(array, float)
    invalid(array, double)
    invalid(array, char)
  }

  def testComparisonOperator(expressionType: (ExprTree, ExprTree) => ExprTree) = {
    val asserter = new ComparisonAsserter(expressionType, TBool)
    import asserter._

    valid(char, char)

    valid(int, int)
    valid(int, long)
    valid(int, float)
    valid(int, double)
    valid(int, char)

    valid(long, long)
    valid(long, float)
    valid(long, double)
    valid(long, char)

    valid(float, float)
    valid(float, double)
    valid(float, char)

    valid(double, double)
    valid(double, char)

    invalid(obj, obj)
    invalid(obj, string)
    invalid(obj, int)
    invalid(obj, long)
    invalid(obj, float)
    invalid(obj, double)
    invalid(obj, char)

    invalid(string, string)
    invalid(string, obj)
    invalid(string, int)
    invalid(string, long)
    invalid(string, float)
    invalid(string, double)
    invalid(string, char)

    invalid(array, array)
    invalid(array, string)
    invalid(array, obj)
    invalid(array, int)
    invalid(array, long)
    invalid(array, float)
    invalid(array, double)
    invalid(array, char)
  }

  def testEqualsOperator(expressionType: (ExprTree, ExprTree) => ExprTree) = {
    val asserter = new ComparisonAsserter(expressionType, TBool)
    import asserter._

    valid(char, char)

    valid(int, int)
    valid(int, long)
    valid(int, float)
    valid(int, double)
    valid(int, char)

    valid(long, long)
    valid(long, float)
    valid(long, double)
    valid(long, char)

    valid(float, float)
    valid(float, double)
    valid(float, char)

    valid(double, double)
    valid(double, char)

    valid(string, string)

    valid(obj, obj)

    valid(array, array)

    invalid(obj, string)
    invalid(obj, int)
    invalid(obj, long)
    invalid(obj, float)
    invalid(obj, double)
    invalid(obj, char)

    invalid(string, obj)
    invalid(string, int)
    invalid(string, long)
    invalid(string, float)
    invalid(string, double)
    invalid(string, char)

    invalid(array, string)
    invalid(array, obj)
    invalid(array, int)
    invalid(array, long)
    invalid(array, float)
    invalid(array, double)
    invalid(array, char)
  }

  class BinaryExpressionAsserter(expressionType: (ExprTree, ExprTree) => ExprTree) {

    def valid(rhs: () => ExprTree, lhs: () => ExprTree, tpe: Type) = {
      testContext.reporter.clearErrors()
      typeChecker.tcExpr(expressionType(rhs(), lhs())) should be(tpe)
      typeChecker.tcExpr(expressionType(lhs(), rhs())) should be(tpe)
      testContext.reporter.hasErrors should be(false)
    }

    def invalid(rhs: () => ExprTree, lhs: () => ExprTree) = {
      typeChecker.tcExpr(expressionType(rhs(), lhs()))
      assertErrors()

      typeChecker.tcExpr(expressionType(lhs(), rhs()))
      assertErrors()
    }



  }

  class ComparisonAsserter(expressionType: (ExprTree, ExprTree) => ExprTree, tpe: Type) {

    def valid(rhs: () => ExprTree, lhs: () => ExprTree) = {
      testContext.reporter.clearErrors()
      typeChecker.tcExpr(expressionType(rhs(), lhs())) should be(tpe)
      typeChecker.tcExpr(expressionType(lhs(), rhs())) should be(tpe)
      testContext.reporter.hasErrors should be(false)
    }

    def invalid(rhs: () => ExprTree, lhs: () => ExprTree) = {
      typeChecker.tcExpr(expressionType(rhs(), lhs()))
      assertErrors()

      typeChecker.tcExpr(expressionType(lhs(), rhs()))
      assertErrors()
    }

  }


  class AssignmentAsserter(expressionType: (Identifier, ExprTree) => ExprTree) {

    def valid(id: () => Identifier, expr: () => ExprTree, tpe: Type) = {
      testContext.reporter.clearErrors()
      typeChecker.tcExpr(expressionType(id(), expr())) should be(tpe)
      testContext.reporter.hasErrors should be(false)
    }

    def invalid(id: () => Identifier, expr: () => ExprTree) = {
      typeChecker.tcExpr(expressionType(id(), expr()))
      assertErrors()
    }

  }

  def assertErrors() = {
    testContext.reporter.hasErrors should be(true)
    testContext.reporter.clearErrors()
  }

  def test(file: File, exception: Boolean = false) = {
    val options = TestUtils.readOptions(file)
    val ctx = new Context(reporter = new koolc.utils.Reporter, file = file, outDir = None)
    val quietCtx = ctx.copy(reporter = new koolc.utils.Reporter(false))
    val exec = Lexer andThen Parser andThen NameAnalysis andThen TypeChecking
    if (exception) {
      try {
        exec.run(quietCtx)(file)
        assert(false)
      } catch {
        case _: CompilationException => ctx.reporter.errors should be(options("expectedErrors"))
      }
    } else {
      val program = exec.run(ctx)(file)
      ctx.reporter.hasErrors should be(false)
      TestUtils.HasTypes(program) should be(true)
    }
  }

}