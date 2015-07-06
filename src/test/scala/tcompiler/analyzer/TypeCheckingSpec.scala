package tcompiler.analyzer

import java.io.File

import tcompiler.TestUtils
import tcompiler.analyzer.Symbols.{VariableSymbol, ClassSymbol, MethodSymbol}
import tcompiler.analyzer.Types._
import tcompiler.ast.Trees._
import tcompiler.ast._
import tcompiler.lexer.Lexer
import tcompiler.utils.{CompilationException, Context}
import org.scalatest._

class TypeCheckingSpec extends FlatSpec with Matchers with BeforeAndAfter {
  val flag = "--ast --symid"

  before {
    Symbols.ID.reset()
  }

  behavior of "Positive tests"
  TestUtils.programFiles(TestUtils.resources + "analyzer/type/valid/").foreach { file =>
    it should "type check program " + file.toPath in test(file)
  }
  TestUtils.programFiles(TestUtils.resources + "programs/").foreach { file =>
    it should "type check program " + file.toPath in test(file)
  }
  TestUtils.programFiles(TestUtils.resources + "given/programs/").foreach { file =>
    it should "type check given program " + file.toPath in test(file)
  }

  behavior of "Negative tests"
  TestUtils.programFiles(TestUtils.resources + "analyzer/type/invalid/").foreach { file =>
    it should "type check invalid program " + file.toPath in test(file, exception = true)
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
  val testContext = Context(new tcompiler.utils.Reporter(), None, new File(""))
  val typeChecker = new TypeChecker(testContext, mainMethod)

  def createIdentifier(tpe: Type) = Identifier("").setSymbol(new VariableSymbol("")).setType(tpe)

  class TypeConstructor(val tpe: Type)
    extends (() => Identifier) {
    def apply(): Identifier = createIdentifier(tpe)
    override def toString() = tpe.toString
    def ==(rhs: TypeConstructor) = tpe.toString == rhs.tpe.toString
  }


  val int    = new TypeConstructor(TInt)
  val bool   = new TypeConstructor(TBool)
  val long   = new TypeConstructor(TLong)
  val float  = new TypeConstructor(TFloat)
  val double = new TypeConstructor(TDouble)
  val char   = new TypeConstructor(TChar)
  val string = new TypeConstructor(TString)
  val array  = new TypeConstructor(TArray(TInt))
  val obj    = new TypeConstructor(TObject(classSymbol))

  val allTypes        = List[() => Identifier](int, bool, long, float, double, char, string, array, obj)
  val allCombinations = for (x <- allTypes; y <- allTypes) yield (x, y)


  behavior of "Operators"

  it should "Plus" in plusOperator
  it should "Minus" in binaryOperator(Minus)
  it should "Times" in binaryOperator(Times)
  it should "Div" in binaryOperator(Div)
  it should "Modulo" in binaryOperator(Modulo)

  it should "LogicAnd" in logicOperator(LogicAnd)
  it should "LogicOr" in logicOperator(LogicOr)
  it should "LogicXor" in logicOperator(LogicXor)
  it should "LeftShift" in shiftOperator(LeftShift)
  it should "RightShift" in shiftOperator(RightShift)

  it should "Assign" in assignOperator
  it should "PlusAssign" in plusAssignOperator
  it should "MinusAssign" in assignmentOperator(MinusAssign)
  it should "MulAssign" in assignmentOperator(MulAssign)
  it should "DivAssign" in assignmentOperator(DivAssign)
  it should "ModAssign" in assignmentOperator(ModAssign)

  it should "AndAssign" in logicalAssignmentOperator(AndAssign)
  it should "OrAssign" in logicalAssignmentOperator(OrAssign)
  it should "XorAssign" in logicalAssignmentOperator(XorAssign)
  it should "LeftShiftAssign" in shiftAssignmentOperator(LeftShiftAssign)
  it should "RightShiftAssign" in shiftAssignmentOperator(RightShiftAssign)
  it should "ArrayAssign" in arrayAssignOperator

  it should "LessThan" in comparisonOperator(LessThan)
  it should "LessThanEquals" in comparisonOperator(LessThanEquals)
  it should "GreaterThan" in comparisonOperator(GreaterThan)
  it should "GreaterThanEquals" in comparisonOperator(GreaterThanEquals)
  it should "Equals" in equalsOperator(Equals)
  it should "NotEquals" in equalsOperator(NotEquals)

  it should "And" in andOr(And)
  it should "Or" in andOr(Or)
  it should "Not" in not(Not)

  it should "Instance" in instance(Instance)

  it should "Negation" in negation(Negation)
  it should "LogicalNot" in logicalNot(LogicNot)

  it should "PreIncrement" in incrementDecrement(PreIncrement)
  it should "PostIncrement" in incrementDecrement(PostIncrement)
  it should "PreDecrement" in incrementDecrement(PreDecrement)
  it should "PostDecrement" in incrementDecrement(PostDecrement)


  def plusOperator() =
    new BinaryExpressionAsserter(Plus).valid(
      (string, string, TString),
      (string, obj, TString),
      (string, int, TString),
      (string, long, TString),
      (string, float, TString),
      (string, double, TString),
      (string, char, TString),
      (string, bool, TString),

      (char, char, TInt),

      (int, int, TInt),
      (int, long, TLong),
      (int, float, TFloat),
      (int, double, TDouble),
      (int, char, TInt),

      (long, long, TLong),
      (long, float, TFloat),
      (long, double, TDouble),
      (long, char, TLong),

      (float, float, TFloat),
      (float, double, TDouble),
      (float, char, TFloat),

      (double, double, TDouble),
      (double, char, TDouble)
    )

  def binaryOperator(expressionType: (Identifier, Identifier) => ExprTree) =
    new BinaryExpressionAsserter(expressionType).valid(
      (char, char, TInt),

      (int, int, TInt),
      (int, long, TLong),
      (int, float, TFloat),
      (int, double, TDouble),
      (int, char, TInt),

      (long, long, TLong),
      (long, float, TFloat),
      (long, double, TDouble),
      (long, char, TLong),

      (float, float, TFloat),
      (float, double, TDouble),
      (float, char, TFloat),

      (double, double, TDouble),
      (double, char, TDouble)
    )

  def logicOperator(expressionType: (Identifier, Identifier) => ExprTree) =
    new BinaryExpressionAsserter(expressionType).valid(
      (int, int, TInt),
      (int, long, TLong),
      (int, char, TInt),

      (char, char, TInt),

      (long, long, TLong),
      (long, char, TLong),
      (bool, bool, TBool)
    )

  def shiftOperator(expressionType: (Identifier, Identifier) => ExprTree) =
    new BinaryExpressionAsserter(expressionType).valid(
      (int, int, TInt),
      (int, long, TLong),
      (int, char, TInt),

      (char, char, TInt),

      (long, long, TLong),
      (long, char, TLong)
    )

  def assignOperator() =
    new AssignmentAsserter(Assign).valid(
      (bool, bool, TBool),

      (char, char, TChar),
      (char, int, TChar),

      (int, int, TInt),
      (int, char, TInt),

      (long, long, TLong),
      (long, int, TLong),
      (long, char, TLong),

      (float, float, TFloat),
      (float, int, TFloat),
      (float, char, TFloat),
      (float, long, TFloat),

      (double, double, TDouble),
      (double, float, TDouble),
      (double, int, TDouble),
      (double, long, TDouble),
      (double, char, TDouble),

      (string, string, TString),

      (obj, obj, obj().getType),

      (array, array, array().getType)
    )

  def plusAssignOperator() =
    new AssignmentAsserter(PlusAssign).valid(
      (int, int, TInt),
      (int, char, TInt),

      (long, long, TLong),
      (long, int, TLong),
      (long, char, TLong),

      (float, float, TFloat),
      (float, int, TFloat),
      (float, char, TFloat),
      (float, long, TFloat),

      (double, double, TDouble),
      (double, float, TDouble),
      (double, int, TDouble),
      (double, long, TDouble),
      (double, char, TDouble),

      (string, string, TString),
      (string, int, TString),
      (string, long, TString),
      (string, float, TString),
      (string, double, TString),
      (string, char, TString)
    )

  def assignmentOperator(expressionType: (Identifier, Identifier) => ExprTree) =
    new AssignmentAsserter(expressionType).valid(
      (int, int, TInt),
      (int, char, TInt),

      (long, long, TLong),
      (long, int, TLong),
      (long, char, TLong),

      (float, float, TFloat),
      (float, int, TFloat),
      (float, char, TFloat),
      (float, long, TFloat),

      (double, double, TDouble),
      (double, float, TDouble),
      (double, int, TDouble),
      (double, long, TDouble),
      (double, char, TDouble)
    )

  def logicalAssignmentOperator(expressionType: (Identifier, Identifier) => ExprTree) =
    new AssignmentAsserter(expressionType).valid(
      (int, int, TInt),
      (int, char, TInt),

      (long, long, TLong),
      (long, int, TLong),
      (long, char, TLong),

      (bool, bool, TBool)
    )

  def shiftAssignmentOperator(expressionType: (Identifier, Identifier) => ExprTree) =
    new AssignmentAsserter(expressionType).valid(
      (int, int, TInt),
      (int, char, TInt),

      (long, long, TLong),
      (long, int, TLong),
      (long, char, TLong)
    )

  def arrayAssignOperator() =
    new ArrayAssignmentAsserter().valid(
      (char, char, TChar),
      (char, int, TChar),

      (int, int, TInt),
      (int, char, TInt),

      (long, long, TLong),
      (long, int, TLong),
      (long, char, TLong),

      (bool, bool, TBool),

      (float, float, TFloat),
      (float, int, TFloat),
      (float, char, TFloat),
      (float, long, TFloat),

      (double, double, TDouble),
      (double, float, TDouble),
      (double, int, TDouble),
      (double, long, TDouble),
      (double, char, TDouble),

      (string, string, TString),

      (obj, obj, obj().getType)
    )

  def comparisonOperator(expressionType: (Identifier, Identifier) => ExprTree) =
    new BinaryExpressionAsserter(expressionType).valid(
      (char, char, TBool),

      (int, int, TBool),
      (int, long, TBool),
      (int, float, TBool),
      (int, double, TBool),
      (int, char, TBool),

      (long, long, TBool),
      (long, float, TBool),
      (long, double, TBool),
      (long, char, TBool),

      (float, float, TBool),
      (float, double, TBool),
      (float, char, TBool),

      (double, double, TBool),
      (double, char, TBool)
    )

  def equalsOperator(expressionType: (Identifier, Identifier) => ExprTree) =
    new BinaryExpressionAsserter(expressionType).valid(
      (char, char, TBool),

      (int, int, TBool),
      (int, long, TBool),
      (int, float, TBool),
      (int, double, TBool),
      (int, char, TBool),

      (long, long, TBool),
      (long, float, TBool),
      (long, double, TBool),
      (long, char, TBool),

      (float, float, TBool),
      (float, double, TBool),
      (float, char, TBool),

      (double, double, TBool),
      (double, char, TBool),

      (string, string, TBool),

      (obj, obj, TBool),

      (array, array, TBool),

      (bool, bool, TBool)
    )

  def andOr(expressionType: (Identifier, Identifier) => ExprTree) =
    new BinaryExpressionAsserter(expressionType).valid(
      (bool, bool, TBool)
    )

  def not(expressionType: Identifier => ExprTree) =
    new UnaryExpressionAsserter(expressionType).valid(
      (bool, TBool)
    )

  def instance(expressionType: (Identifier, Identifier) => ExprTree) =
    new BinaryExpressionAsserter(expressionType).valid(
      (obj, obj, TBool)
    )

  def negation(expressionType: Identifier => ExprTree) =
    new UnaryExpressionAsserter(expressionType).valid(
      (int, TInt),
      (char, TInt),
      (long, TLong),
      (float, TFloat),
      (double, TDouble)
    )

  def logicalNot(expressionType: Identifier => ExprTree) =
    new UnaryExpressionAsserter(expressionType).valid(
      (int, TInt),
      (char, TInt),
      (long, TLong)
    )

  def incrementDecrement(expressionType: Identifier => ExprTree) =
    new UnaryExpressionAsserter(expressionType).valid(
      (int, TInt),
      (char, TChar),
      (long, TLong),
      (float, TFloat),
      (double, TDouble)
    )



  class UnaryExpressionAsserter(expressionType: Identifier => ExprTree) {

    def getInvalidCombinations(validTpes: List[(() => Identifier, Type)]) =
      allTypes.filter(tpe => {
        !validTpes.exists(listElem => tpe == listElem._1)
      })

    def valid(validTpes: (() => Identifier, Type)*): Unit = {

      validTpes.foreach { case (id, tpe) =>
        testContext.reporter.clearErrors()

        testContext.reporter.clearErrors()
        val resType = typeChecker.tcExpr(expressionType(id()))
        assert(resType == tpe, "for " + id + "")

        val noErrors = !testContext.reporter.hasErrors
        assert(noErrors, "for " + id + "")
      }

      getInvalidCombinations(validTpes.toList) foreach { id =>
        typeChecker.tcExpr(expressionType(id()))
        val invalid = testContext.reporter.hasErrors
        assert(invalid, "for " + id + "")
        testContext.reporter.clearErrors()
      }
    }
  }

  class BinaryExpressionAsserter(expressionType: (Identifier, Identifier) => ExprTree) {

    def getInvalidCombinations(validCombinations: List[(() => Identifier, () => Identifier, Type)]) =
      allCombinations.filter(combination => {
        !validCombinations.exists(listElem => {
          val tuple1 = (listElem._1, listElem._2)
          val tuple2 = (listElem._2, listElem._1)
          combination == tuple1 || combination == tuple2
        })
      })

    def valid(validCombinations: (() => Identifier, () => Identifier, Type)*): Unit = {

      validCombinations.foreach { case (lhs, rhs, tpe) =>
        testContext.reporter.clearErrors()

        val resType1 = typeChecker.tcExpr(expressionType(rhs(), lhs()))
        assert(resType1 == tpe, "for (" + rhs + ", " + lhs + ")")

        val resType2 = typeChecker.tcExpr(expressionType(lhs(), rhs()))
        assert(resType2 == tpe, "for (" + lhs + ", " + rhs + ")")

        val noErrors = !testContext.reporter.hasErrors
        assert(noErrors, "for (" + lhs + ", " + rhs + ")")
      }

      getInvalidCombinations(validCombinations.toList) foreach { case (lhs, rhs) =>
        typeChecker.tcExpr(expressionType(rhs(), lhs()))
        val invalid = testContext.reporter.hasErrors
        assert(invalid, "for (" + lhs + ", " + rhs + ")")
        testContext.reporter.clearErrors()
      }
    }
  }

  class AssignmentAsserter(expressionType: (Identifier, Identifier) => ExprTree) {

    def invalidCombinations(validCombinations: List[(() => Identifier, () => Identifier, Type)]) =
      allCombinations.filter(combination => {
        !validCombinations.exists(listElem => {
          val tuple1 = (listElem._1, listElem._2)
          combination == tuple1
        })
      })

    def valid(validCombinations: (() => Identifier, () => Identifier, Type)*): Unit = {
      validCombinations.foreach { case (id, expr, tpe) =>

        testContext.reporter.clearErrors()
        val resType = typeChecker.tcExpr(expressionType(id(), expr()))
        assert(resType == tpe, "for (" + id + ", " + expr + ")")

        val noErrors = !testContext.reporter.hasErrors
        assert(noErrors, "for (" + id + ", " + expr + ")")
      }

      invalidCombinations(validCombinations.toList) foreach { case (id, expr) =>
        typeChecker.tcExpr(expressionType(id(), expr()))
        val invalid = testContext.reporter.hasErrors
        assert(invalid, "for (" + id + ", " + expr + ")")
        testContext.reporter.clearErrors()
      }
    }
  }

  class ArrayAssignmentAsserter() extends AssignmentAsserter(Assign) {

    override def valid(validCombinations: (() => Identifier, () => Identifier, Type)*) = {
      validCombinations.foreach { case (identifier, expr, tpe) =>
        testContext.reporter.clearErrors()
        val id = createIdentifier(TArray(identifier().getType))

        val resType = typeChecker.tcExpr(ArrayAssign(id, IntLit(0), expr()))
        assert(resType == tpe, "for (" + identifier + ", " + expr + ")")

        val noErrors = !testContext.reporter.hasErrors
        assert(noErrors, "for (" + identifier + ", " + expr + ")")
      }

      invalidCombinations(validCombinations.toList) foreach { case (identifier, expr) =>
        val id = createIdentifier(TArray(identifier().getType))
        typeChecker.tcExpr(ArrayAssign(id, IntLit(0), expr()))
        val invalid = testContext.reporter.hasErrors
        assert(invalid, "for (" + identifier + ", " + expr + ")")
        testContext.reporter.clearErrors()
      }
    }
  }

  def test(file: File, exception: Boolean = false) = {
    val options = TestUtils.readOptions(file)
    val ctx = new Context(reporter = new tcompiler.utils.Reporter, file = file, outDir = None)
    val quietCtx = ctx.copy(reporter = new tcompiler.utils.Reporter(false))
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