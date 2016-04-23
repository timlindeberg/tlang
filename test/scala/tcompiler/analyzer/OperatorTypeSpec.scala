package tcompiler.analyzer

import java.io.File

import org.scalatest._
import tcompiler.analyzer.Symbols.{ClassSymbol, MethodSymbol, VariableSymbol}
import tcompiler.analyzer.Types._
import tcompiler.ast.Trees._
import tcompiler.utils.Context

class OperatorTypeSpec extends FlatSpec with Matchers {

  val Flag = "--ast --symid"
  val MethodDecl = new MethodDecl(None, Identifier(""), List(), None, Set(Private()))
  val ClassSymbol = new ClassSymbol("obj")
  val VarSymbol   = new VariableSymbol("var")
  val MainMethod  = new MethodSymbol("main", ClassSymbol, MethodDecl).setType(TUnit)
  val TestContext = Context(new tcompiler.utils.Reporter(), None, new File(""))
  val TypeChecker = new TypeChecker(TestContext, MainMethod)


  val int    = new TypeConstructor(TInt)
  val bool   = new TypeConstructor(TBool)
  val long   = new TypeConstructor(TLong)
  val float  = new TypeConstructor(TFloat)
  val double = new TypeConstructor(TDouble)
  val char   = new TypeConstructor(TChar)
  val string = new TypeConstructor(TString)
  val array  = new TypeConstructor(TArray(TInt))
  val obj    = new TypeConstructor(TObject(ClassSymbol))

  val allTypes        = List[() => Identifier](int, bool, long, float, double, char, string, array, obj)
  val allCombinations = for (x <- allTypes; y <- allTypes) yield (x, y)

  private def createIdentifier(tpe: Type) = Identifier("").setSymbol(new VariableSymbol("")).setType(tpe)

  class TypeConstructor(val tpe: Type)
    extends (() => Identifier) {
    def apply(): Identifier = createIdentifier(tpe)
    override def toString() = tpe.toString
    def ==(rhs: TypeConstructor) = tpe.toString == rhs.tpe.toString
  }

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


  private def plusOperator() =
    BinaryExpressionAsserter.valid(Plus,
      (string, string, TString),
      (string, obj, TString),
      (string, int, TString),
      (string, long, TString),
      (string, float, TString),
      (string, double, TString),
      (string, char, TString),
      (string, bool, TString),
      (string, array, TString),

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

  private def binaryOperator(expressionType: (Identifier, Identifier) => ExprTree) =
    BinaryExpressionAsserter.valid(expressionType,
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

  private def logicOperator(expressionType: (Identifier, Identifier) => ExprTree) =
    BinaryExpressionAsserter.valid(expressionType,
      (int, int, TInt),
      (int, long, TLong),
      (int, char, TInt),

      (char, char, TInt),

      (long, long, TLong),
      (long, char, TLong),
      (bool, bool, TBool)
    )

  private def shiftOperator(expressionType: (Identifier, Identifier) => ExprTree) =
    BinaryExpressionAsserter.valid(expressionType,
      (int, int, TInt),
      (int, long, TLong),
      (int, char, TInt),

      (char, char, TInt),

      (long, long, TLong),
      (long, char, TLong)
    )

  private def assignOperator() =
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

  private def arrayAssignOperator() =
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

      (array, array, array().getType),

      (obj, obj, obj().getType)
    )

  private def comparisonOperator(expressionType: (Identifier, Identifier) => ExprTree) =
    BinaryExpressionAsserter.valid(expressionType,
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

  private def equalsOperator(expressionType: (Identifier, Identifier) => ExprTree) =
    BinaryExpressionAsserter.valid(expressionType,
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

  private def andOr(expressionType: (Identifier, Identifier) => ExprTree) =
    BinaryExpressionAsserter.valid(expressionType,
      (bool, bool, TBool)
    )

  private def not(expressionType: Identifier => ExprTree) =
    UnaryExpressionAsserter.valid(expressionType,
      (bool, TBool)
    )

  private def instance(expressionType: (Identifier, Identifier) => ExprTree) =
    BinaryExpressionAsserter.valid(expressionType,
      (obj, obj, TBool)
    )

  private def negation(expressionType: Identifier => ExprTree) =
    UnaryExpressionAsserter.valid(expressionType,
      (int, TInt),
      (char, TInt),
      (long, TLong),
      (float, TFloat),
      (double, TDouble)
    )

  private def logicalNot(expressionType: Identifier => ExprTree) =
    UnaryExpressionAsserter.valid(expressionType,
      (int, TInt),
      (char, TInt),
      (long, TLong)
    )

  private def incrementDecrement(expressionType: Identifier => ExprTree) =
    UnaryExpressionAsserter.valid(expressionType,
      (int, TInt),
      (char, TChar),
      (long, TLong),
      (float, TFloat),
      (double, TDouble)
    )



  object UnaryExpressionAsserter {

    def getInvalidCombinations(validTpes: List[(() => Identifier, Type)]) =
      allTypes.filter(tpe => {
        !validTpes.exists(listElem => tpe == listElem._1)
      })

    def valid(expressionType: Identifier => ExprTree, validTpes: (() => Identifier, Type)*): Unit = {
      validTpes.foreach { case (id, tpe) =>
        TestContext.reporter.clear()
        val resType = TypeChecker.tcExpr(expressionType(id()))
        assert(resType == tpe, "for " + id + "")

        val noErrors = !TestContext.reporter.hasErrors
        assert(noErrors, "for " + id + "")
      }

      getInvalidCombinations(validTpes.toList) foreach { id =>
        TypeChecker.tcExpr(expressionType(id()))
        val invalid = TestContext.reporter.hasErrors
        assert(invalid, "for " + id + "")
        TestContext.reporter.clear()
      }
    }
  }

  object BinaryExpressionAsserter {

    def valid(expressionType: (Identifier, Identifier) => ExprTree,
              validCombinations: (() => Identifier, () => Identifier, Type)*): Unit = {

      validCombinations.foreach { case (lhs, rhs, tpe) =>
        TestContext.reporter.clear()

        val resType1 = TypeChecker.tcExpr(expressionType(rhs(), lhs()))
        assert(resType1 == tpe, "for (" + rhs + ", " + lhs + ")")

        val resType2 = TypeChecker.tcExpr(expressionType(lhs(), rhs()))
        assert(resType2 == tpe, "for (" + lhs + ", " + rhs + ")")

        val noErrors = !TestContext.reporter.hasErrors
        assert(noErrors, "for (" + lhs + ", " + rhs + ")")
      }

      getInvalidCombinations(validCombinations.toList).foreach { case (lhs, rhs) =>
        TypeChecker.tcExpr(expressionType(lhs(), rhs()))
        val invalid = TestContext.reporter.hasErrors
        assert(invalid, "for (" + lhs + ", " + rhs + ")")
        TestContext.reporter.clear()
      }
    }

    private def getInvalidCombinations(validCombinations: List[(() => Identifier, () => Identifier, Type)]) =
      allCombinations.filter(combination => {
        !validCombinations.exists(listElem => {
          val tuple1 = (listElem._1, listElem._2)
          val tuple2 = (listElem._2, listElem._1)
          combination == tuple1 || combination == tuple2
        })
      })

  }

  class AssignmentAsserter(expressionType: (Identifier, Identifier) => ExprTree) {

    def valid(validCombinations: (() => Identifier, () => Identifier, Type)*): Unit = {
      validCombinations.foreach { case (id, expr, tpe) =>

        TestContext.reporter.clear()
        val resType = TypeChecker.tcExpr(expressionType(id(), expr()))
        assert(resType == tpe, "for (" + id + ", " + expr + ")")

        val noErrors = !TestContext.reporter.hasErrors
        assert(noErrors, "for (" + id + ", " + expr + ")")
      }

      getInvalidCombinations(validCombinations.toList) foreach { case (id, expr) =>
        TypeChecker.tcExpr(expressionType(id(), expr()))
        val invalid = TestContext.reporter.hasErrors

        assert(invalid, "for (" + id + ", " + expr + ")")
        TestContext.reporter.clear()
      }
    }

    protected def getInvalidCombinations(validCombinations: List[(() => Identifier, () => Identifier, Type)]) =
      allCombinations.filter(combination => {
        !validCombinations.exists(listElem => {
          val tuple1 = (listElem._1, listElem._2)
          combination == tuple1
        })
      })
  }

  class ArrayAssignmentAsserter() extends AssignmentAsserter(Assign) {

    override def valid(validCombinations: (() => Identifier, () => Identifier, Type)*) = {
      validCombinations.foreach { case (identifier, expr, tpe) =>
        TestContext.reporter.clear()
        val id = createIdentifier(TArray(identifier().getType))

        val resType = TypeChecker.tcExpr(ArrayAssign(id, IntLit(0), expr()))
        assert(resType == tpe, "for (" + identifier + ", " + expr + ")")

        val noErrors = !TestContext.reporter.hasErrors
        assert(noErrors, "for (" + identifier + ", " + expr + ")")
      }

      getInvalidCombinations(validCombinations.toList) foreach { case (identifier, expr) =>
        val id = createIdentifier(TArray(identifier().getType))
        TypeChecker.tcExpr(ArrayAssign(id, IntLit(0), expr()))
        val invalid = TestContext.reporter.hasErrors

        assert(invalid, "for (" + identifier + ", " + expr + ")")
        TestContext.reporter.clear()
      }
    }
  }
}