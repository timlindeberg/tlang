package tcompiler.analyzer

import org.scalatest._
import tcompiler.Tester
import tcompiler.analyzer.Symbols.{ClassSymbol, MethodSymbol, VariableSymbol}
import tcompiler.analyzer.Types._
import tcompiler.ast.Trees._
import tcompiler.imports.ImportMap
import tcompiler.utils.Context

class OperatorTypeSpec extends FlatSpec with Matchers {

  val ClassSymbol               = new ClassSymbol("obj", false)
  val MainMethod : MethodSymbol = new MethodSymbol("main", ClassSymbol, None, Set(Public(), Static())).setType(TUnit)
  val TestContext: Context      = Tester.testContext
  val TestImportMap             = new ImportMap(TestContext)
  val TypeChecker               = new TypeChecker(TestContext, TestImportMap, MainMethod)


  val int    = new TypeConstructor(Int)
  val bool   = new TypeConstructor(Bool)
  val long   = new TypeConstructor(Long)
  val float  = new TypeConstructor(Float)
  val double = new TypeConstructor(Double)
  val char   = new TypeConstructor(Char)
  val array  = new TypeConstructor(TArray(Int))
  val obj    = new TypeConstructor(Object)

  val allTypes       : List[() => VariableID]                     = List[() => VariableID](int, bool, long, float, double, char, array, obj)
  val allCombinations: List[(() => VariableID, () => VariableID)] = for (x <- allTypes; y <- allTypes) yield (x, y)

  private def createIdentifier(tpe: Type) = VariableID("").setSymbol(new VariableSymbol("")).setType(tpe)

  class TypeConstructor(val tpe: Type)
    extends (() => VariableID) {
    def apply(): VariableID = createIdentifier(tpe)
    override def toString(): String = tpe.toString
    def ==(rhs: TypeConstructor): Boolean = tpe.toString == rhs.tpe.toString
  }

  behavior of "Operators"

  it should "Plus" in arithmeticOperator(Plus)
  it should "Minus" in arithmeticOperator(Minus)
  it should "Times" in arithmeticOperator(Times)
  it should "Div" in arithmeticOperator(Div)
  it should "Modulo" in arithmeticOperator(Modulo)

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

  it should "Negation" in negation(Negation)
  it should "LogicalNot" in logicalNot(LogicNot)

  it should "PreIncrement" in incrementDecrement(PreIncrement)
  it should "PostIncrement" in incrementDecrement(PostIncrement)
  it should "PreDecrement" in incrementDecrement(PreDecrement)
  it should "PostDecrement" in incrementDecrement(PostDecrement)


  private def arithmeticOperator(expressionType: (VariableID, VariableID) => ExprTree) =
    BinaryExpressionAsserter.valid(expressionType,
      (char, char, Int),

      (int, int, Int),
      (int, long, Long),
      (int, float, Float),
      (int, double, Double),
      (int, char, Int),

      (long, long, Long),
      (long, float, Float),
      (long, double, Double),
      (long, char, Long),

      (float, float, Float),
      (float, double, Double),
      (float, char, Float),

      (double, double, Double),
      (double, char, Double)
    )

  private def logicOperator(expressionType: (VariableID, VariableID) => ExprTree) =
    BinaryExpressionAsserter.valid(expressionType,
      (int, int, Int),
      (int, long, Long),
      (int, char, Int),

      (char, char, Int),

      (long, long, Long),
      (long, char, Long),
      (bool, bool, Bool)
    )

  private def shiftOperator(expressionType: (VariableID, VariableID) => ExprTree) =
    BinaryExpressionAsserter.valid(expressionType,
      (int, int, Int),
      (int, long, Long),
      (int, char, Int),

      (char, char, Int),

      (long, long, Long),
      (long, char, Long)
    )

  private def assignOperator() =
    new AssignmentAsserter(Assign).valid(
      (bool, bool, Bool),

      (char, char, Char),
      (char, int, Char),

      (int, int, Int),
      (int, char, Int),

      (long, long, Long),
      (long, int, Long),
      (long, char, Long),

      (float, float, Float),
      (float, int, Float),
      (float, char, Float),
      (float, long, Float),

      (double, double, Double),
      (double, float, Double),
      (double, int, Double),
      (double, long, Double),
      (double, char, Double),

      (obj, obj, Object),
      (obj, double, Object),
      (obj, float, Object),
      (obj, int, Object),
      (obj, long, Object),
      (obj, char, Object),
      (obj, bool, Object),

      (array, array, array().getType)
    )


  private def arrayAssignOperator() =
    new ArrayAssignmentAsserter().valid(
      (char, char, Char),
      (char, int, Char),

      (int, int, Int),
      (int, char, Int),

      (long, long, Long),
      (long, int, Long),
      (long, char, Long),

      (bool, bool, Bool),

      (float, float, Float),
      (float, int, Float),
      (float, char, Float),
      (float, long, Float),

      (double, double, Double),
      (double, float, Double),
      (double, int, Double),
      (double, long, Double),
      (double, char, Double),

      (obj, obj, Object),
      (obj, double, Object),
      (obj, float, Object),
      (obj, int, Object),
      (obj, long, Object),
      (obj, char, Object),
      (obj, bool, Object),

      (array, array, array().getType)


    )

  private def comparisonOperator(expressionType: (VariableID, VariableID) => ExprTree) =
    BinaryExpressionAsserter.valid(expressionType,
      (char, char, Bool),

      (int, int, Bool),
      (int, long, Bool),
      (int, float, Bool),
      (int, double, Bool),
      (int, char, Bool),

      (long, long, Bool),
      (long, float, Bool),
      (long, double, Bool),
      (long, char, Bool),

      (float, float, Bool),
      (float, double, Bool),
      (float, char, Bool),

      (double, double, Bool),
      (double, char, Bool)
    )

  private def equalsOperator(expressionType: (VariableID, VariableID) => ExprTree) =
    BinaryExpressionAsserter.valid(expressionType,
      (char, char, Bool),

      (int, int, Bool),
      (int, long, Bool),
      (int, float, Bool),
      (int, double, Bool),
      (int, char, Bool),

      (long, long, Bool),
      (long, float, Bool),
      (long, double, Bool),
      (long, char, Bool),

      (float, float, Bool),
      (float, double, Bool),
      (float, char, Bool),

      (double, double, Bool),
      (double, char, Bool),

      (obj, obj, Bool),
      (obj, double, Bool),
      (obj, float, Bool),
      (obj, int, Bool),
      (obj, long, Bool),
      (obj, char, Bool),
      (obj, bool, Bool),

      (array, array, Bool),

      (bool, bool, Bool)
    )

  private def andOr(expressionType: (VariableID, VariableID) => ExprTree) =
    BinaryExpressionAsserter.valid(expressionType,
      (bool, bool, Bool)
    )

  private def not(expressionType: VariableID => ExprTree) =
    UnaryExpressionAsserter.valid(expressionType,
      (bool, Bool)
    )

  private def negation(expressionType: VariableID => ExprTree) =
    UnaryExpressionAsserter.valid(expressionType,
      (int, Int),
      (char, Int),
      (long, Long),
      (float, Float),
      (double, Double)
    )

  private def logicalNot(expressionType: VariableID => ExprTree) =
    UnaryExpressionAsserter.valid(expressionType,
      (int, Int),
      (char, Int),
      (long, Long)
    )

  private def incrementDecrement(expressionType: VariableID => ExprTree) =
    UnaryExpressionAsserter.valid(expressionType,
      (int, Int),
      (char, Char),
      (long, Long),
      (float, Float),
      (double, Double)
    )


  object UnaryExpressionAsserter {

    def getInvalidCombinations(validTpes: List[(() => VariableID, Type)]): List[() => VariableID] =
      allTypes.filter(tpe => {
        !validTpes.exists(listElem => tpe == listElem._1)
      })

    def valid(expressionType: VariableID => ExprTree, validTpes: (() => VariableID, Type)*): Unit = {
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

    def valid(expressionType: (VariableID, VariableID) => ExprTree,
      validCombinations: (() => VariableID, () => VariableID, Type)*): Unit = {

      validCombinations.foreach { case (lhs, rhs, tpe) =>
        TestContext.reporter.clear()

        val resType1 = TypeChecker.tcExpr(expressionType(rhs(), lhs()))
        assert(resType1 == tpe, "for (" + rhs + ", " + lhs + ")")

        val resType2 = TypeChecker.tcExpr(expressionType(lhs(), rhs()))
        assert(resType2 == tpe, "for (" + lhs + ", " + rhs + ")")

        val noErrors = !TestContext.reporter.hasErrors
        if (!noErrors) {
          println(TestContext.reporter.errorsString)
        }
        assert(noErrors, "for (" + lhs + ", " + rhs + ")")
      }

      getInvalidCombinations(validCombinations.toList).foreach { case (lhs, rhs) =>
        TypeChecker.tcExpr(expressionType(lhs(), rhs()))
        val invalid = TestContext.reporter.hasErrors
        assert(invalid, "for (" + lhs + ", " + rhs + ")")
        TestContext.reporter.clear()
      }
    }

    private def getInvalidCombinations(validCombinations: List[(() => VariableID, () => VariableID, Type)]) =
      allCombinations.filter(combination => {
        !validCombinations.exists(listElem => {
          val tuple1 = (listElem._1, listElem._2)
          val tuple2 = (listElem._2, listElem._1)
          combination == tuple1 || combination == tuple2
        })
      })

  }

  class AssignmentAsserter(expressionType: (VariableID, VariableID) => ExprTree) {

    def valid(validCombinations: (() => VariableID, () => VariableID, Type)*): Unit = {
      validCombinations.foreach { case (id, expr, tpe) =>

        TestContext.reporter.clear()
        val resType = TypeChecker.tcExpr(expressionType(id(), expr()))
        assert(resType == tpe, s"for $id = $expr")

        val noErrors = !TestContext.reporter.hasErrors
        assert(noErrors, s"for $id = $expr")
      }

      getInvalidCombinations(validCombinations.toList) foreach { case (id, expr) =>
        TypeChecker.tcExpr(expressionType(id(), expr()))
        val invalid = TestContext.reporter.hasErrors

        assert(invalid, s"for $id = $expr")
        TestContext.reporter.clear()
      }
    }

    protected def getInvalidCombinations(validCombinations: List[(() => VariableID, () => VariableID, Type)]): List[(() => VariableID, () => VariableID)] =
      allCombinations.filter(combination => {
        !validCombinations.exists(listElem => {
          val tuple1 = (listElem._1, listElem._2)
          combination == tuple1
        })
      })
  }

  class ArrayAssignmentAsserter() extends AssignmentAsserter(Assign) {

    override def valid(validCombinations: (() => VariableID, () => VariableID, Type)*): Unit = {
      validCombinations.foreach { case (identifier, expr, tpe) =>
        TestContext.reporter.clear()
        val id = createIdentifier(TArray(identifier().getType))

        val resType = TypeChecker.tcExpr(Assign(ArrayRead(id, IntLit(0)), expr()))
        assert(resType == tpe, "for (" + identifier + ", " + expr + ")")

        val noErrors = !TestContext.reporter.hasErrors
        assert(noErrors, "for (" + identifier + ", " + expr + ")")
      }

      getInvalidCombinations(validCombinations.toList) foreach { case (identifier, expr) =>
        val id = createIdentifier(TArray(identifier().getType))
        TypeChecker.tcExpr(Assign(ArrayRead(id, IntLit(0)), expr()))
        val invalid = TestContext.reporter.hasErrors

        assert(invalid, "for (" + identifier + ", " + expr + ")")
        TestContext.reporter.clear()
      }
    }
  }
}