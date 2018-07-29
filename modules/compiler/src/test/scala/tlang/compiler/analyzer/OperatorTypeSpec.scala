package tlang
package compiler
package analyzer

import tlang.compiler.CompilerIntegrationTestSpec
import tlang.compiler.analyzer.Symbols.{ClassSymbol, MethodSymbol, VariableSymbol}
import tlang.compiler.analyzer.Types._
import tlang.compiler.ast.Trees._
import tlang.compiler.imports.Imports
import tlang.compiler.output.ErrorMessageOutput
import tlang.formatting.ErrorStringContext

class OperatorTypeSpec extends CompilerIntegrationTestSpec {

  import TestContext.formatter

  private val ClassSymbol  = new ClassSymbol("obj")
  private val MainMethod   = new MethodSymbol("main", ClassSymbol, None, Set(Public(), Static())).setType(TUnit)
  private val ErrorContext = ErrorStringContext()
  private val TestImports  = Imports(TestContext, ErrorContext)
  private val TypeChecker  = new TypeChecker(TestContext.reporter, ErrorContext, TestImports, MainMethod)

  private val int    = new TypeConstructor(Int)
  private val bool   = new TypeConstructor(Bool)
  private val long   = new TypeConstructor(Long)
  private val float  = new TypeConstructor(Float)
  private val double = new TypeConstructor(Double)
  private val char   = new TypeConstructor(Char)
  private val array  = new TypeConstructor(TArray(Int))
  private val obj    = new TypeConstructor(Object)

  private val allTypes        = List[TypeConstructor](int, bool, long, float, double, char, array, obj)
  private val allCombinations = for (x <- allTypes; y <- allTypes) yield (x, y)

  private def createIdentifier(tpe: Type) = VariableID("").setSymbol(new VariableSymbol("")).setType(tpe)

  class TypeConstructor(val tpe: Type) {
    def apply(): VariableID = createIdentifier(tpe)
    override def toString: String = tpe.toString
    def ==(rhs: TypeConstructor): Boolean = tpe.toString == rhs.tpe.toString
  }


  "Plus" in { arithmeticOperator(Plus) }
  "Minus" in { arithmeticOperator(Minus) }
  "Times" in { arithmeticOperator(Times) }
  "Div" in { arithmeticOperator(Div) }
  "Modulo" in { arithmeticOperator(Modulo) }

  "LogicAnd" in { logicOperator(LogicAnd) }
  "LogicOr" in { logicOperator(LogicOr) }
  "LogicXor" in { logicOperator(LogicXor) }

  "LeftShift" in { shiftOperator(LeftShift) }
  "RightShift" in { shiftOperator(RightShift) }

  "Assign" in { assignOperator() }
  "ArrayAssign" in { arrayAssignOperator() }

  "LessThan" in { comparisonOperator(LessThan) }
  "LessThanEquals" in { comparisonOperator(LessThanEquals) }
  "GreaterThan" in { comparisonOperator(GreaterThan) }
  "GreaterThanEquals" in { comparisonOperator(GreaterThanEquals) }


  "Equals" in { equalsOperator(Equals) }
  "NotEquals" in { equalsOperator(NotEquals) }

  "And" in { andOr(And) }
  "Or" in { andOr(Or) }
  "Not" in { not(Not) }

  "Negation" in { negation(Negation) }
  "LogicalNot" in { logicalNot(LogicNot) }

  "PreIncrement" in { incrementDecrement(PreIncrement) }
  "PostIncrement" in { incrementDecrement(PostIncrement) }
  "PreDecrement" in { incrementDecrement(PreDecrement) }
  "PostDecrement" in { incrementDecrement(PostDecrement) }

  private def arithmeticOperator(expressionType: (VariableID, VariableID) => BinaryOperatorTree): Unit =
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

  private def logicOperator(expressionType: (VariableID, VariableID) => LogicalOperatorTree): Unit =
    BinaryExpressionAsserter.valid(expressionType,
      (int, int, Int),
      (int, long, Long),
      (int, char, Int),

      (char, char, Int),

      (long, long, Long),
      (long, char, Long),
      (bool, bool, Bool)
    )

  private def shiftOperator(expressionType: (VariableID, VariableID) => ShiftOperatorTree): Unit =
    BinaryExpressionAsserter.valid(expressionType,
      (int, int, Int),
      (int, long, Long),
      (int, char, Int),

      (char, char, Int),

      (long, long, Long),
      (long, char, Long)
    )

  private def assignOperator(): Unit =
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
      (obj, array, Object),

      (array, array, array().getType)
    )


  private def arrayAssignOperator(): Unit =
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
      (obj, array, Object),

      (array, array, array().getType)


    )

  private def comparisonOperator(expressionType: (VariableID, VariableID) => ComparisonOperatorTree): Unit =
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

  private def equalsOperator(expressionType: (VariableID, VariableID) => EqualsOperatorTree): Unit =
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
      (obj, array, Bool),

      (array, array, Bool),

      (bool, bool, Bool)
    )

  private def andOr(expressionType: (VariableID, VariableID) => BinaryOperatorTree): Unit =
    BinaryExpressionAsserter.valid(expressionType,
      (bool, bool, Bool)
    )

  private def not(expressionType: VariableID => UnaryOperatorTree): Unit =
    UnaryExpressionAsserter.valid(expressionType,
      (bool, Bool)
    )

  private def negation(expressionType: VariableID => UnaryOperatorTree): Unit =
    UnaryExpressionAsserter.valid(expressionType,
      (int, Int),
      (char, Int),
      (long, Long),
      (float, Float),
      (double, Double)
    )

  private def logicalNot(expressionType: VariableID => UnaryOperatorTree): Unit =
    UnaryExpressionAsserter.valid(expressionType,
      (int, Int),
      (char, Int),
      (long, Long)
    )

  private def incrementDecrement(expressionType: VariableID => UnaryOperatorTree): Unit =
    UnaryExpressionAsserter.valid(expressionType,
      (int, Int),
      (char, Char),
      (long, Long),
      (float, Float),
      (double, Double)
    )


  object UnaryExpressionAsserter {

    def getInvalidCombinations(validTpes: List[(TypeConstructor, Type)]): List[TypeConstructor] =
      allTypes.filter(tpe => {
        !validTpes.exists(listElem => tpe == listElem._1)
      })

    def valid(expressionType: VariableID => UnaryOperatorTree, validTpes: (TypeConstructor, Type)*): Unit = {
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

    def valid(expressionType: (VariableID, VariableID) => BinaryOperatorTree,
      validCombinations: (TypeConstructor, TypeConstructor, Type)*): Unit = {

      val reporter = TestContext.reporter
      validCombinations.foreach { case (lhs, rhs, tpe) =>

        reporter.clear()

        val resType1 = TypeChecker.tcExpr(expressionType(rhs(), lhs()))
        assert(resType1 == tpe, "for (" + rhs + ", " + lhs + ")")

        val resType2 = TypeChecker.tcExpr(expressionType(lhs(), rhs()))
        assert(resType2 == tpe, "for (" + lhs + ", " + rhs + ")")

        if (reporter.hasErrors)
          TestContext.output += ErrorMessageOutput(reporter.messages)

        assert(!reporter.hasErrors, "for (" + lhs + ", " + rhs + ")")
      }

      getInvalidCombinations(validCombinations.toList).foreach { case (lhs, rhs) =>
        TypeChecker.tcExpr(expressionType(lhs(), rhs()))
        val invalid = TestContext.reporter.hasErrors
        assert(invalid, "for (" + lhs + ", " + rhs + ")")
        TestContext.reporter.clear()
      }
    }

    private def getInvalidCombinations(validCombinations: List[(TypeConstructor, TypeConstructor, Type)]) =
      allCombinations.filter(combination => {
        !validCombinations.exists(listElem => {
          val tuple1 = (listElem._1, listElem._2)
          val tuple2 = (listElem._2, listElem._1)
          combination == tuple1 || combination == tuple2
        })
      })

  }

  class AssignmentAsserter(expressionType: (VariableID, VariableID) => ExprTree) {

    def valid(validCombinations: (TypeConstructor, TypeConstructor, Type)*): Unit = {
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

    protected def getInvalidCombinations(validCombinations: List[(TypeConstructor, TypeConstructor, Type)]): List[(TypeConstructor, TypeConstructor)] =
      allCombinations.filter(combination => {
        !validCombinations.exists(listElem => {
          val tuple1 = (listElem._1, listElem._2)
          combination == tuple1
        })
      })
  }

  class ArrayAssignmentAsserter() extends AssignmentAsserter(Assign) {

    override def valid(validCombinations: (TypeConstructor, TypeConstructor, Type)*): Unit = {
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
