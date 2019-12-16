package tlang
package compiler
package ast

import better.files.File
import org.scalatest.AppendedClues
import tlang.compiler.ast.Trees._
import tlang.compiler.lexer.Lexing
import tlang.compiler.messages.CompilationException
import tlang.compiler.output.ErrorMessageOutput
import tlang.compiler.testutils.PositionTest
import tlang.utils.{FileSource, NoPosition, Position}

import scala.reflect.{ClassTag, classTag}

class ParsingPositionSpec extends CompilerIntegrationTestSpec with AppendedClues {

  import tlang.testutils.TestConstants._

  private val TestFile: File = File(s"$Resources/positions/ParserPositions.t")

  // We make Tree lazy so the parsing time counts towards the test execution time
  private lazy val Tree: Tree = {
    val file = FileSource(TestFile) :: Nil
    try {
      (Lexing andThen Parsing).execute(TestContext)(file).head
    } catch {
      case e: CompilationException =>
        TestContext.output += ErrorMessageOutput(e.messages)
        Empty()
    }
  }

  private lazy val Trees: Map[Class[_], List[Tree]] = Tree.groupBy(_.getClass)

  private def testPositions[T <: Tree : ClassTag](expectedPositions: PositionTest*): Unit = {
    val clazz = classTag[T].runtimeClass
    val className = clazz.getSimpleName
    className in {
      if (Tree == Empty())
        fail(s"Failed to parse $TestFile")

      val treePositions = Trees
        .getOrElse(clazz, fail(s"No trees of class $className"))
        .map(Position(_))
        .filter { _ != NoPosition }

      treePositions
        .zip(expectedPositions.zipWithIndex)
        .foreach { case (foundPos, (test, index)) =>
          val clue = s"for $className number ${ index + 1 }."
          PositionTest.compare(foundPos, test, clue = Some(clue))
        }
    }
  }

  testPositions[CompilationUnit](
    PositionTest(1, 1, 57, 15),
  )
  testPositions[ClassDecl](
    PositionTest(1, 1, 51, 11),
  )
  testPositions[TraitDecl](
    PositionTest(55, 1, 55, 8),
  )
  testPositions[ExtensionDecl](
    PositionTest(57, 1, 57, 15),
  )
  testPositions[MethodDecl](
    PositionTest(12, 2, 12, 30),
    PositionTest(14, 2, 51, 11),
  )
  testPositions[ConstructorDecl](
    PositionTest(8, 2, 8, 46),
  )
  testPositions[OperatorDecl](
    PositionTest(10, 2, 10, 47),
  )
  testPositions[Formal](
    PositionTest(8, 19, 8, 28),
    PositionTest(8, 30, 8, 41),
    PositionTest(10, 8, 10, 19),
    PositionTest(10, 21, 10, 32),
    PositionTest(14, 11, 14, 19),
    PositionTest(14, 21, 14, 28),
  )
  testPositions[ArrayType](
    PositionTest(8, 33, 8, 41),
  )
  testPositions[NullableType](
    PositionTest(14, 14, 14, 19),
  )
  testPositions[UnitType](
    PositionTest(12, 22, 12, 26),
  )
  testPositions[VarDecl](
    PositionTest(3, 2, 3, 16),
    PositionTest(4, 2, 4, 11),
    PositionTest(5, 2, 5, 23),
    PositionTest(6, 2, 6, 29),
    PositionTest(23, 7, 23, 16),
    PositionTest(26, 7, 26, 12),
    PositionTest(30, 3, 37, 4),
  )
  testPositions[Block](
    PositionTest(8, 45, 8, 46),
    PositionTest(10, 46, 10, 47),
    PositionTest(12, 29, 12, 30),
    PositionTest(15, 1, 51, 11),
  )
  testPositions[If](
    PositionTest(15, 3, 18, 16),
  )
  testPositions[While](
    PositionTest(20, 3, 21, 16),
  )
  testPositions[For](
    PositionTest(23, 3, 24, 12),
  )
  testPositions[Foreach](
    PositionTest(26, 3, 27, 9),
  )
  testPositions[Error](
    PositionTest(21, 4, 21, 16),
  )
  testPositions[Return](
    PositionTest(51, 3, 51, 11),
  )
  testPositions[Break](
    PositionTest(27, 4, 27, 9),
  )
  testPositions[Continue](
    PositionTest(24, 4, 24, 12),
  )
  testPositions[Print](
    PositionTest(18, 4, 18, 16),
  )
  testPositions[Println](
    PositionTest(16, 4, 16, 18),
  )
  testPositions[Plus](
    PositionTest(16, 12, 16, 17),
  )
  testPositions[Minus](
    PositionTest(18, 10, 18, 15),
  )
  testPositions[Times](
    PositionTest(21, 10, 21, 15),
  )
  testPositions[Div](
    PositionTest(29, 9, 29, 14),
  )
  testPositions[Modulo](
    PositionTest(31, 7, 31, 13),
  )
  testPositions[LogicAnd](
    PositionTest(32, 7, 32, 20),
  )
  testPositions[LogicOr](
    PositionTest(33, 7, 33, 13),
  )
  testPositions[LogicXor](
    PositionTest(34, 7, 34, 16),
  )
  testPositions[LeftShift](
    PositionTest(35, 7, 35, 20),
  )
  testPositions[RightShift](
    PositionTest(36, 7, 36, 20),
  )
  testPositions[LessThan](
    PositionTest(15, 6, 15, 13),
    PositionTest(23, 18, 23, 23),
  )
  testPositions[LessThanEquals](
    PositionTest(15, 17, 15, 25),
  )
  testPositions[GreaterThan](
    PositionTest(15, 29, 15, 38),
  )
  testPositions[GreaterThanEquals](
    PositionTest(15, 42, 15, 52),
  )
  testPositions[Equals](
    PositionTest(20, 9, 20, 17),
  )
  testPositions[NotEquals](
    PositionTest(20, 21, 20, 27),
  )
  testPositions[And](
    PositionTest(15, 6, 15, 52),
    PositionTest(15, 6, 15, 38),
    PositionTest(15, 6, 15, 25),
  )
  testPositions[Or](
    PositionTest(20, 9, 20, 27),
  )
  testPositions[Not](
    PositionTest(15, 6, 15, 8),
  )
  testPositions[Hash](
    PositionTest(15, 11, 15, 13),
  )
  testPositions[Negation](
    PositionTest(15, 17, 15, 19),
  )
  testPositions[LogicNot](
    PositionTest(15, 23, 15, 25),
  )
  testPositions[ExtractNullable](
    PositionTest(15, 29, 15, 32),
  )
  testPositions[PreIncrement](
    PositionTest(15, 35, 15, 38),
  )
  testPositions[PostIncrement](
    PositionTest(15, 42, 15, 45),
  )
  testPositions[PreDecrement](
    PositionTest(15, 49, 15, 52),
  )
  testPositions[PostDecrement](
    PositionTest(20, 9, 20, 12),
  )
  testPositions[ArrayRead](
    PositionTest(40, 11, 40, 15),
    PositionTest(44, 3, 44, 7),
  )
  testPositions[ArraySlice](
    PositionTest(41, 13, 41, 17),
    PositionTest(42, 10, 42, 30),
    PositionTest(43, 14, 43, 19),
    PositionTest(46, 7, 46, 12),
    PositionTest(47, 9, 47, 21),
  )
  testPositions[IntLit](
    PositionTest(3, 15, 3, 16),
    PositionTest(4, 10, 4, 11),
    PositionTest(23, 15, 23, 16),
    PositionTest(23, 22, 23, 23),
    PositionTest(31, 7, 31, 8),
    PositionTest(33, 7, 33, 9),
    PositionTest(33, 12, 33, 13),
    PositionTest(42, 14, 42, 15),
    PositionTest(42, 20, 42, 21),
    PositionTest(42, 26, 42, 27),
    PositionTest(43, 17, 43, 18),
    PositionTest(46, 9, 46, 10),
    PositionTest(47, 12, 47, 13),
    PositionTest(47, 16, 47, 17),
    PositionTest(48, 13, 48, 14),
    PositionTest(49, 33, 49, 34),
    PositionTest(50, 18, 50, 19),
    PositionTest(50, 21, 50, 22),
    PositionTest(54, 41, 54, 42),
  )
  testPositions[LongLit](
    PositionTest(31, 11, 31, 13),
  )
  testPositions[FloatLit](
    PositionTest(32, 13, 32, 20),
  )
  testPositions[DoubleLit](
    PositionTest(32, 7, 32, 10),
    PositionTest(34, 7, 34, 10),
  )
  testPositions[CharLit](
    PositionTest(34, 13, 34, 16),
  )
  testPositions[StringLit](
    PositionTest(6, 24, 6, 29),
    PositionTest(35, 7, 35, 12),
    PositionTest(54, 30, 54, 35),
  )
  testPositions[TrueLit](
    PositionTest(35, 16, 35, 20),
  )
  testPositions[FalseLit](
    PositionTest(36, 7, 36, 12),
  )
  testPositions[NullLit](
    PositionTest(36, 16, 36, 20),
  )
  testPositions[ArrayLit](
    PositionTest(30, 11, 37, 4),
  )
  testPositions[ClassID](
    PositionTest(1, 7, 1, 15),
    PositionTest(1, 13, 1, 14),
    PositionTest(1, 18, 1, 19),
    PositionTest(1, 21, 1, 22),
    PositionTest(1, 24, 1, 25),
    PositionTest(3, 9, 3, 12),
    PositionTest(5, 19, 5, 23),
    PositionTest(6, 16, 6, 21),
    PositionTest(8, 22, 8, 28),
    PositionTest(8, 33, 8, 39),
    PositionTest(10, 11, 10, 19),
    PositionTest(10, 17, 10, 18),
    PositionTest(10, 24, 10, 32),
    PositionTest(10, 30, 10, 31),
    PositionTest(10, 35, 10, 43),
    PositionTest(10, 41, 10, 42),
    PositionTest(14, 14, 14, 18),
    PositionTest(14, 24, 14, 28),
    PositionTest(44, 15, 44, 16),
    PositionTest(48, 11, 48, 12),
    PositionTest(49, 13, 49, 19),
    PositionTest(50, 11, 50, 17),
    PositionTest(55, 7, 55, 8),
    PositionTest(54, 2, 54, 12),
    PositionTest(54, 14, 54, 25),
    PositionTest(57, 11, 57, 12),
    PositionTest(57, 14, 57, 15),
  )
  testPositions[VariableID](
    PositionTest(3, 6, 3, 7),
    PositionTest(4, 6, 4, 7),
    PositionTest(5, 16, 5, 17),
    PositionTest(6, 13, 6, 14),
    PositionTest(8, 19, 8, 20),
    PositionTest(8, 30, 8, 31),
    PositionTest(10, 8, 10, 9),
    PositionTest(10, 21, 10, 22),
    PositionTest(14, 11, 14, 12),
    PositionTest(14, 21, 14, 22),
    PositionTest(15, 7, 15, 8),
    PositionTest(15, 12, 15, 13),
    PositionTest(15, 18, 15, 19),
    PositionTest(15, 24, 15, 25),
    PositionTest(15, 29, 15, 30),
    PositionTest(15, 37, 15, 38),
    PositionTest(15, 42, 15, 43),
    PositionTest(15, 51, 15, 52),
    PositionTest(16, 12, 16, 13),
    PositionTest(16, 16, 16, 17),
    PositionTest(18, 10, 18, 11),
    PositionTest(18, 14, 18, 15),
    PositionTest(20, 9, 20, 10),
    PositionTest(20, 16, 20, 17),
    PositionTest(20, 21, 20, 22),
    PositionTest(20, 26, 20, 27),
    PositionTest(21, 10, 21, 11),
    PositionTest(21, 14, 21, 15),
    PositionTest(23, 11, 23, 12),
    PositionTest(23, 18, 23, 19),
    PositionTest(23, 25, 23, 26),
    PositionTest(26, 11, 26, 12),
    PositionTest(26, 16, 26, 17),
    PositionTest(29, 9, 29, 10),
    PositionTest(29, 13, 29, 14),
    PositionTest(30, 7, 30, 8),
    PositionTest(40, 3, 40, 4),
    PositionTest(40, 11, 40, 12),
    PositionTest(40, 13, 40, 14),
    PositionTest(41, 13, 41, 14),
    PositionTest(42, 3, 42, 4),
    PositionTest(42, 10, 42, 11),
    PositionTest(43, 14, 43, 15),
    PositionTest(44, 3, 44, 4),
    PositionTest(44, 5, 44, 6),
    PositionTest(44, 10, 44, 11),
    PositionTest(46, 3, 46, 4),
    PositionTest(46, 7, 46, 8),
    PositionTest(47, 3, 47, 4),
    PositionTest(47, 5, 47, 6),
    PositionTest(47, 9, 47, 10),
    PositionTest(48, 3, 48, 4),
    PositionTest(49, 3, 49, 4),
    PositionTest(49, 8, 49, 9),
    PositionTest(49, 23, 49, 24),
    PositionTest(49, 28, 49, 29),
    PositionTest(50, 3, 50, 4),
    PositionTest(51, 10, 51, 11),
    PositionTest(54, 26, 54, 27),
    PositionTest(54, 37, 54, 38),
  )
  testPositions[MethodID](
    PositionTest(8, 15, 8, 18),
    PositionTest(12, 13, 12, 18),
    PositionTest(14, 6, 14, 10),
    PositionTest(29, 3, 29, 8),
    PositionTest(40, 6, 40, 10),
    PositionTest(41, 8, 41, 12),
    PositionTest(42, 5, 42, 9),
    PositionTest(43, 9, 43, 13),
  )
  testPositions[NormalAccess](
    PositionTest(29, 3, 29, 15),
    PositionTest(41, 3, 41, 18),
    PositionTest(42, 3, 42, 31),
    PositionTest(43, 3, 43, 20),
    PositionTest(47, 3, 47, 6),
  )
  testPositions[SafeAccess](
    PositionTest(40, 3, 40, 16),
  )
  testPositions[Assign](
    PositionTest(44, 3, 44, 16),
    PositionTest(46, 3, 46, 12),
    PositionTest(47, 3, 47, 21),
    PositionTest(48, 3, 48, 15),
    PositionTest(49, 3, 49, 35),
    PositionTest(50, 3, 50, 23),
  )
  testPositions[MethodCall](
    PositionTest(29, 3, 29, 15),
    PositionTest(40, 6, 40, 16),
    PositionTest(41, 8, 41, 18),
    PositionTest(42, 5, 42, 31),
    PositionTest(43, 9, 43, 20),
  )
  testPositions[This](
    PositionTest(41, 3, 41, 7),
  )
  testPositions[Super](
    PositionTest(43, 3, 43, 8),
  )
  testPositions[NewArray](
    PositionTest(48, 7, 48, 15),
  )
  testPositions[New](
    PositionTest(50, 7, 50, 23),
  )
  testPositions[Ternary](
    PositionTest(49, 7, 49, 35),
  )
  testPositions[Elvis](
    PositionTest(49, 27, 49, 35),
  )
  testPositions[Is](
    PositionTest(49, 7, 49, 20),
  )
  testPositions[As](
    PositionTest(44, 10, 44, 16),
  )
  testPositions[Annotation](
    PositionTest(54, 1, 54, 12),
    PositionTest(54, 13, 54, 43),
  )
  testPositions[KeyValuePair](
    PositionTest(54, 26, 54, 35),
    PositionTest(54, 37, 54, 42),
  )
}
