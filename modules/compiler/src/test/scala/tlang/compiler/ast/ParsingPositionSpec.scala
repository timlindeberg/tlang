package tlang
package compiler
package ast

import better.files.File
import org.scalatest.AppendedClues
import tlang.compiler.ast.Trees._
import tlang.compiler.lexer.Lexing
import tlang.compiler.messages.CompilationException
import tlang.compiler.output.ErrorMessageOutput
import tlang.testutils.TestPosition
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

  private def testPositions[T <: Tree : ClassTag](expectedPositions: TestPosition*): Unit = {
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
          TestPosition.compare(foundPos, test, clue = Some(clue))
        }
    }
  }

  testPositions[CompilationUnit](
    TestPosition(1, 1, 57, 15),
  )
  testPositions[ClassDecl](
    TestPosition(1, 1, 51, 11),
  )
  testPositions[TraitDecl](
    TestPosition(55, 1, 55, 8),
  )
  testPositions[ExtensionDecl](
    TestPosition(57, 1, 57, 15),
  )
  testPositions[MethodDecl](
    TestPosition(12, 2, 12, 30),
    TestPosition(14, 2, 51, 11),
  )
  testPositions[ConstructorDecl](
    TestPosition(8, 2, 8, 46),
  )
  testPositions[OperatorDecl](
    TestPosition(10, 2, 10, 47),
  )
  testPositions[Formal](
    TestPosition(8, 19, 8, 28),
    TestPosition(8, 30, 8, 41),
    TestPosition(10, 8, 10, 19),
    TestPosition(10, 21, 10, 32),
    TestPosition(14, 11, 14, 19),
    TestPosition(14, 21, 14, 28),
  )
  testPositions[ArrayType](
    TestPosition(8, 33, 8, 41),
  )
  testPositions[NullableType](
    TestPosition(14, 14, 14, 19),
  )
  testPositions[UnitType](
    TestPosition(12, 22, 12, 26),
  )
  testPositions[VarDecl](
    TestPosition(3, 2, 3, 16),
    TestPosition(4, 2, 4, 11),
    TestPosition(5, 2, 5, 23),
    TestPosition(6, 2, 6, 29),
    TestPosition(23, 7, 23, 16),
    TestPosition(26, 7, 26, 12),
    TestPosition(30, 3, 37, 4),
  )
  testPositions[Block](
    TestPosition(8, 45, 8, 46),
    TestPosition(10, 46, 10, 47),
    TestPosition(12, 29, 12, 30),
    TestPosition(15, 1, 51, 11),
  )
  testPositions[If](
    TestPosition(15, 3, 18, 16),
  )
  testPositions[While](
    TestPosition(20, 3, 21, 16),
  )
  testPositions[For](
    TestPosition(23, 3, 24, 12),
  )
  testPositions[Foreach](
    TestPosition(26, 3, 27, 9),
  )
  testPositions[Error](
    TestPosition(21, 4, 21, 16),
  )
  testPositions[Return](
    TestPosition(51, 3, 51, 11),
  )
  testPositions[Break](
    TestPosition(27, 4, 27, 9),
  )
  testPositions[Continue](
    TestPosition(24, 4, 24, 12),
  )
  testPositions[Print](
    TestPosition(18, 4, 18, 16),
  )
  testPositions[Println](
    TestPosition(16, 4, 16, 18),
  )
  testPositions[Plus](
    TestPosition(16, 12, 16, 17),
  )
  testPositions[Minus](
    TestPosition(18, 10, 18, 15),
  )
  testPositions[Times](
    TestPosition(21, 10, 21, 15),
  )
  testPositions[Div](
    TestPosition(29, 9, 29, 14),
  )
  testPositions[Modulo](
    TestPosition(31, 7, 31, 13),
  )
  testPositions[LogicAnd](
    TestPosition(32, 7, 32, 20),
  )
  testPositions[LogicOr](
    TestPosition(33, 7, 33, 13),
  )
  testPositions[LogicXor](
    TestPosition(34, 7, 34, 16),
  )
  testPositions[LeftShift](
    TestPosition(35, 7, 35, 20),
  )
  testPositions[RightShift](
    TestPosition(36, 7, 36, 20),
  )
  testPositions[LessThan](
    TestPosition(15, 6, 15, 13),
    TestPosition(23, 18, 23, 23),
  )
  testPositions[LessThanEquals](
    TestPosition(15, 17, 15, 25),
  )
  testPositions[GreaterThan](
    TestPosition(15, 29, 15, 38),
  )
  testPositions[GreaterThanEquals](
    TestPosition(15, 42, 15, 52),
  )
  testPositions[Equals](
    TestPosition(20, 9, 20, 17),
  )
  testPositions[NotEquals](
    TestPosition(20, 21, 20, 27),
  )
  testPositions[And](
    TestPosition(15, 6, 15, 52),
    TestPosition(15, 6, 15, 38),
    TestPosition(15, 6, 15, 25),
  )
  testPositions[Or](
    TestPosition(20, 9, 20, 27),
  )
  testPositions[Not](
    TestPosition(15, 6, 15, 8),
  )
  testPositions[Hash](
    TestPosition(15, 11, 15, 13),
  )
  testPositions[Negation](
    TestPosition(15, 17, 15, 19),
  )
  testPositions[LogicNot](
    TestPosition(15, 23, 15, 25),
  )
  testPositions[ExtractNullable](
    TestPosition(15, 29, 15, 32),
  )
  testPositions[PreIncrement](
    TestPosition(15, 35, 15, 38),
  )
  testPositions[PostIncrement](
    TestPosition(15, 42, 15, 45),
  )
  testPositions[PreDecrement](
    TestPosition(15, 49, 15, 52),
  )
  testPositions[PostDecrement](
    TestPosition(20, 9, 20, 12),
  )
  testPositions[ArrayRead](
    TestPosition(40, 11, 40, 15),
    TestPosition(44, 3, 44, 7),
  )
  testPositions[ArraySlice](
    TestPosition(41, 13, 41, 17),
    TestPosition(42, 10, 42, 30),
    TestPosition(43, 14, 43, 19),
    TestPosition(46, 7, 46, 12),
    TestPosition(47, 9, 47, 21),
  )
  testPositions[IntLit](
    TestPosition(3, 15, 3, 16),
    TestPosition(4, 10, 4, 11),
    TestPosition(23, 15, 23, 16),
    TestPosition(23, 22, 23, 23),
    TestPosition(31, 7, 31, 8),
    TestPosition(33, 7, 33, 9),
    TestPosition(33, 12, 33, 13),
    TestPosition(42, 14, 42, 15),
    TestPosition(42, 20, 42, 21),
    TestPosition(42, 26, 42, 27),
    TestPosition(43, 17, 43, 18),
    TestPosition(46, 9, 46, 10),
    TestPosition(47, 12, 47, 13),
    TestPosition(47, 16, 47, 17),
    TestPosition(48, 13, 48, 14),
    TestPosition(49, 33, 49, 34),
    TestPosition(50, 18, 50, 19),
    TestPosition(50, 21, 50, 22),
    TestPosition(54, 41, 54, 42),
  )
  testPositions[LongLit](
    TestPosition(31, 11, 31, 13),
  )
  testPositions[FloatLit](
    TestPosition(32, 13, 32, 20),
  )
  testPositions[DoubleLit](
    TestPosition(32, 7, 32, 10),
    TestPosition(34, 7, 34, 10),
  )
  testPositions[CharLit](
    TestPosition(34, 13, 34, 16),
  )
  testPositions[StringLit](
    TestPosition(6, 24, 6, 29),
    TestPosition(35, 7, 35, 12),
    TestPosition(54, 30, 54, 35),
  )
  testPositions[TrueLit](
    TestPosition(35, 16, 35, 20),
  )
  testPositions[FalseLit](
    TestPosition(36, 7, 36, 12),
  )
  testPositions[NullLit](
    TestPosition(36, 16, 36, 20),
  )
  testPositions[ArrayLit](
    TestPosition(30, 11, 37, 4),
  )
  testPositions[ClassID](
    TestPosition(1, 7, 1, 15),
    TestPosition(1, 13, 1, 14),
    TestPosition(1, 18, 1, 19),
    TestPosition(1, 21, 1, 22),
    TestPosition(1, 24, 1, 25),
    TestPosition(3, 9, 3, 12),
    TestPosition(5, 19, 5, 23),
    TestPosition(6, 16, 6, 21),
    TestPosition(8, 22, 8, 28),
    TestPosition(8, 33, 8, 39),
    TestPosition(10, 11, 10, 19),
    TestPosition(10, 17, 10, 18),
    TestPosition(10, 24, 10, 32),
    TestPosition(10, 30, 10, 31),
    TestPosition(10, 35, 10, 43),
    TestPosition(10, 41, 10, 42),
    TestPosition(14, 14, 14, 18),
    TestPosition(14, 24, 14, 28),
    TestPosition(44, 15, 44, 16),
    TestPosition(48, 11, 48, 12),
    TestPosition(49, 13, 49, 19),
    TestPosition(50, 11, 50, 17),
    TestPosition(55, 7, 55, 8),
    TestPosition(54, 2, 54, 12),
    TestPosition(54, 14, 54, 25),
    TestPosition(57, 11, 57, 12),
    TestPosition(57, 14, 57, 15),
  )
  testPositions[VariableID](
    TestPosition(3, 6, 3, 7),
    TestPosition(4, 6, 4, 7),
    TestPosition(5, 16, 5, 17),
    TestPosition(6, 13, 6, 14),
    TestPosition(8, 19, 8, 20),
    TestPosition(8, 30, 8, 31),
    TestPosition(10, 8, 10, 9),
    TestPosition(10, 21, 10, 22),
    TestPosition(14, 11, 14, 12),
    TestPosition(14, 21, 14, 22),
    TestPosition(15, 7, 15, 8),
    TestPosition(15, 12, 15, 13),
    TestPosition(15, 18, 15, 19),
    TestPosition(15, 24, 15, 25),
    TestPosition(15, 29, 15, 30),
    TestPosition(15, 37, 15, 38),
    TestPosition(15, 42, 15, 43),
    TestPosition(15, 51, 15, 52),
    TestPosition(16, 12, 16, 13),
    TestPosition(16, 16, 16, 17),
    TestPosition(18, 10, 18, 11),
    TestPosition(18, 14, 18, 15),
    TestPosition(20, 9, 20, 10),
    TestPosition(20, 16, 20, 17),
    TestPosition(20, 21, 20, 22),
    TestPosition(20, 26, 20, 27),
    TestPosition(21, 10, 21, 11),
    TestPosition(21, 14, 21, 15),
    TestPosition(23, 11, 23, 12),
    TestPosition(23, 18, 23, 19),
    TestPosition(23, 25, 23, 26),
    TestPosition(26, 11, 26, 12),
    TestPosition(26, 16, 26, 17),
    TestPosition(29, 9, 29, 10),
    TestPosition(29, 13, 29, 14),
    TestPosition(30, 7, 30, 8),
    TestPosition(40, 3, 40, 4),
    TestPosition(40, 11, 40, 12),
    TestPosition(40, 13, 40, 14),
    TestPosition(41, 13, 41, 14),
    TestPosition(42, 3, 42, 4),
    TestPosition(42, 10, 42, 11),
    TestPosition(43, 14, 43, 15),
    TestPosition(44, 3, 44, 4),
    TestPosition(44, 5, 44, 6),
    TestPosition(44, 10, 44, 11),
    TestPosition(46, 3, 46, 4),
    TestPosition(46, 7, 46, 8),
    TestPosition(47, 3, 47, 4),
    TestPosition(47, 5, 47, 6),
    TestPosition(47, 9, 47, 10),
    TestPosition(48, 3, 48, 4),
    TestPosition(49, 3, 49, 4),
    TestPosition(49, 8, 49, 9),
    TestPosition(49, 23, 49, 24),
    TestPosition(49, 28, 49, 29),
    TestPosition(50, 3, 50, 4),
    TestPosition(51, 10, 51, 11),
    TestPosition(54, 26, 54, 27),
    TestPosition(54, 37, 54, 38),
  )
  testPositions[MethodID](
    TestPosition(8, 15, 8, 18),
    TestPosition(12, 13, 12, 18),
    TestPosition(14, 6, 14, 10),
    TestPosition(29, 3, 29, 8),
    TestPosition(40, 6, 40, 10),
    TestPosition(41, 8, 41, 12),
    TestPosition(42, 5, 42, 9),
    TestPosition(43, 9, 43, 13),
  )
  testPositions[NormalAccess](
    TestPosition(29, 3, 29, 15),
    TestPosition(41, 3, 41, 18),
    TestPosition(42, 3, 42, 31),
    TestPosition(43, 3, 43, 20),
    TestPosition(47, 3, 47, 6),
  )
  testPositions[SafeAccess](
    TestPosition(40, 3, 40, 16),
  )
  testPositions[Assign](
    TestPosition(44, 3, 44, 16),
    TestPosition(46, 3, 46, 12),
    TestPosition(47, 3, 47, 21),
    TestPosition(48, 3, 48, 15),
    TestPosition(49, 3, 49, 35),
    TestPosition(50, 3, 50, 23),
  )
  testPositions[MethodCall](
    TestPosition(29, 3, 29, 15),
    TestPosition(40, 6, 40, 16),
    TestPosition(41, 8, 41, 18),
    TestPosition(42, 5, 42, 31),
    TestPosition(43, 9, 43, 20),
  )
  testPositions[This](
    TestPosition(41, 3, 41, 7),
  )
  testPositions[Super](
    TestPosition(43, 3, 43, 8),
  )
  testPositions[NewArray](
    TestPosition(48, 7, 48, 15),
  )
  testPositions[New](
    TestPosition(50, 7, 50, 23),
  )
  testPositions[Ternary](
    TestPosition(49, 7, 49, 35),
  )
  testPositions[Elvis](
    TestPosition(49, 27, 49, 35),
  )
  testPositions[Is](
    TestPosition(49, 7, 49, 20),
  )
  testPositions[As](
    TestPosition(44, 10, 44, 16),
  )
  testPositions[Annotation](
    TestPosition(54, 1, 54, 12),
    TestPosition(54, 13, 54, 43),
  )
  testPositions[KeyValuePair](
    TestPosition(54, 26, 54, 35),
    TestPosition(54, 37, 54, 42),
  )
}
