package tlang.compiler.ast

import better.files.File
import tlang.Context
import tlang.compiler.CompilerIntegrationTestSpec
import tlang.compiler.ast.Trees._
import tlang.compiler.lexer.Lexing
import tlang.messages.{CompilationException, MessageType}
import tlang.testutils.TestConstants._
import tlang.utils.{FileSource, NoPosition, Position}

import scala.reflect.{ClassTag, classTag}

class ParsingPositionSpec extends CompilerIntegrationTestSpec {

  private val TestFile   : File    = File(s"$Resources/positions/ParserPositions.t")
  private val TestContext: Context = testContext()

  // We make Tree lazy so the parsing time counts towards the test execution time
  private lazy val Tree: Tree = {
    val file = FileSource(TestFile) :: Nil
    try {
      (Lexing andThen Parsing).execute(TestContext)(file).head
    } catch {
      case e: CompilationException =>
        e.messages.print(MessageType.Error)
        Empty()
    }
  }

  private lazy val Trees: Map[Class[_], List[Tree]] = Tree.groupBy(_.getClass)

  private def testPositions[T <: Tree : ClassTag](positions: Position*): Unit = {
    val clazz = classTag[T].runtimeClass
    val className = clazz.getSimpleName
    className in {
      if (Tree == Empty())
        fail(s"Failed to parse $TestFile")

      val treePositions = Trees
        .getOrElse(clazz, fail(s"No trees of class $className"))
        .map(Position(_))
        .filter { pos => pos != NoPosition }

      treePositions.zip(positions.zipWithIndex) foreach { case (foundPos, (expectedPos, index)) =>
        withClue(s" for $className number ${ index + 1 }.") {
          foundPos shouldBe expectedPos
        }
      }
    }
  }

  testPositions[CompilationUnit](
    Position(1, 1, 57, 12)
  )
  testPositions[ClassDecl](
    Position(1, 1, 51, 11)
  )
  testPositions[TraitDecl](
    Position(54, 1, 54, 8)
  )
  testPositions[ExtensionDecl](
    Position(57, 1, 57, 12)
  )
  testPositions[MethodDecl](
    Position(12, 2, 12, 30),
    Position(14, 2, 51, 11)
  )
  testPositions[ConstructorDecl](
    Position(8, 2, 8, 46)
  )
  testPositions[OperatorDecl](
    Position(10, 2, 10, 47)
  )
  testPositions[Formal](
    Position(8, 19, 8, 28),
    Position(8, 30, 8, 41),
    Position(10, 8, 10, 19),
    Position(10, 21, 10, 32),
    Position(14, 11, 14, 19),
    Position(14, 21, 14, 28)
  )
  testPositions[ArrayType](
    Position(8, 33, 8, 41)
  )
  testPositions[NullableType](
    Position(14, 14, 14, 19)
  )
  testPositions[UnitType](
    Position(12, 22, 12, 26)
  )
  testPositions[VarDecl](
    Position(3, 2, 3, 16),
    Position(4, 2, 4, 11),
    Position(5, 2, 5, 23),
    Position(6, 2, 6, 29),
    Position(23, 7, 23, 16),
    Position(26, 7, 26, 12),
    Position(30, 3, 37, 4)
  )
  testPositions[Block](
    Position(8, 45, 8, 46),
    Position(10, 46, 10, 47),
    Position(12, 29, 12, 30),
    Position(15, 1, 51, 11)
  )
  testPositions[If](
    Position(15, 3, 18, 16)
  )
  testPositions[While](
    Position(20, 3, 21, 16)
  )
  testPositions[For](
    Position(23, 3, 24, 12)
  )
  testPositions[Foreach](
    Position(26, 3, 27, 9)
  )
  testPositions[Error](
    Position(21, 4, 21, 16)
  )
  testPositions[Return](
    Position(51, 3, 51, 11)
  )
  testPositions[Break](
    Position(27, 4, 27, 9)
  )
  testPositions[Continue](
    Position(24, 4, 24, 12)
  )
  testPositions[Print](
    Position(18, 4, 18, 16)
  )
  testPositions[Println](
    Position(16, 4, 16, 18)
  )
  testPositions[Plus](
    Position(16, 12, 16, 17)
  )
  testPositions[Minus](
    Position(18, 10, 18, 15)
  )
  testPositions[Times](
    Position(21, 10, 21, 15)
  )
  testPositions[Div](
    Position(29, 9, 29, 14)
  )
  testPositions[Modulo](
    Position(31, 7, 31, 13)
  )
  testPositions[LogicAnd](
    Position(32, 7, 32, 20)
  )
  testPositions[LogicOr](
    Position(33, 7, 33, 13)
  )
  testPositions[LogicXor](
    Position(34, 7, 34, 16)
  )
  testPositions[LeftShift](
    Position(35, 7, 35, 20)
  )
  testPositions[RightShift](
    Position(36, 7, 36, 20)
  )
  testPositions[LessThan](
    Position(15, 6, 15, 13),
    Position(23, 18, 23, 23)
  )
  testPositions[LessThanEquals](
    Position(15, 17, 15, 25)
  )
  testPositions[GreaterThan](
    Position(15, 29, 15, 38)
  )
  testPositions[GreaterThanEquals](
    Position(15, 42, 15, 52)
  )
  testPositions[Equals](
    Position(20, 9, 20, 17)
  )
  testPositions[NotEquals](
    Position(20, 21, 20, 27)
  )
  testPositions[And](
    Position(15, 6, 15, 52),
    Position(15, 6, 15, 38),
    Position(15, 6, 15, 25)
  )
  testPositions[Or](
    Position(20, 9, 20, 27)
  )
  testPositions[Not](
    Position(15, 6, 15, 8)
  )
  testPositions[Hash](
    Position(15, 11, 15, 13)
  )
  testPositions[Negation](
    Position(15, 17, 15, 19)
  )
  testPositions[LogicNot](
    Position(15, 23, 15, 25)
  )
  testPositions[ExtractNullable](
    Position(15, 29, 15, 32)
  )
  testPositions[PreIncrement](
    Position(15, 35, 15, 38)
  )
  testPositions[PostIncrement](
    Position(15, 42, 15, 45)
  )
  testPositions[PreDecrement](
    Position(15, 49, 15, 52)
  )
  testPositions[PostDecrement](
    Position(20, 9, 20, 12)
  )
  testPositions[ArrayRead](
    Position(40, 11, 40, 15),
    Position(44, 3, 44, 7)
  )
  testPositions[ArraySlice](
    Position(41, 13, 41, 17),
    Position(42, 10, 42, 30),
    Position(43, 14, 43, 19),
    Position(46, 7, 46, 12),
    Position(47, 9, 47, 21)
  )
  testPositions[IntLit](
    Position(3, 15, 3, 16),
    Position(4, 10, 4, 11),
    Position(23, 15, 23, 16),
    Position(23, 22, 23, 23),
    Position(31, 7, 31, 8),
    Position(33, 7, 33, 9),
    Position(33, 12, 33, 13),
    Position(42, 14, 42, 15),
    Position(42, 20, 42, 21),
    Position(42, 26, 42, 27),
    Position(43, 17, 43, 18),
    Position(46, 9, 46, 10),
    Position(47, 12, 47, 13),
    Position(47, 16, 47, 17),
    Position(48, 13, 48, 14),
    Position(49, 33, 49, 34),
    Position(50, 18, 50, 19),
    Position(50, 21, 50, 22)
  )
  testPositions[LongLit](
    Position(31, 11, 31, 13)
  )
  testPositions[FloatLit](
    Position(32, 13, 32, 20)
  )
  testPositions[DoubleLit](
    Position(32, 7, 32, 10),
    Position(34, 7, 34, 10)
  )
  testPositions[CharLit](
    Position(34, 13, 34, 16)
  )
  testPositions[StringLit](
    Position(6, 24, 6, 29),
    Position(35, 7, 35, 12)
  )
  testPositions[TrueLit](
    Position(35, 16, 35, 20)
  )
  testPositions[FalseLit](
    Position(36, 7, 36, 12)
  )
  testPositions[NullLit](
    Position(36, 16, 36, 20)
  )
  testPositions[ArrayLit](
    Position(30, 11, 37, 4)
  )
  testPositions[ClassID](
    Position(1, 7, 1, 15),
    Position(1, 13, 1, 14),
    Position(1, 18, 1, 19),
    Position(1, 21, 1, 22),
    Position(1, 24, 1, 25),
    Position(3, 9, 3, 12),
    Position(5, 19, 5, 23),
    Position(6, 16, 6, 21),
    Position(8, 22, 8, 28),
    Position(8, 33, 8, 39),
    Position(10, 11, 10, 19),
    Position(10, 17, 10, 18),
    Position(10, 24, 10, 32),
    Position(10, 30, 10, 31),
    Position(10, 35, 10, 43),
    Position(10, 41, 10, 42),
    Position(14, 14, 14, 18),
    Position(14, 24, 14, 28),
    Position(44, 15, 44, 16),
    Position(48, 11, 48, 12),
    Position(49, 13, 49, 19),
    Position(50, 11, 50, 17),
    Position(54, 7, 54, 8),
    Position(57, 11, 57, 12)
  )
  testPositions[VariableID](
    Position(3, 6, 3, 7),
    Position(4, 6, 4, 7),
    Position(5, 16, 5, 17),
    Position(6, 13, 6, 14),
    Position(8, 19, 8, 20),
    Position(8, 30, 8, 31),
    Position(10, 8, 10, 9),
    Position(10, 21, 10, 22),
    Position(14, 11, 14, 12),
    Position(14, 21, 14, 22),
    Position(15, 7, 15, 8),
    Position(15, 12, 15, 13),
    Position(15, 18, 15, 19),
    Position(15, 24, 15, 25),
    Position(15, 29, 15, 30),
    Position(15, 37, 15, 38),
    Position(15, 42, 15, 43),
    Position(15, 51, 15, 52),
    Position(16, 12, 16, 13),
    Position(16, 16, 16, 17),
    Position(18, 10, 18, 11),
    Position(18, 14, 18, 15),
    Position(20, 9, 20, 10),
    Position(20, 16, 20, 17),
    Position(20, 21, 20, 22),
    Position(20, 26, 20, 27),
    Position(21, 10, 21, 11),
    Position(21, 14, 21, 15),
    Position(23, 11, 23, 12),
    Position(23, 18, 23, 19),
    Position(23, 25, 23, 26),
    Position(26, 11, 26, 12),
    Position(26, 16, 26, 17),
    Position(29, 9, 29, 10),
    Position(29, 13, 29, 14),
    Position(30, 7, 30, 8),
    Position(40, 3, 40, 4),
    Position(40, 11, 40, 12),
    Position(40, 13, 40, 14),
    Position(41, 13, 41, 14),
    Position(42, 3, 42, 4),
    Position(42, 10, 42, 11),
    Position(43, 14, 43, 15),
    Position(44, 3, 44, 4),
    Position(44, 5, 44, 6),
    Position(44, 10, 44, 11),
    Position(46, 3, 46, 4),
    Position(46, 7, 46, 8),
    Position(47, 3, 47, 4),
    Position(47, 5, 47, 6),
    Position(47, 9, 47, 10),
    Position(48, 3, 48, 4),
    Position(49, 3, 49, 4),
    Position(49, 8, 49, 9),
    Position(49, 23, 49, 24),
    Position(49, 28, 49, 29),
    Position(50, 3, 50, 4),
    Position(51, 10, 51, 11)
  )
  testPositions[MethodID](
    Position(8, 15, 8, 18),
    Position(12, 13, 12, 18),
    Position(14, 6, 14, 10),
    Position(29, 3, 29, 8),
    Position(40, 6, 40, 10),
    Position(41, 8, 41, 12),
    Position(42, 5, 42, 9),
    Position(43, 9, 43, 13)
  )
  testPositions[NormalAccess](
    Position(29, 3, 29, 15),
    Position(41, 3, 41, 18),
    Position(42, 3, 42, 31),
    Position(43, 3, 43, 20),
    Position(47, 3, 47, 6)
  )
  testPositions[SafeAccess](
    Position(40, 3, 40, 16)
  )
  testPositions[Assign](
    Position(44, 3, 44, 16),
    Position(46, 3, 46, 12),
    Position(47, 3, 47, 21),
    Position(48, 3, 48, 15),
    Position(49, 3, 49, 35),
    Position(50, 3, 50, 23)
  )
  testPositions[MethodCall](
    Position(29, 3, 29, 15),
    Position(40, 6, 40, 16),
    Position(41, 8, 41, 18),
    Position(42, 5, 42, 31),
    Position(43, 9, 43, 20)
  )
  testPositions[This](
    Position(41, 3, 41, 7)
  )
  testPositions[Super](
    Position(43, 3, 43, 8)
  )
  testPositions[NewArray](
    Position(48, 7, 48, 15)
  )
  testPositions[New](
    Position(50, 7, 50, 23)
  )
  testPositions[Ternary](
    Position(49, 7, 49, 35)
  )
  testPositions[Elvis](
    Position(49, 27, 49, 35)
  )
  testPositions[Is](
    Position(49, 7, 49, 20)
  )
  testPositions[As](
    Position(44, 10, 44, 16)
  )

}
