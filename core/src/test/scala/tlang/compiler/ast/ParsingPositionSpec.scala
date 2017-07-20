package tlang.compiler.ast

import java.io.File

import org.scalatest.{FunSuite, Matchers}
import tlang.Context
import tlang.compiler.ast.Trees._
import tlang.compiler.error.CompilationException
import tlang.compiler.lexer.Lexing
import tlang.compiler.{Pos, Tester}
import tlang.utils.Extensions._
import tlang.utils.FileSource

import scala.reflect.{ClassTag, classTag}

class ParsingPositionSpec extends FunSuite with Matchers {

  private val File                                      = "ParserPositions.t"
  private val NoPos                    : Pos            = Pos(-1, -1, -1, -1)
  private val TestFile                 : String         = Tester.Resources + "positions/" + File
  private val TestContext              : Context        = Tester.getTestContext()
  private var compilationFailureMessage: Option[String] = None

  private val Tree: Tree = {
    val file = FileSource(new File(TestFile)) :: Nil
    try {
      (Lexing andThen Parsing).execute(TestContext)(file).head
    } catch {
      case e: CompilationException =>
        compilationFailureMessage = Some(e.messages.formattedErrors.clearAnsi)
        Empty()
    }
  }

  private val Trees: Map[Class[_], List[Tree]] = Tree.groupBy(_.getClass)

  private def testPositions[T <: Tree : ClassTag](positions: Pos*): Unit = {
    val clazz = classTag[T].runtimeClass
    val className = clazz.getSimpleName
    test(className) {

      compilationFailureMessage ifDefined { msg =>
        fail(s"Failed to parse $File:\n$msg")
      }

      val trees = Trees
        .getOrElse(clazz, fail(s"No trees of class $className"))
        .filter { t => Pos(t) != NoPos }

      trees.zip(positions.zipWithIndex) foreach { case (tree, (expectedPos, index)) =>
        val pos = new Pos(tree)
        assert(pos == expectedPos, s" for $className number ${ index + 1 }.")
      }
    }
  }

  testPositions[CompilationUnit](
    Pos(1, 1, 57, 12)
  )
  testPositions[ClassDecl](
    Pos(1, 1, 51, 11)
  )
  testPositions[TraitDecl](
    Pos(54, 1, 54, 8)
  )
  testPositions[ExtensionDecl](
    Pos(57, 1, 57, 12)
  )
  testPositions[MethodDecl](
    Pos(12, 2, 12, 30),
    Pos(14, 2, 51, 11)
  )
  testPositions[ConstructorDecl](
    Pos(8, 2, 8, 46)
  )
  testPositions[OperatorDecl](
    Pos(10, 2, 10, 47)
  )
  testPositions[Formal](
    Pos(8, 19, 8, 28),
    Pos(8, 30, 8, 41),
    Pos(10, 8, 10, 19),
    Pos(10, 21, 10, 32),
    Pos(14, 11, 14, 19),
    Pos(14, 21, 14, 28)
  )
  testPositions[ArrayType](
    Pos(8, 33, 8, 41)
  )
  testPositions[NullableType](
    Pos(14, 14, 14, 19)
  )
  testPositions[UnitType](
    Pos(12, 22, 12, 26)
  )
  testPositions[VarDecl](
    Pos(3, 2, 3, 16),
    Pos(4, 2, 4, 11),
    Pos(5, 2, 5, 23),
    Pos(6, 2, 6, 29),
    Pos(23, 7, 23, 16),
    Pos(26, 7, 26, 12),
    Pos(30, 3, 37, 4)
  )
  testPositions[Block](
    Pos(8, 45, 8, 46),
    Pos(10, 46, 10, 47),
    Pos(12, 29, 12, 30),
    Pos(15, 1, 51, 11)
  )
  testPositions[If](
    Pos(15, 3, 18, 16)
  )
  testPositions[While](
    Pos(20, 3, 21, 16)
  )
  testPositions[For](
    Pos(23, 3, 24, 12)
  )
  testPositions[Foreach](
    Pos(26, 3, 27, 9)
  )
  testPositions[Error](
    Pos(21, 4, 21, 16)
  )
  testPositions[Return](
    Pos(51, 3, 51, 11)
  )
  testPositions[Break](
    Pos(27, 4, 27, 9)
  )
  testPositions[Continue](
    Pos(24, 4, 24, 12)
  )
  testPositions[Print](
    Pos(18, 4, 18, 16)
  )
  testPositions[Println](
    Pos(16, 4, 16, 18)
  )
  testPositions[Plus](
    Pos(16, 12, 16, 17)
  )
  testPositions[Minus](
    Pos(18, 10, 18, 15)
  )
  testPositions[Times](
    Pos(21, 10, 21, 15)
  )
  testPositions[Div](
    Pos(29, 9, 29, 14)
  )
  testPositions[Modulo](
    Pos(31, 7, 31, 13)
  )
  testPositions[LogicAnd](
    Pos(32, 7, 32, 20)
  )
  testPositions[LogicOr](
    Pos(33, 7, 33, 13)
  )
  testPositions[LogicXor](
    Pos(34, 7, 34, 16)
  )
  testPositions[LeftShift](
    Pos(35, 7, 35, 20)
  )
  testPositions[RightShift](
    Pos(36, 7, 36, 20)
  )
  testPositions[LessThan](
    Pos(15, 6, 15, 13),
    Pos(23, 18, 23, 23)
  )
  testPositions[LessThanEquals](
    Pos(15, 17, 15, 25)
  )
  testPositions[GreaterThan](
    Pos(15, 29, 15, 38)
  )
  testPositions[GreaterThanEquals](
    Pos(15, 42, 15, 52)
  )
  testPositions[Equals](
    Pos(20, 9, 20, 17)
  )
  testPositions[NotEquals](
    Pos(20, 21, 20, 27)
  )
  testPositions[And](
    Pos(15, 6, 15, 52),
    Pos(15, 6, 15, 38),
    Pos(15, 6, 15, 25)
  )
  testPositions[Or](
    Pos(20, 9, 20, 27)
  )
  testPositions[Not](
    Pos(15, 6, 15, 8)
  )
  testPositions[Hash](
    Pos(15, 11, 15, 13)
  )
  testPositions[Negation](
    Pos(15, 17, 15, 19)
  )
  testPositions[LogicNot](
    Pos(15, 23, 15, 25)
  )
  testPositions[ExtractNullable](
    Pos(15, 29, 15, 32)
  )
  testPositions[PreIncrement](
    Pos(15, 35, 15, 38)
  )
  testPositions[PostIncrement](
    Pos(15, 42, 15, 45)
  )
  testPositions[PreDecrement](
    Pos(15, 49, 15, 52)
  )
  testPositions[PostDecrement](
    Pos(20, 9, 20, 12)
  )
  testPositions[ArrayRead](
    Pos(40, 11, 40, 15),
    Pos(44, 3, 44, 7)
  )
  testPositions[ArraySlice](
    Pos(41, 13, 41, 17),
    Pos(42, 10, 42, 30),
    Pos(43, 14, 43, 19),
    Pos(46, 7, 46, 12),
    Pos(47, 9, 47, 21)
  )
  testPositions[IntLit](
    Pos(3, 15, 3, 16),
    Pos(4, 10, 4, 11),
    Pos(23, 15, 23, 16),
    Pos(23, 22, 23, 23),
    Pos(31, 7, 31, 8),
    Pos(33, 7, 33, 9),
    Pos(33, 12, 33, 13),
    Pos(42, 14, 42, 15),
    Pos(42, 20, 42, 21),
    Pos(42, 26, 42, 27),
    Pos(43, 17, 43, 18),
    Pos(46, 9, 46, 10),
    Pos(47, 12, 47, 13),
    Pos(47, 16, 47, 17),
    Pos(48, 13, 48, 14),
    Pos(49, 33, 49, 34),
    Pos(50, 18, 50, 19),
    Pos(50, 21, 50, 22)
  )
  testPositions[LongLit](
    Pos(31, 11, 31, 13)
  )
  testPositions[FloatLit](
    Pos(32, 13, 32, 20)
  )
  testPositions[DoubleLit](
    Pos(32, 7, 32, 10),
    Pos(34, 7, 34, 10)
  )
  testPositions[CharLit](
    Pos(34, 13, 34, 16)
  )
  testPositions[StringLit](
    Pos(6, 24, 6, 29),
    Pos(35, 7, 35, 12)
  )
  testPositions[TrueLit](
    Pos(35, 16, 35, 20)
  )
  testPositions[FalseLit](
    Pos(36, 7, 36, 12)
  )
  testPositions[NullLit](
    Pos(36, 16, 36, 20)
  )
  testPositions[ArrayLit](
    Pos(30, 11, 37, 4)
  )
  testPositions[ClassID](
    Pos(1, 7, 1, 15),
    Pos(1, 13, 1, 14),
    Pos(1, 18, 1, 19),
    Pos(1, 21, 1, 22),
    Pos(1, 24, 1, 25),
    Pos(3, 9, 3, 12),
    Pos(5, 19, 5, 23),
    Pos(6, 16, 6, 21),
    Pos(8, 22, 8, 28),
    Pos(8, 33, 8, 39),
    Pos(10, 11, 10, 19),
    Pos(10, 17, 10, 18),
    Pos(10, 24, 10, 32),
    Pos(10, 30, 10, 31),
    Pos(10, 35, 10, 43),
    Pos(10, 41, 10, 42),
    Pos(14, 14, 14, 18),
    Pos(14, 24, 14, 28),
    Pos(44, 15, 44, 16),
    Pos(48, 11, 48, 12),
    Pos(49, 13, 49, 19),
    Pos(50, 11, 50, 17),
    Pos(54, 7, 54, 8),
    Pos(57, 11, 57, 12)
  )
  testPositions[VariableID](
    Pos(3, 6, 3, 7),
    Pos(4, 6, 4, 7),
    Pos(5, 16, 5, 17),
    Pos(6, 13, 6, 14),
    Pos(8, 19, 8, 20),
    Pos(8, 30, 8, 31),
    Pos(10, 8, 10, 9),
    Pos(10, 21, 10, 22),
    Pos(14, 11, 14, 12),
    Pos(14, 21, 14, 22),
    Pos(15, 7, 15, 8),
    Pos(15, 12, 15, 13),
    Pos(15, 18, 15, 19),
    Pos(15, 24, 15, 25),
    Pos(15, 29, 15, 30),
    Pos(15, 37, 15, 38),
    Pos(15, 42, 15, 43),
    Pos(15, 51, 15, 52),
    Pos(16, 12, 16, 13),
    Pos(16, 16, 16, 17),
    Pos(18, 10, 18, 11),
    Pos(18, 14, 18, 15),
    Pos(20, 9, 20, 10),
    Pos(20, 16, 20, 17),
    Pos(20, 21, 20, 22),
    Pos(20, 26, 20, 27),
    Pos(21, 10, 21, 11),
    Pos(21, 14, 21, 15),
    Pos(23, 11, 23, 12),
    Pos(23, 18, 23, 19),
    Pos(23, 25, 23, 26),
    Pos(26, 11, 26, 12),
    Pos(26, 16, 26, 17),
    Pos(29, 9, 29, 10),
    Pos(29, 13, 29, 14),
    Pos(30, 7, 30, 8),
    Pos(40, 3, 40, 4),
    Pos(40, 11, 40, 12),
    Pos(40, 13, 40, 14),
    Pos(41, 13, 41, 14),
    Pos(42, 3, 42, 4),
    Pos(42, 10, 42, 11),
    Pos(43, 14, 43, 15),
    Pos(44, 3, 44, 4),
    Pos(44, 5, 44, 6),
    Pos(44, 10, 44, 11),
    Pos(46, 3, 46, 4),
    Pos(46, 7, 46, 8),
    Pos(47, 3, 47, 4),
    Pos(47, 5, 47, 6),
    Pos(47, 9, 47, 10),
    Pos(48, 3, 48, 4),
    Pos(49, 3, 49, 4),
    Pos(49, 8, 49, 9),
    Pos(49, 23, 49, 24),
    Pos(49, 28, 49, 29),
    Pos(50, 3, 50, 4),
    Pos(51, 10, 51, 11)
  )
  testPositions[MethodID](
    Pos(8, 15, 8, 18),
    Pos(12, 13, 12, 18),
    Pos(14, 6, 14, 10),
    Pos(29, 3, 29, 8),
    Pos(40, 6, 40, 10),
    Pos(41, 8, 41, 12),
    Pos(42, 5, 42, 9),
    Pos(43, 9, 43, 13)
  )
  testPositions[NormalAccess](
    Pos(29, 3, 29, 15),
    Pos(41, 3, 41, 18),
    Pos(42, 3, 42, 31),
    Pos(43, 3, 43, 20),
    Pos(47, 3, 47, 6)
  )
  testPositions[SafeAccess](
    Pos(40, 3, 40, 16)
  )
  testPositions[Assign](
    Pos(44, 3, 44, 16),
    Pos(46, 3, 46, 12),
    Pos(47, 3, 47, 21),
    Pos(48, 3, 48, 15),
    Pos(49, 3, 49, 35),
    Pos(50, 3, 50, 23)
  )
  testPositions[MethodCall](
    Pos(29, 3, 29, 15),
    Pos(40, 6, 40, 16),
    Pos(41, 8, 41, 18),
    Pos(42, 5, 42, 31),
    Pos(43, 9, 43, 20)
  )
  testPositions[This](
    Pos(41, 3, 41, 7)
  )
  testPositions[Super](
    Pos(43, 3, 43, 8)
  )
  testPositions[NewArray](
    Pos(48, 7, 48, 15)
  )
  testPositions[New](
    Pos(50, 7, 50, 23)
  )
  testPositions[Ternary](
    Pos(49, 7, 49, 35)
  )
  testPositions[Elvis](
    Pos(49, 27, 49, 35)
  )
  testPositions[Is](
    Pos(49, 7, 49, 20)
  )
  testPositions[As](
    Pos(44, 10, 44, 16)
  )

}
