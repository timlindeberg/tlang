package tlang.compiler.ast

import java.io.File

import org.scalatest.{FunSuite, Matchers}
import tlang.compiler.ast.Trees._
import tlang.compiler.lexer.Lexer
import tlang.compiler.{Context, Pos, Tester}
import tlang.utils.FileSource

import scala.reflect.{ClassTag, classTag}

/**
  * Created by Tim Lindeberg on 1/19/2017.
  */
class ParserPositionSpec extends FunSuite with Matchers {

  private val NoPos      : Pos     = Pos(-1, -1, -1, -1)
  private val TestFile   : String  = Tester.Resources + "positions/ParserPositions.t"
  private val TestContext: Context = Tester.testContext

  private val Tree: Tree = {
    val file = FileSource(new File(TestFile)) :: Nil
    (Lexer andThen Parser).run(TestContext)(file).head
  }

  private val Trees: Map[Class[_], List[Tree]] = Tree.groupBy(_.getClass)

  private def testPositions[T <: Tree : ClassTag](positions: Pos*): Unit = {
    val clazz = classTag[T].runtimeClass
    val className = clazz.getSimpleName
    test(className) {
      val className = clazz.getSimpleName
      val trees = Trees
        .getOrElse(clazz, fail(s"No trees of class $className"))
        .filter { t => new Pos(t) != NoPos }

      trees.zip(positions) foreach { case (tree, expectedPos) =>
        val pos = new Pos(tree)
        pos shouldBe expectedPos
      }
    }
  }

  testPositions[CompilationUnit](Pos(7, 1, 67, 18))
  testPositions[ClassDecl](Pos(7, 1, 60, 2))
  testPositions[TraitDecl](Pos(62, 1, 65, 2))
  testPositions[ExtensionDecl](Pos(67, 1, 67, 18))

  testPositions[MethodDecl](
    Pos(18, 5, 18, 34),
    Pos(20, 5, 58, 6)
  )

  testPositions[ConstructorDecl](Pos(14, 5, 14, 50))
  testPositions[OperatorDecl](Pos(16, 5, 16, 51))

  testPositions[Formal](
    Pos(14, 22, 14, 31),
    Pos(14, 33, 14, 44),
    Pos(16, 11, 16, 22),
    Pos(16, 24, 16, 35),
    Pos(20, 14, 20, 22),
    Pos(20, 24, 20, 31)
  )

  testPositions[ArrayType](Pos(14, 36, 14, 44))
  testPositions[NullableType](Pos(20, 17, 20, 22))
  testPositions[UnitType](Pos(18, 25, 18, 29))

  testPositions[VarDecl](
    Pos(9, 5, 9, 19),
    Pos(10, 5, 10, 14),
    Pos(11, 5, 11, 26),
    Pos(12, 5, 12, 32),
    Pos(29, 13, 29, 22),
    Pos(32, 13, 32, 18),
    Pos(36, 9, 43, 10)
  )
  testPositions[Block](
    Pos(14, 48, 14, 50),
    Pos(16, 49, 16, 51),
    Pos(18, 32, 18, 34),
    Pos(20, 35, 58, 6)
  )

  testPositions[If](Pos(21, 9, 24, 25))
  testPositions[While](Pos(26, 9, 27, 25))
  testPositions[For](Pos(29, 9, 30, 21))
  testPositions[Foreach](Pos(32, 9, 33, 18))
  testPositions[Error](Pos(27, 13, 27, 25))
  testPositions[Return](Pos(57, 9, 57, 17))
  testPositions[Break](Pos(33, 13, 33, 18))
  testPositions[Continue](Pos(30, 13, 30, 21))
  testPositions[Print](Pos(24, 13, 24, 25))
  testPositions[Println](Pos(22, 13, 22, 27))

  testPositions[Plus](Pos(22, 21, 22, 26))
  testPositions[Minus](Pos(24, 19, 24, 24))
  testPositions[Times](Pos(27, 19, 27, 24))
  testPositions[Div](Pos(35, 15, 35, 20))
  testPositions[Modulo](Pos(37, 13, 37, 19))
  testPositions[LogicAnd](Pos(38, 13, 38, 26))
  testPositions[LogicOr](Pos(39, 13, 39, 19))
  testPositions[LogicXor](Pos(40, 13, 40, 22))
  testPositions[LeftShift](Pos(41, 13, 41, 26))
  testPositions[RightShift](Pos(42, 13, 42, 26))

  testPositions[LessThan](
    Pos(21, 12, 21, 19),
    Pos(29, 24, 29, 29)
  )
  testPositions[LessThanEquals](Pos(21, 23, 21, 31))
  testPositions[GreaterThan](Pos(21, 35, 21, 44))
  testPositions[GreaterThanEquals](Pos(21, 48, 21, 58))

  testPositions[Equals](Pos(26, 15, 26, 23))
  testPositions[NotEquals](Pos(26, 27, 26, 33))

  testPositions[And](
    Pos(21, 12, 21, 58),
    Pos(21, 12, 21, 44),
    Pos(21, 12, 21, 31)
  )
  testPositions[Or](Pos(26, 15, 26, 33))

  testPositions[Not](Pos(21, 12, 21, 14))
  testPositions[Hash](Pos(21, 17, 21, 19))
  testPositions[Negation](Pos(21, 23, 21, 25))
  testPositions[LogicNot](Pos(21, 29, 21, 31))
  testPositions[ExtractNullable](Pos(21, 35, 21, 38))
  testPositions[PreIncrement](Pos(21, 41, 21, 44))
  testPositions[PostIncrement](Pos(21, 48, 21, 51))
  testPositions[PreDecrement](Pos(21, 55, 21, 58))
  testPositions[PostDecrement](Pos(26, 15, 26, 18))

  testPositions[ArrayRead](
    Pos(46, 17, 46, 21),
    Pos(50, 9, 50, 13)
  )
  testPositions[ArraySlice](
    Pos(47, 19, 47, 23),
    Pos(48, 16, 48, 24),
    Pos(49, 20, 49, 25),
    Pos(52, 13, 52, 18),
    Pos(53, 15, 53, 22)
  )
  testPositions[IntLit](
    Pos(9, 18, 9, 19),
    Pos(10, 13, 10, 14),
    Pos(29, 21, 29, 22),
    Pos(29, 28, 29, 29),
    Pos(37, 13, 37, 14),
    Pos(39, 13, 39, 15),
    Pos(39, 18, 39, 19),
    Pos(48, 18, 48, 19),
    Pos(48, 20, 48, 21),
    Pos(48, 22, 48, 23),
    Pos(49, 23, 49, 24),
    Pos(52, 15, 52, 16),
    Pos(53, 17, 53, 18),
    Pos(53, 19, 53, 20),
    Pos(54, 19, 54, 20),
    Pos(55, 39, 55, 40),
    Pos(56, 24, 56, 25),
    Pos(56, 27, 56, 28)
  )
  testPositions[LongLit](Pos(37, 17, 37, 19))
  testPositions[FloatLit](Pos(38, 19, 38, 26))
  testPositions[DoubleLit](
    Pos(38, 13, 38, 16),
    Pos(40, 13, 40, 16)
  )
  testPositions[CharLit](Pos(40, 19, 40, 22))
  testPositions[StringLit](
    Pos(12, 27, 12, 32),
    Pos(41, 13, 41, 18)
  )
  testPositions[TrueLit](Pos(41, 22, 41, 26))
  testPositions[FalseLit](Pos(42, 13, 42, 18))
  testPositions[NullLit](Pos(42, 22, 42, 26))
  testPositions[ArrayLit](Pos(36, 17, 43, 10))

  testPositions[ClassID](
    Pos(7, 7, 7, 15),
    Pos(7, 13, 7, 14),
    Pos(7, 18, 7, 19),
    Pos(7, 21, 7, 22),
    Pos(7, 24, 7, 25),
    Pos(9, 12, 9, 15),
    Pos(11, 22, 11, 26),
    Pos(12, 19, 12, 24),
    Pos(14, 25, 14, 31),
    Pos(14, 36, 14, 42),
    Pos(16, 14, 16, 22),
    Pos(16, 20, 16, 21),
    Pos(16, 27, 16, 35),
    Pos(16, 33, 16, 34),
    Pos(16, 38, 16, 46),
    Pos(16, 44, 16, 45),
    Pos(20, 17, 20, 21),
    Pos(20, 27, 20, 31),
    Pos(50, 21, 50, 22),
    Pos(54, 17, 54, 18),
    Pos(55, 19, 55, 25),
    Pos(56, 17, 56, 23),
    Pos(62, 7, 62, 8),
    Pos(67, 11, 67, 12)
  )
  // Skipping some VariableID since there are so many
  testPositions[VariableID](
    Pos(9, 9, 9, 10),
    Pos(10, 9, 10, 10),
    Pos(11, 19, 11, 20),
    Pos(12, 16, 12, 17),
    Pos(14, 22, 14, 23),
    Pos(14, 33, 14, 34),
    Pos(16, 11, 16, 12),
    Pos(16, 24, 16, 25)
  )
  testPositions[MethodID](
    Pos(14, 18, 14, 21),
    Pos(18, 16, 18, 21),
    Pos(20, 9, 20, 13),
    Pos(35, 9, 35, 14),
    Pos(46, 12, 46, 16),
    Pos(47, 14, 47, 18),
    Pos(48, 11, 48, 15),
    Pos(49, 15, 49, 19)
  )
  testPositions[NormalAccess](
    Pos(35, 9, 35, 21),
    Pos(47, 9, 47, 24),
    Pos(48, 9, 48, 25),
    Pos(49, 9, 49, 26),
    Pos(53, 9, 53, 12)
  )
  testPositions[SafeAccess](Pos(46, 9, 46, 22))
  testPositions[Assign](
    Pos(50, 9, 50, 22),
    Pos(52, 9, 52, 18),
    Pos(53, 9, 53, 22),
    Pos(54, 9, 54, 21),
    Pos(55, 9, 55, 41),
    Pos(56, 9, 56, 29)
  )

  testPositions[MethodCall](
    Pos(35, 9, 35, 21),
    Pos(46, 12, 46, 22),
    Pos(47, 14, 47, 24),
    Pos(48, 11, 48, 25),
    Pos(49, 15, 49, 26)
  )
  testPositions[This](Pos(47, 9, 47, 13))
  testPositions[Super](Pos(49, 9, 49, 14))
  testPositions[NewArray](Pos(54, 13, 54, 21))
  testPositions[New](Pos(56, 13, 56, 29))
  testPositions[Ternary](Pos(55, 13, 55, 41))
  testPositions[Elvis](Pos(55, 33, 55, 41))
  testPositions[Is](Pos(55, 13, 55, 26))
  testPositions[As](Pos(50, 16, 50, 22))

}
