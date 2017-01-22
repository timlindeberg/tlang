package tcompiler.positions

import java.io.File

import org.scalatest.{FlatSpec, Matchers}
import tcompiler.TestUtils
import tcompiler.ast.Parser
import tcompiler.ast.Trees.{ClassDecl, CompilationUnit, ExtensionDecl, ExtensionImport, RegularImport, TraitDecl, Tree, WildCardImport}
import tcompiler.lexer.Tokens._
import tcompiler.lexer.{Lexer, Token}
import tcompiler.utils.Extensions._
import tcompiler.utils.{Context, Positioned}

import scala.reflect.{ClassTag, classTag}

/**
  * Created by Tim Lindeberg on 1/19/2017.
  */
class PositionSpec extends FlatSpec with Matchers {

  case class Pos(line: Int, col: Int, endLine: Int, endCol: Int) {
    def this(pos: Positioned) = this(pos.line, pos.col, pos.endLine, pos.endCol)
  }

  val LexerTestFile : String = TestUtils.Resources + "positions/LexerPositions.kool"
  val ParserTestFile: String = TestUtils.Resources + "positions/LexerPositions.kool"

  val TestContext: Context = TestUtils.getTestContext(None)

  val Tokens: List[Token] = {
    val file = new File(LexerTestFile)
    Lexer.run(TestContext)(List(file)).head
  }

  val Trees: Map[Class[_], List[Tree]] = {
    val file = new File(ParserTestFile)
    (Lexer andThen Parser).run(TestContext)(List(file)).head.groupBy(_.getClass)
  }

  def testPositions(predicate: Token => Boolean, positions: (String, Pos)*): Unit = {
    val t = Tokens
      .filterNot(_.kind == NEWLINE)
      .filter(predicate)

    assert(t.length == positions.length)

    val tests = t.zip(positions)

    tests foreach { case (token, (name, expectedPos)) =>
      it should name in {
        val pos = new Pos(token)
        pos shouldBe expectedPos
      }
    }
  }

  def testPositions[T: ClassTag](positions: Pos*): Unit = {
    val clazz = classTag[T].runtimeClass
    val trees = Trees(clazz)

    assert(trees.length == positions.length)

    trees.zip(positions) foreach { case (tree, expectedPos) =>
      it should clazz.getSimpleName in {
        val pos = new Pos(tree)
        pos shouldBe expectedPos
      }
    }
  }

  behavior of "Lexer Positions"

  // @formatter:off

  /*-------------------------------- Basic tokens --------------------------------*/
  testPositions(_.kind.str.length > 0,
    ";"         -> Pos(1, 1, 1, 2),
    "."         -> Pos(1, 3, 1, 4),
    ":"         -> Pos(1, 5, 1, 6),
    ","         -> Pos(2, 1, 2, 2),
    "="         -> Pos(2, 3, 2, 4),
    "+="        -> Pos(2, 5, 2, 7),
    "-="        -> Pos(3, 1, 3, 3),
    "*="        -> Pos(3, 4, 3, 6),
    "/="        -> Pos(3, 7, 3, 9),
    "%="        -> Pos(4, 1, 4, 3),
    "&="        -> Pos(4, 4, 4, 6),
    "|="        -> Pos(4, 7, 4, 9),
    "^="        -> Pos(5, 1, 5, 3),
    "<<="       -> Pos(5, 4, 5, 7),
    ">>="       -> Pos(5, 8, 5, 11),
    "=="        -> Pos(6, 1, 6, 3),
    "!="        -> Pos(6, 4, 6, 6),
    "!"         -> Pos(6, 7, 6, 8),
    "#"         -> Pos(7, 1, 7, 2),
    "++"        -> Pos(7, 3, 7, 5),
    "--"        -> Pos(7, 6, 7, 8),
    "("         -> Pos(8, 1, 8, 2),
    ")"         -> Pos(8, 2, 8, 3),
    "["         -> Pos(8, 3, 8, 4),
    "]"         -> Pos(9, 1, 9, 2),
    "{"         -> Pos(9, 2, 9, 3),
    "}"         -> Pos(9, 3, 9, 4),
    "&&"        -> Pos(10, 1, 10, 3),
    "||"        -> Pos(10, 3, 10, 5),
    "?"         -> Pos(10, 5, 10, 6),
    "?."        -> Pos(11, 1, 11, 3),
    "?:"        -> Pos(11, 4, 11, 6),
    "~"         -> Pos(11, 7 ,11, 8),
    "&"         -> Pos(12, 1, 12, 2),
    "|"         -> Pos(12, 3, 12, 4),
    "^"         -> Pos(12, 5, 12, 6),
    "%"         -> Pos(13, 1, 13, 2),
    "<<"        -> Pos(13, 3, 13, 5),
    ">>"        -> Pos(13, 6, 13, 8),
    "<"         -> Pos(14, 1, 14, 2),
    "<="        -> Pos(14, 3, 14, 5),
    ">"         -> Pos(14, 6, 14, 7),
    ">="        -> Pos(15, 1, 15, 3),
    "+"         -> Pos(15, 4, 15, 5),
    "-"         -> Pos(15, 6, 15, 7 ),
    "*"         -> Pos(16, 1, 16, 2),
    "/"         -> Pos(16, 3, 16, 4),
    "!!"        -> Pos(16, 5, 16, 7),
    "package"   -> Pos(17, 1, 17, 8),
    "import"    -> Pos(17, 9, 17, 15),
    "is"        -> Pos(17, 16, 17, 18),
    "as"        -> Pos(18, 1, 18, 3),
    "object"    -> Pos(18, 4, 18, 10),
    "class"     -> Pos(18, 11, 18, 16),
    "extension" -> Pos(19, 1, 19, 10),
    "trait"     -> Pos(19, 11, 19, 16),
    "Def"       -> Pos(19, 17, 19, 20),
    "def"       -> Pos(20, 1, 20, 4),
    "protected" -> Pos(20, 5, 20, 14),
    "Var"       -> Pos(20, 15, 20, 18),
    "Val"       -> Pos(21, 1, 21, 4),
    "var"       -> Pos(21, 5, 21, 8),
    "val"       -> Pos(21, 9, 21, 12),
    "static"    -> Pos(22, 1, 22, 7),
    "while"     -> Pos(22, 8, 22, 13),
    "for"       -> Pos(22, 14, 22, 17),
    "if"        -> Pos(23, 1, 23, 3),
    "else"      -> Pos(23, 4, 23, 8),
    "return"    -> Pos(23, 9, 23, 15),
    "true"      -> Pos(24, 1, 24, 5),
    "false"     -> Pos(24, 6, 24, 11),
    "this"      -> Pos(24, 12, 24, 16),
    "super"     -> Pos(25, 1, 25, 6),
    "new"       -> Pos(25, 7, 25, 10),
    "implicit"  -> Pos(25, 11, 25,19),
    "print"     -> Pos(27, 1, 27, 6),
    "println"   -> Pos(27, 7, 27, 14),
    "error"     -> Pos(27, 15, 27, 20),
    "break"     -> Pos(29, 5, 29, 10),
    "continue"  -> Pos(29, 19, 29, 27),
    "in"        -> Pos(29, 37, 29, 39),
    "null"      -> Pos(31, 1, 31, 5)
  )

  /*-------------------------------- Identifiers --------------------------------*/
  testPositions(_.kind == IDKIND,
    "identifier" -> Pos(33, 1, 33, 11),
    "a"          -> Pos(33, 12, 33, 13),
    "hej"        -> Pos(33, 14, 33, 17),
    "test"       -> Pos(33, 23, 33, 27),
    "id"         -> Pos(33, 28, 33, 30)
  )

  /*-------------------------------- Number literals --------------------------------*/
  testPositions(_.kind in List(INTLITKIND, LONGLITKIND, FLOATLITKIND, DOUBLELITKIND),
    "0"           -> Pos(38, 6, 38, 7),
    "123456789"   -> Pos(39, 6, 39, 15),
    "1_234_567"   -> Pos(40, 6, 40, 15),
    "0L"          -> Pos(41, 6, 41, 8),
    "12345l"      -> Pos(42, 6, 42, 12),
    "0xabcdef"    -> Pos(43, 6, 43, 14),
    "0xabcdel"    -> Pos(44, 6, 44, 14),
    "0x123abcdeL" -> Pos(45, 6, 45, 17),
    "0b010101"    -> Pos(46, 6, 46, 14),
    "0b010101L"   -> Pos(47, 6, 47, 15),
    "0.000"       -> Pos(48, 6, 48, 11),
    "0.000f"      -> Pos(49, 6, 49, 12),
    "0.01F"       -> Pos(50, 6, 50, 11),
    "0.1e-5F"     -> Pos(51, 6, 51, 13),
    "0.1E123F"    -> Pos(52, 6, 52, 14)
  )

  /*-------------------------------- Char literals --------------------------------*/
  testPositions(_.kind == CHARLITKIND,
    "'a'"          -> Pos(55, 6, 55, 9),
    "'\\n'"        -> Pos(56, 6, 56, 10),
    "'\\uabcd'"    -> Pos(57, 6, 57, 14)
  )

  /*-------------------------------- String literals --------------------------------*/
  testPositions(_.kind == STRLITKIND,
    "\"\""                            -> Pos(59, 6, 59, 8),
    "\"string\""                      -> Pos(60, 6, 60, 14),
    "\"stri\\ng\\uabcdhej\""          -> Pos(61, 6, 61, 24),
    "eine kleine multilinen stringen" -> Pos(62, 6, 65, 7)
  )

  // @formatter:on

  behavior of "Parser Positions"

  testPositions[CompilationUnit](
    Pos(1, 1, 69, 20)
  )
  testPositions[Package](
    Pos(1, 1, 1, 16)
  )
  testPositions[RegularImport](
    Pos(3, 1, 3, 29)
  )
  testPositions[WildCardImport](
    Pos(4, 1, 4, 21)
  )
  testPositions[ExtensionImport](
    Pos(5, 1, 5, 48)
  )
  testPositions[ClassDecl](
    Pos(7, 1, 62, 2)
  )
  testPositions[TraitDecl](
    Pos(64, 1, 67, 2)
  )
  testPositions[ExtensionDecl](
    Pos(69, 1, 69, 20)
  )
}
