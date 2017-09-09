package tlang.compiler.lexer

import better.files.File
import tlang.Context
import tlang.compiler.CompilerIntegrationTestSpec
import tlang.compiler.lexer.Tokens._
import tlang.utils.Extensions._
import tlang.utils.{FileSource, Position}

class LexingPositionSpec extends CompilerIntegrationTestSpec {

  import tlang.testsuites.CompilerIntegrationTestSuite._

  val TestFile   : File    = File(s"$Resources/positions/LexerPositions.t")
  val TestContext: Context = testContext()

  lazy val Tokens: List[Token] = {
    val file = FileSource(TestFile) :: Nil
    Lexing.execute(TestContext)(file).head
  }

  def testPositions(predicate: Token => Boolean, positions: (String, Position)*): Unit = {
    val tokens = Tokens
      .filterNot(_.kind == NEWLINE)
      .filter(predicate)

    val tests = tokens.zip(positions.map(_._2))
    tests foreach { case (foundPos, expectedPos) =>
      assert(foundPos equalPos expectedPos)
    }
  }

  // @formatter:off
  "Basic Tokens" in {
    testPositions(_.kind.str.length > 0,
      ";"         -> Position(1, 1, 1, 2),
      "."         -> Position(1, 3, 1, 4),
      ":"         -> Position(1, 5, 1, 6),
      ","         -> Position(2, 1, 2, 2),
      "="         -> Position(2, 3, 2, 4),
      "+="        -> Position(2, 5, 2, 7),
      "-="        -> Position(3, 1, 3, 3),
      "*="        -> Position(3, 4, 3, 6),
      "/="        -> Position(3, 7, 3, 9),
      "%="        -> Position(4, 1, 4, 3),
      "&="        -> Position(4, 4, 4, 6),
      "|="        -> Position(4, 7, 4, 9),
      "^="        -> Position(5, 1, 5, 3),
      "<<="       -> Position(5, 4, 5, 7),
      ">>="       -> Position(5, 8, 5, 11),
      "=="        -> Position(6, 1, 6, 3),
      "!="        -> Position(6, 4, 6, 6),
      "!"         -> Position(6, 7, 6, 8),
      "#"         -> Position(7, 1, 7, 2),
      "++"        -> Position(7, 3, 7, 5),
      "--"        -> Position(7, 6, 7, 8),
      "("         -> Position(8, 1, 8, 2),
      ")"         -> Position(8, 2, 8, 3),
      "["         -> Position(8, 3, 8, 4),
      "]"         -> Position(9, 1, 9, 2),
      "{"         -> Position(9, 2, 9, 3),
      "}"         -> Position(9, 3, 9, 4),
      "&&"        -> Position(10, 1, 10, 3),
      "||"        -> Position(10, 3, 10, 5),
      "?"         -> Position(10, 5, 10, 6),
      "?."        -> Position(11, 1, 11, 3),
      "?:"        -> Position(11, 4, 11, 6),
      "~"         -> Position(11, 7 ,11, 8),
      "&"         -> Position(12, 1, 12, 2),
      "|"         -> Position(12, 3, 12, 4),
      "^"         -> Position(12, 5, 12, 6),
      "%"         -> Position(13, 1, 13, 2),
      "<<"        -> Position(13, 3, 13, 5),
      ">>"        -> Position(13, 6, 13, 8),
      "<"         -> Position(14, 1, 14, 2),
      "<="        -> Position(14, 3, 14, 5),
      ">"         -> Position(14, 6, 14, 7),
      ">="        -> Position(15, 1, 15, 3),
      "+"         -> Position(15, 4, 15, 5),
      "-"         -> Position(15, 6, 15, 7 ),
      "*"         -> Position(16, 1, 16, 2),
      "/"         -> Position(16, 3, 16, 4),
      "!!"        -> Position(16, 5, 16, 7),
      "package"   -> Position(17, 1, 17, 8),
      "import"    -> Position(17, 9, 17, 15),
      "is"        -> Position(17, 16, 17, 18),
      "as"        -> Position(18, 1, 18, 3),
      "object"    -> Position(18, 4, 18, 10),
      "class"     -> Position(18, 11, 18, 16),
      "extension" -> Position(19, 1, 19, 10),
      "trait"     -> Position(19, 11, 19, 16),
      "Def"       -> Position(19, 17, 19, 20),
      "def"       -> Position(20, 1, 20, 4),
      "protected" -> Position(20, 5, 20, 14),
      "Var"       -> Position(20, 15, 20, 18),
      "Val"       -> Position(21, 1, 21, 4),
      "var"       -> Position(21, 5, 21, 8),
      "val"       -> Position(21, 9, 21, 12),
      "static"    -> Position(22, 1, 22, 7),
      "while"     -> Position(22, 8, 22, 13),
      "for"       -> Position(22, 14, 22, 17),
      "if"        -> Position(23, 1, 23, 3),
      "else"      -> Position(23, 4, 23, 8),
      "return"    -> Position(23, 9, 23, 15),
      "true"      -> Position(24, 1, 24, 5),
      "false"     -> Position(24, 6, 24, 11),
      "this"      -> Position(24, 12, 24, 16),
      "super"     -> Position(25, 1, 25, 6),
      "new"       -> Position(25, 7, 25, 10),
      "implicit"  -> Position(25, 11, 25,19),
      "print"     -> Position(27, 1, 27, 6),
      "println"   -> Position(27, 7, 27, 14),
      "error"     -> Position(27, 15, 27, 20),
      "break"     -> Position(29, 5, 29, 10),
      "continue"  -> Position(29, 19, 29, 27),
      "in"        -> Position(29, 37, 29, 39),
      "null"      -> Position(31, 1, 31, 5)
    )
  }

  "Identifiers" in {
     testPositions(_.kind == IDKIND,
       "identifier" -> Position(33, 1, 33, 11),
       "x"          -> Position(33, 12, 33, 13),
       "hej"        -> Position(33, 14, 33, 17),
       "test"       -> Position(33, 23, 33, 27),
       "id"         -> Position(33, 28, 33, 30),
       "a"          -> Position(67, 1, 67, 2),
       "b"          -> Position(68, 2, 68, 3),
       "c"          -> Position(69, 3, 69, 4),
       "d"          -> Position(70, 8, 70, 9),
       "e"          -> Position(70, 17, 70, 18),
       "f"          -> Position(71, 3, 71, 4)
     )
  }

  "Number literals" in {
    testPositions(_.kind in List(INTLITKIND, LONGLITKIND, FLOATLITKIND, DOUBLELITKIND),
      "0"           -> Position(38, 6, 38, 7),
      "123456789"   -> Position(39, 6, 39, 15),
      "1_234_567"   -> Position(40, 6, 40, 15),
      "0L"          -> Position(41, 6, 41, 8),
      "12345l"      -> Position(42, 6, 42, 12),
      "0xabcdef"    -> Position(43, 6, 43, 14),
      "0xabcdel"    -> Position(44, 6, 44, 14),
      "0x123abcdeL" -> Position(45, 6, 45, 17),
      "0b010101"    -> Position(46, 6, 46, 14),
      "0b010101L"   -> Position(47, 6, 47, 15),
      "0.000"       -> Position(48, 6, 48, 11),
      "0.000f"      -> Position(49, 6, 49, 12),
      "0.01F"       -> Position(50, 6, 50, 11),
      "0.1e-5F"     -> Position(51, 6, 51, 13),
      "0.1E123F"    -> Position(52, 6, 52, 14)
    )
  }

  "Char Literals" in {
    testPositions(_.kind == CHARLITKIND,
      "'a'"       -> Position(55, 6, 55, 9),
      "'\\n'"     -> Position(56, 6, 56, 10),
      "'\\uabcd'" -> Position(57, 6, 57, 14)
    )
  }

  "String literals" in {
    testPositions(_.kind == STRLITKIND,
      "\"\""                            -> Position(59, 6, 59, 8),
      "\"string\""                      -> Position(60, 6, 60, 14),
      "\"stri\\ng\\uabcdhej\""          -> Position(61, 6, 61, 24),
      "eine kleine multilinen stringen" -> Position(62, 6, 65, 7)
    )
  }

  "Comment literals" in {
    testPositions(_.kind == COMMENTLITKIND,
      "/**/"                  -> Position(29, 1, 29, 5),
      "/*  */"                -> Position(29, 12, 29, 18),
      "/******/"              -> Position(29, 28, 29, 36),
      "/** */"                -> Position(29, 40, 29, 46),
      "// null"               -> Position(30, 1, 30, 8),
      "/* null  \\n null  */" -> Position(31, 6, 32, 22)
    )
  }

  "Indents" in {
    testPositions(_.kind == INDENT,
      "a->b"     -> Position(68, 1, 68, 2),
      "b->->c"   -> Position(69, 1, 69, 3),
      "c->->->d" -> Position(70, 1, 70, 4)
    )
  }

  "Dedents" in {
    testPositions(_.kind == DEDENT,
      "d<-<-e"   -> Position(71, 1, 71, 3),
      "e<-<-EOF" -> Position(71, 4, 71, 4),
      "e<-EOF"   -> Position(71, 4, 71, 4)
    )
  }


  // @formatter:on
}
