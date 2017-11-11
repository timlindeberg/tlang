package tlang.compiler
package lexer

import tlang.utils.{Enumerable, Enumeration, Positioned}

import scala.util.matching.Regex

sealed class Token(val kind: TokenKind) extends Positioned {
  override def toString: String = kind.toString
}

sealed abstract class TokenKind(val str: String) extends Ordered[TokenKind] {
  def compare(that: TokenKind): Int = str.length - that.str.length

  override def toString: String = str
}

object Tokens extends Enumerable[TokenKind] {


  object IDKIND extends TokenKind("") {
    override def toString = "Identifier"
  }

  object INTLITKIND extends TokenKind("") {
    override def toString = "Int literal"
  }

  object LONGLITKIND extends TokenKind("") {
    override def toString = "Long literal"
  }

  object FLOATLITKIND extends TokenKind("") {
    override def toString = "Float literal"
  }

  object DOUBLELITKIND extends TokenKind("") {
    override def toString = "Double literal"
  }

  object CHARLITKIND extends TokenKind("") {
    override def toString = "Char literal"
  }

  object STRLITKIND extends TokenKind("") {
    override def toString = "String literal"
  }

  object COMMENTLITKIND extends TokenKind("") {
    override def toString = "Comment literal"
  }


  // @formatter:off
  case object EOF             extends TokenKind("EOF")
  case object BAD             extends TokenKind("")
  case object SEMICOLON       extends TokenKind(";")
  case object DOT             extends TokenKind(".")
  case object COLON           extends TokenKind(":")
  case object COMMA           extends TokenKind(",")
  case object EQSIGN          extends TokenKind("=")
  case object PLUSEQ          extends TokenKind("+=")
  case object MINUSEQ         extends TokenKind("-=")
  case object MULEQ           extends TokenKind("*=")
  case object DIVEQ           extends TokenKind("/=")
  case object MODEQ           extends TokenKind("%=")
  case object ANDEQ           extends TokenKind("&=")
  case object OREQ            extends TokenKind("|=")
  case object XOREQ           extends TokenKind("^=")
  case object LEFTSHIFTEQ     extends TokenKind("<<=")
  case object RIGHTSHIFTEQ    extends TokenKind(">>=")
  case object EQUALS          extends TokenKind("==")
  case object NOTEQUALS       extends TokenKind("!=")
  case object BANG            extends TokenKind("!")
  case object HASH            extends TokenKind("#")
  case object INCREMENT       extends TokenKind("++")
  case object DECREMENT       extends TokenKind("--")
  case object LPAREN          extends TokenKind("(")
  case object RPAREN          extends TokenKind(")")
  case object LBRACKET        extends TokenKind("[")
  case object RBRACKET        extends TokenKind("]")
  case object LBRACE          extends TokenKind("{")
  case object RBRACE          extends TokenKind("}")
  case object AND             extends TokenKind("&&")
  case object OR              extends TokenKind("||")
  case object QUESTIONMARK    extends TokenKind("?")
  case object SAFEACCESS      extends TokenKind("?.")
  case object ELVIS           extends TokenKind("?:")
  case object LOGICNOT        extends TokenKind("~")
  case object LOGICAND        extends TokenKind("&")
  case object LOGICOR         extends TokenKind("|")
  case object LOGICXOR        extends TokenKind("^")
  case object MODULO          extends TokenKind("%")
  case object LEFTSHIFT       extends TokenKind("<<")
  case object RIGHTSHIFT      extends TokenKind(">>")
  case object LESSTHAN        extends TokenKind("<")
  case object LESSTHANEQ      extends TokenKind("<=")
  case object GREATERTHAN     extends TokenKind(">")
  case object GREATERTHANEQ   extends TokenKind(">=")
  case object PLUS            extends TokenKind("+")
  case object MINUS           extends TokenKind("-")
  case object TIMES           extends TokenKind("*")
  case object DIV             extends TokenKind("/")
  case object EXTRACTNULLABLE extends TokenKind("!!")
  case object PACKAGE         extends TokenKind("package")
  case object IMPORT          extends TokenKind("import")
  case object IS              extends TokenKind("is")
  case object AS              extends TokenKind("as")
  case object OBJECT          extends TokenKind("object")
  case object CLASS           extends TokenKind("class")
  case object EXTENSION       extends TokenKind("extension")
  case object TRAIT           extends TokenKind("trait")
  case object PUBDEF          extends TokenKind("Def")
  case object PRIVDEF         extends TokenKind("def")
  case object PROTECTED       extends TokenKind("protected")
  case object PUBVAR          extends TokenKind("Var")
  case object PUBVAL          extends TokenKind("Val")
  case object PRIVVAR         extends TokenKind("var")
  case object PRIVVAL         extends TokenKind("val")
  case object STATIC          extends TokenKind("static")
  case object WHILE           extends TokenKind("while")
  case object FOR             extends TokenKind("for")
  case object IF              extends TokenKind("if")
  case object ELSE            extends TokenKind("else")
  case object RETURN          extends TokenKind("return")
  case object TRUE            extends TokenKind("true")
  case object FALSE           extends TokenKind("false")
  case object THIS            extends TokenKind("this")
  case object SUPER           extends TokenKind("super")
  case object NEW             extends TokenKind("new")
  case object IMPLICIT        extends TokenKind("implicit")
  case object PRINT           extends TokenKind("print")
  case object PRINTLN         extends TokenKind("println")
  case object ERROR           extends TokenKind("error")
  case object BREAK           extends TokenKind("break")
  case object CONTINUE        extends TokenKind("continue")
  case object IN              extends TokenKind("in")
  case object NULL            extends TokenKind("null")
  // @formatter:on

  case object NEWLINE extends TokenKind("\\n")
  case object INDENT extends TokenKind("<indentation>")
  case object DEDENT extends TokenKind("<dedentation>")


  class ID(val value: String) extends Token(IDKIND) {
    override def toString: String = value
  }

  class INTLIT(val value: Int) extends Token(INTLITKIND) {
    override def toString: String = s"$value"
  }

  class LONGLIT(val value: Long) extends Token(LONGLITKIND) {
    override def toString: String = s"$value"
  }

  class FLOATLIT(val value: Float) extends Token(FLOATLITKIND) {
    override def toString: String = s"$value"
  }

  class DOUBLELIT(val value: Double) extends Token(DOUBLELITKIND) {
    override def toString: String = s"$value"
  }

  class CHARLIT(val value: Char) extends Token(CHARLITKIND) {
    override def toString: String = s"'$value'"
  }

  class STRLIT(val value: String) extends Token(STRLITKIND) {
    override def toString: String = '"' + value + '"'
  }

  class COMMENTLIT(val value: String) extends Token(COMMENTLITKIND) {
    override def toString: String = value
  }

  override lazy val Values       : List[TokenKind]        = Enumeration.instancesOf[TokenKind]
  lazy          val Keywords     : Set[TokenKind]         = Tokens.filter(t => t.str.matches("[A-Za-z]+")).toSet
  lazy          val KeywordMap   : Map[String, TokenKind] = Keywords.map(t => t.str -> t).toMap
  lazy          val NonKeywords  : Map[String, TokenKind] = Tokens.filter(t => t.str.length > 0 && !KeywordMap.contains(t.str)).map(t => t.str -> t).toMap
  lazy          val KeywordsRegex: Regex                  = s"(${ Keywords.toList.sortBy(-_.str.length).mkString("|") })".r


}
