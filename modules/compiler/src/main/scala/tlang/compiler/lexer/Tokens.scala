package tlang
package compiler
package lexer

import tlang.utils.{Enumerable, Enumeration, Positioned}

import scala.util.matching.Regex

sealed class Token(val kind: TokenKind) extends Positioned {
  override def toString: String = kind.toString
  override def equals(o: Any): Boolean = o matches {
    case t: Token if t.kind == kind => true
  }
}

sealed class TokenWithValue[T](override val kind: TokenKind, val value: T) extends Token(kind) {
  override def toString: String = s"$kind($value)"
  override def equals(o: Any): Boolean = o matches {
    case t: TokenWithValue[T] if t.kind == kind && t.value == value => true
  }
}

sealed abstract class TokenKind(val str: String) extends Ordered[TokenKind] {
  def compare(that: TokenKind): Int = str.length - that.str.length

  override def toString: String = str
}

object Tokens extends Enumerable[TokenKind] {

  // @formatter:off

  case object IDKIND         extends TokenKind("") { override def toString = "Id" }
  case object INTLITKIND     extends TokenKind("") { override def toString = "Int" }
  case object LONGLITKIND    extends TokenKind("") { override def toString = "Long" }
  case object FLOATLITKIND   extends TokenKind("") { override def toString = "Float" }
  case object DOUBLELITKIND  extends TokenKind("") { override def toString = "Double" }
  case object CHARLITKIND    extends TokenKind("") { override def toString = "Char" }
  case object STRLITKIND     extends TokenKind("") { override def toString = "String" }
  case object COMMENTLITKIND extends TokenKind("") { override def toString = "Comment" }

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
  case object LSHIFTEQ        extends TokenKind("<<=")
  case object RSHIFTEQ        extends TokenKind(">>=")
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
  case object LSHIFT          extends TokenKind("<<")
  case object RSHIFT          extends TokenKind(">>")
  case object LESSTHAN        extends TokenKind("<")
  case object LESSTHANEQ      extends TokenKind("<=")
  case object GREATERTHAN     extends TokenKind(">")
  case object GREATERTHANEQ   extends TokenKind(">=")
  case object PLUS            extends TokenKind("+")
  case object MINUS           extends TokenKind("-")
  case object TIMES           extends TokenKind("*")
  case object DIV             extends TokenKind("/")
  case object EXTRACTNULLABLE extends TokenKind("!!")
  case object AT              extends TokenKind("@")
  case object PACKAGE         extends TokenKind("package")
  case object IMPORT          extends TokenKind("import")
  case object IS              extends TokenKind("is")
  case object AS              extends TokenKind("as")
  case object CLASS           extends TokenKind("class")
  case object EXTENSION       extends TokenKind("extension")
  case object TRAIT           extends TokenKind("trait")
  case object ANNOTATION      extends TokenKind("annotation")
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
  case object NEWLINE         extends TokenKind("\\n")
  case object INDENT          extends TokenKind("<indentation>")
  case object DEDENT          extends TokenKind("<dedentation>")
  // @formatter:on

  case class ID(override val value: String) extends TokenWithValue[String](IDKIND, value)
  case class INTLIT(override val value: Int) extends TokenWithValue[Int](INTLITKIND, value)
  case class LONGLIT(override val value: Long) extends TokenWithValue[Long](LONGLITKIND, value)
  case class FLOATLIT(override val value: Float) extends TokenWithValue[Float](FLOATLITKIND, value)
  case class DOUBLELIT(override val value: Double) extends TokenWithValue[Double](DOUBLELITKIND, value)
  case class COMMENTLIT(override val value: String) extends TokenWithValue[String](COMMENTLITKIND, value)
  case class CHARLIT(override val value: Char) extends TokenWithValue[Char](CHARLITKIND, value) {
    override def toString: String = s"'$value'"
  }
  case class STRLIT(override val value: String) extends TokenWithValue[String](STRLITKIND, value) {
    override def toString: String = '"' + value + '"'
  }

  override lazy val Values: List[TokenKind] = Enumeration.instancesOf[TokenKind]
  lazy val Keywords: Set[TokenKind] = Tokens.filter(_.str.matches("[A-Za-z]+")).toSet
  lazy val KeywordMap: Map[String, TokenKind] = Keywords.map(t => t.str -> t).toMap
  lazy val NonKeywords: Map[String, TokenKind] = Tokens.filter(t => t.str.length > 0 && !KeywordMap.contains(t.str)).map(t => t.str -> t).toMap
  lazy val KeywordsRegex: Regex = s"(${ Keywords.toList.sortBy(-_.str.length).mkString("|") })".r
}
