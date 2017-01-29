package tcompiler.error

import tcompiler.lexer.Tokens._
import tcompiler.lexer.{Token, Tokenizer, Tokens}
import tcompiler.utils.Extensions._
import tcompiler.utils.{Colorizer, Context}

/**
  * Created by Tim Lindeberg on 1/29/2017.
  */
class SyntaxHighlighter(colorizer: Colorizer) {

  import colorizer._

  val context = Context(new VoidReporter(), Nil)

  def apply(code: String): String = {
    if (!colorizer.useColor)
      return code

    val tokenizer = new Tokenizer(context, None)
    val tokens = tokenizer.apply(code.toList)
    val sb = new StringBuilder()
    sb ++= code.substring(0, tokens.head.col - 1)
    for (t1 :: t2 :: Nil <- tokens.sliding(2)) {
      val start = t1.col - 1
      val end = t1.endCol - 1

      val color = getColor(t1)
      sb ++= color(code.substring(start, end))
      if (t2.kind != EOF)
        sb ++= code.substring(end, t2.col - 1)
    }
    if (tokens.length >= 2) {
      val last = tokens(tokens.length - 2)
      sb ++= code.substring(last.endCol - 1, code.length)
    }
    sb.toString()
  }


  def getColor(token: Token): String => String = token.kind match {
    case INTLITKIND | LONGLITKIND | FLOATLITKIND | DOUBLELITKIND => NumColor
    case CHARLITKIND | STRLITKIND                                => StringColor
    case IDKIND                                                  => VarColor
    case COMMENT                                                 => CommentColor
    case x if x in Tokens.Keywords                               => KeywordColor
    case _                                                       => identity
  }


}
