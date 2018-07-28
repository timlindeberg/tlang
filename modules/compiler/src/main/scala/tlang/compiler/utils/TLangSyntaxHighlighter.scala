package tlang.compiler.utils

import tlang.compiler.lexer.Tokens._
import tlang.compiler.lexer.{Lexer, Token}
import tlang.compiler.messages.VoidReporter
import tlang.formatting.Colors.Color
import tlang.formatting.textformatters.{Coloring, SyntaxHighlighter}
import tlang.formatting.{ErrorStringContext, Formatter}
import tlang.utils.Extensions._
import tlang.utils._


object TLangSyntaxHighlighter {

  def apply()(implicit formatter: Formatter): TLangSyntaxHighlighter = {
    val errorStringContext = ErrorStringContext(null)(Formatter.SimpleFormatter)
    val lexer = new Lexer(VoidReporter(), errorStringContext) {
      override lazy val logger: Logger = new Logger()(LoggingSettings(logLevel = LogLevel.Off))
    }

    new TLangSyntaxHighlighter(lexer)
  }

  def lexing(lexer: Lexer)(implicit formatter: Formatter): String => List[Coloring] = {
    import formatter._
    def getColor(token: Token): Color = token.kind match {
      case NEWLINE | INDENT | DEDENT | BAD                         => NoColor
      case COMMENTLITKIND                                          => CommentColor
      case INTLITKIND | LONGLITKIND | FLOATLITKIND | DOUBLELITKIND => NumColor
      case CHARLITKIND | STRLITKIND                                => StringColor
      case IDKIND                                                  => VarColor
      case x if x in Keywords                                      => KeywordColor
      case _                                                       => SymbolColor
    }

    s: String => lexer
      .apply(StringSource(s, ""))
      .map { token => Coloring(getColor(token), token) }
  }

}

class TLangSyntaxHighlighter(lexer: Lexer)(implicit formatter: Formatter) extends
  SyntaxHighlighter(TLangSyntaxHighlighter.lexing(lexer))
