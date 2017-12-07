package tlang.compiler.utils

import tlang.compiler.lexer.{Lexer, Token}
import tlang.compiler.lexer.Tokens._
import tlang.compiler.messages.VoidReporter
import tlang.formatting.Colors.Color
import tlang.formatting.{ErrorStringContext, Formatting}
import tlang.formatting.textformatters.{Coloring, SyntaxHighlighter}
import tlang.utils._
import tlang.utils.Extensions._


object TLangSyntaxHighlighter {

  def apply(formatting: Formatting): TLangSyntaxHighlighter = {
    val errorStringContext = ErrorStringContext(null)
    val lexer = new Lexer(VoidReporter(), errorStringContext) {
      override lazy val logger: Logger = new Logger()(LoggingSettings(logLevel = LogLevel.Off))
    }

    new TLangSyntaxHighlighter(formatting, lexer)
  }

  def lexing(formatting: Formatting, lexer: Lexer): String => List[Coloring] = {
    import formatting._
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

class TLangSyntaxHighlighter(formatting: Formatting, lexer: Lexer) extends
  SyntaxHighlighter(formatting)(TLangSyntaxHighlighter.lexing(formatting, lexer))