package tlang.testutils

import org.markushauck.mockito.MockitoSugar
import org.scalatest.{FlatSpec, Matchers}
import tlang.formatting.Colors.ColorScheme
import tlang.formatting.Colors.ColorScheme.DefaultColorScheme
import tlang.formatting._
import tlang.formatting.textformatters.{StackTraceHighlighter, SyntaxHighlighter, Truncator, WordWrapper}
import tlang.utils.Extensions._

trait UnitSpec extends FlatSpec with Matchers with AnsiMatchers with MockitoSugar {


  // For scoping and readability
  def test[U](description: String = "")(f: => U): U = f

  def mockedWordWrapperReturningSameLine: WordWrapper = {
    mock[WordWrapper] use { wordWrapper =>
      wordWrapper.apply(*, *) answers { _.getArgument[String](0) :: Nil }
    }
  }

  def *[T]: T = org.mockito.ArgumentMatchers.any[T]()

  def createMockFormatter(
    width: Int = 80,
    useColor: Boolean = true,
    asciiOnly: Boolean = true,
    colorScheme: ColorScheme = DefaultColorScheme,
    formatting: Option[Formatting] = None,
    wordWrapper: WordWrapper = mock[WordWrapper],
    truncator: Truncator = mock[Truncator],
    syntaxHighlighter: SyntaxHighlighter = mock[SyntaxHighlighter],
    stackTraceHighlighter: StackTraceHighlighter = mock[StackTraceHighlighter]
  ): Formatter = {

    Formatter(
      formatting = formatting.getOrElse(Formatting(width, colorScheme, useColor, asciiOnly)),
      wordWrapper,
      truncator,
      syntaxHighlighter,
      stackTraceHighlighter
    )
  }

}
