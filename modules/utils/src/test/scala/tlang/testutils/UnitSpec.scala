package tlang.testutils

import better.files.File
import org.scalatest.{FlatSpec, Inspectors, Matchers, OptionValues}
import tlang.formatting.Colors.ColorScheme
import tlang.formatting.Colors.ColorScheme.DefaultColorScheme
import tlang.formatting._
import tlang.formatting.textformatters._
import tlang.testutils.snapshot.SnapshotTesting
import tlang.utils.Extensions._

trait UnitSpec extends FlatSpec
  with Matchers
  with Inspectors
  with OptionValues
  with AnsiMatchers
  with MockitoSugar
  with SnapshotTesting {

  def mockedWordWrapperReturningSplitLines: WordWrapper = {
    mock[WordWrapper] use { wordWrapper =>
      wordWrapper.apply(*, *) answers { _.getArgument[String](0).split("\r?\n", -1).toList }
      wordWrapper.wrapAnsiFormatting(*) forwardsArg 0
    }
  }

  def mockedTruncatorReturningSameLine: Truncator = {
    mock[Truncator] use { truncator =>
      truncator.apply(*, *) answers { _.getArgument[String](0) }
    }
  }

  def mockedSyntaxHighlighter: SyntaxHighlighter =
    mock[SyntaxHighlighter] use { _.apply(*).forwardsArg(0) }


  def memoryFile(content: String = ""): (StringBuilder, File) = {
    val buffer = new StringBuilder
    val file = mock[File]
    // Save the data to a local stringBuilder instead
    file.write(*)(*, *) answers { invocation =>
      buffer.clear
      buffer ++= invocation.getArgument(0)
      file
    }

    file.appendLine(*)(*) answers { invocation =>
      buffer ++= invocation.getArgument(0)
      buffer ++= NL
      file
    }

    file.exists returns true
    file.lineIterator returns content.lines

    (buffer, file)
  }


  def testFormatter(
    width: Int = 80,
    useColor: Boolean = true,
    asciiOnly: Boolean = true,
    colorScheme: ColorScheme = DefaultColorScheme,
    wordWrapper: WordWrapper = mock[WordWrapper],
    truncator: Truncator = mock[Truncator],
    tabReplacer: TabReplacer = mock[TabReplacer],
    syntaxHighlighter: SyntaxHighlighter = mock[SyntaxHighlighter],
    stackTraceHighlighter: StackTraceHighlighter = mock[StackTraceHighlighter]
  ): Formatter = {
    Formatter(
      wordWrapper,
      truncator,
      tabReplacer,
      width,
      colorScheme,
      useColor,
      asciiOnly
    )
  }

}
