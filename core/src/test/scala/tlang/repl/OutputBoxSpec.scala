package tlang.repl

import tlang.formatting.textformatters.{SyntaxHighlighter, TabReplacer}
import tlang.messages.MessageFormatter
import tlang.testutils.UnitSpec

class OutputBoxSpec extends UnitSpec {

  behavior of "An output box"

  it should "render a welcome box" in {
    val outputBox = createOutputBox().welcome()

    outputBox.renderState.header shouldBe "Welcome to the T-REPL!"
    val content = outputBox.renderState.contentAsString
    content should include("CTRL + Space")
    content should include("CTRL + C")
    content should include("help")
    content should include("quit")
    content should include("help")
  }

  it should "render an exit message" in {
    val outputBox = createOutputBox().exit()

    outputBox.renderState.header shouldBe "Input"
    outputBox.renderState.contentAsString shouldBe "Thanks for using the T-REPL!"
  }


  it should "render new input" in {


  }

  private def createOutputBox(
    width: Int = 30,
    useColor: Boolean = false,
    maxOutputLines: Int = 5,
    syntaxHighlighter: SyntaxHighlighter = mock[SyntaxHighlighter],
    messageFormatter: MessageFormatter = mock[MessageFormatter],
    tabReplacer: TabReplacer = mock[TabReplacer]
  ) = {
    OutputBox(
      createMockFormatter(width = width, useColor = useColor, syntaxHighlighter = syntaxHighlighter),
      tabReplacer,
      messageFormatter,
      maxOutputLines
    )
  }
}
