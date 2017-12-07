package tlang.compiler.messages

import tlang.compiler.Main
import tlang.formatting.textformatters.TabReplacer
import tlang.formatting.{Formatter, SimpleFormatting}
import tlang.utils.Extensions._

object CompilationException {
  private def constructCompilerMessages(compilerMessage: CompilerMessage) = {
    val formatter = Formatter(SimpleFormatting)
    CompilerMessages(formatter, MessageFormatter(formatter, TabReplacer(3))) use { _ += compilerMessage }
  }
}

class CompilationException(val messages: CompilerMessages) extends Exception() {

  def this(compilerMessage: CompilerMessage) = {
    this(CompilationException.constructCompilerMessages(compilerMessage))
  }

  override def getMessage: String =
    NL + messages.formatMessages(MessageType.Warning) + messages.formatMessages(MessageType.Error)
}