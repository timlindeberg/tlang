package tlang.compiler.messages

import tlang.utils.Extensions._

class CompilationException(val messages: CompilerMessages) extends Exception() {

  def this(compilerMessage: CompilerMessage) = this(CompilerMessages() use { _ += compilerMessage })
  override def getMessage: String = messages.toString
}
