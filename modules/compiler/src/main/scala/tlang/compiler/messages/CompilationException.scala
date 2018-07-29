package tlang
package compiler
package messages


class CompilationException(val messages: CompilerMessages) extends Exception() {

  def this(compilerMessage: CompilerMessage) = this(CompilerMessages() use { _ += compilerMessage })
  override def getMessage: String = messages.toString
}
