package tlang.messages

trait Reporter {

  def printWarnings(): Unit
  def printErrors(): Unit
  def getWarnings: List[CompilerMessage]
  def getErrors: List[CompilerMessage]

  def report(error: CompilerMessage): Unit
  def clear(): Unit
  def terminateIfErrors(): Unit

  def hasErrors: Boolean
  def hasWarnings: Boolean
}

case class DefaultReporter(messages: CompilerMessages) extends Reporter {

  def report(error: CompilerMessage): Unit = {
    messages += error

    if (error.messageType == MessageType.Fatal)
      throwException()
  }

  def clear(): Unit = messages.clear()

  def terminateIfErrors(): Unit =
    if (hasErrors)
      throwException()

  def hasErrors: Boolean = messages(MessageType.Error).nonEmpty
  def hasWarnings: Boolean = messages(MessageType.Warning).nonEmpty

  override def printWarnings(): Unit = messages.print(MessageType.Warning)
  override def printErrors(): Unit = messages.print(MessageType.Error)
  override def getWarnings: List[CompilerMessage] = messages(MessageType.Warning)
  override def getErrors: List[CompilerMessage] = messages(MessageType.Error)

  private def throwException() = throw new CompilationException(messages)

}

case class VoidReporter() extends Reporter {

  private var _hasErrors   = false
  private var _hasWarnings = false

  override def printWarnings(): Unit = {}
  override def printErrors(): Unit = {}
  override def getWarnings: List[CompilerMessage] = Nil
  override def getErrors: List[CompilerMessage] = Nil

  override def report(message: CompilerMessage): Unit = message match {
    case _: FatalMessage   => throw new CompilationException(null)
    case _: WarningMessage => _hasWarnings = true
    case _                 => _hasErrors = true
  }

  override def clear(): Unit = {
    _hasErrors = false
    _hasWarnings = false
  }
  override def terminateIfErrors(): Unit = {}

  override def hasErrors: Boolean = _hasErrors
  override def hasWarnings: Boolean = _hasWarnings

}