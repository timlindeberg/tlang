package tlang
package compiler
package messages

import tlang.options.argument.MaxErrorsFlag

import scala.collection.mutable

object CompilerMessages {
  def createMessages(): mutable.Map[MessageType, mutable.Set[CompilerMessage]] = mutable.Map(
    MessageType.Error -> mutable.LinkedHashSet[CompilerMessage](),
    MessageType.Warning -> mutable.LinkedHashSet[CompilerMessage]()
  )
}

case class CompilerMessages(
  maxErrors: Int = MaxErrorsFlag.defaultValue,
  warningIsError: Boolean = false,
  suppressWarnings: Boolean = false,
  private var messages: mutable.Map[MessageType, mutable.Set[CompilerMessage]] = CompilerMessages.createMessages()) {

  private val _hitMax = mutable.Set[MessageType]()

  override def clone(): CompilerMessages = copy(messages = mutable.Map() ++ messages.toMap)

  def ++=(messages: Traversable[CompilerMessage]): Unit = messages foreach { this += _ }

  def +=(message: CompilerMessage): CompilerMessages = {
    if (!message.isValid)
      return this

    var messageType = message.messageType
    if (messageType == MessageType.Warning) {
      if (suppressWarnings)
        return this

      // Copy the existing values such as code type etc., only change should be the message type
      if (warningIsError)
        return this += message.copy(messageType = MessageType.Error)
    }

    if (maxErrors >= 0 && messages(messageType).size >= maxErrors) {
      _hitMax += messageType
      return this
    }
    messages(messageType) += message
    this
  }

  def apply(messageType: MessageType): List[CompilerMessage] = messages(messageType).toList

  def clear(): Unit = {
    messages = CompilerMessages.createMessages()
    _hitMax.clear()
  }

  def hitMax(tpe: MessageType): Boolean = _hitMax(tpe)
}
