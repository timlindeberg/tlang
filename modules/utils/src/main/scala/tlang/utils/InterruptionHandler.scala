package tlang
package utils

import sun.misc.Signal

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object InterruptionHandler {
  case class Category(name: String, priority: Int = 0)

  val ExitCategory = Category("Exit", Int.MaxValue)
}

case class InterruptionHandler() {

  import InterruptionHandler._

  type InterruptHandler = () => Unit
  type HandlerId = Int

  private val handlerCategories = mutable.Map[Category, ListBuffer[InterruptHandler]]()

  Signal.handle(new Signal("INT"), (_: Signal) => onInterrupt())

  def setHandler(handlerCategory: Category, handler: InterruptHandler): HandlerId = {
    val handlers = handlerCategories.getOrElseUpdate(handlerCategory, ListBuffer())
    handlers += handler
    handlers.size - 1
  }

  def removeHandler(handlerCategory: Category, id: HandlerId): Unit = {
    handlerCategories.get(handlerCategory) ifDefined { _.remove(id, 1) }
  }

  private def onInterrupt(): Unit = {
    handlerCategories
      .toList
      .sortBy { _._1.priority }
      .map { _._2 }
      .foreach { handlers => handlers.last() }
  }
}
