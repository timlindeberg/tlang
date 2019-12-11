package tlang
package compiler

import sun.misc.Signal

import scala.collection.mutable.ListBuffer

case class InterruptionHandler() {

  type InterruptHandler = () => Unit
  type HandlerId = Int

  private val handlers = ListBuffer[InterruptHandler]()

  Signal.handle(new Signal("INT"), (_: Signal) => handlers.last())

  def setHandler(handler: InterruptHandler): HandlerId = {
    handlers += handler
    handlers.size - 1
  }

  def removeHandler(id: HandlerId): Unit = handlers.remove(id, 1)
}
