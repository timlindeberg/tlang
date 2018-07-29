package tlang
package compiler
package output

import tlang.utils.JSON

import scala.collection.mutable

trait OutputHandler {

  def add(output: Output): Unit
  def +=(output: Output): Unit = add(output)

  def flush(): Unit

}

case class PrettyOutputHandler() extends OutputHandler {
  override def add(output: Output): Unit = {
    val s = output.pretty
    if(s.nonEmpty)
      println(s)
  }
  override def flush(): Unit = {}
}

case class JSONOutputHandler() extends OutputHandler {

  private val content: mutable.Map[Any, Any] = mutable.Map()

  override def add(output: Output): Unit = content ++= output.json
  override def flush(): Unit = println(JSON(content))

}
