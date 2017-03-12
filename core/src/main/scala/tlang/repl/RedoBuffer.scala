package tlang.repl

import scalaz.Cord

/**
  * Created by Tim Lindeberg on 2/26/2017.
  */

case object State {
  val Empty = State(Cord.empty, List(0), 0)
}
case class State(cord: Cord, linePositions: List[Int], position: Int)
case class RedoBuffer(maxSize: Int) {

  private var _first: Option[HistoryNode] = None
  private var _last : Option[HistoryNode] = None
  private var _size : Int                 = 0


  def current: State = if (_last.isEmpty) State.Empty else _last.get.elem

  def size: Int = _size
  def isEmpty: Boolean = _size == 0
  def nonEmpty: Boolean = _size != 0

  def +=(state: State): Unit = add(state)

  def clear(): Unit = {
    _first = None
    _last = None
    _size = 0
  }

  def undo(): Boolean = {
    if (isEmpty || _last.get.prev.isEmpty)
      return false

    _last = _last.get.prev
    true
  }

  def redo(): Boolean = {
    if (isEmpty || _last.get.next.isEmpty)
      return false

    _last = _last.get.next
    true
  }

  def removeFirst(): Unit = {
    if (_first.isEmpty)
      return

    _size -= 1
    if (_first.get.next.isEmpty) {
      _first = None
      _last = None
      return
    }

    _first.get.next.get.prev = None
    _first = _first.get.next
  }

  def removeLast(): Unit = {
    if (_last.isEmpty)
      return

    _size -= 1
    if (_last.get.prev.isEmpty) {
      _first = None
      _last = None
      return
    }
    _last.get.prev.get.next = None
    _last = _last.get.prev
  }

  override def toString: String = {
    if (isEmpty)
      return "Empty"

    val sb = new StringBuilder
    var node = _first
    var first = true
    while (node.isDefined) {
      val n = node.get
      if (!first)
        sb ++= " -> "
      first = false

      val text = '"' + n.elem.cord.toString.replaceAll("\n", "\\\\n") + '"'
      val pos = n.elem.position
      var s = s"($text, $pos)"
      if (n eq _last.get) {
        sb ++= Console.RED + Console.UNDERLINED
        sb ++= s
        sb ++= Console.RESET
      } else {
        sb ++= s
      }
      node = n.next
    }
    sb.toString()
  }

  private def add(t: State): Unit = {
    if (_size >= maxSize)
      removeFirst()

    val node = Some(new HistoryNode(t, None, None))

    _size += 1
    if (_first.isEmpty) {
      _first = node
      _last = node
      return
    }

    node.get.prev = _last
    _last.get.next = node
    _last = node
  }

  private class HistoryNode(var elem: State, var prev: Option[HistoryNode], var next: Option[HistoryNode])

}

