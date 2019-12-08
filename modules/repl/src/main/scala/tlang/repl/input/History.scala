package tlang
package repl
package input

import tlang.formatting.Colors


object History {

  def apply[T](): History[T] = History[T](Int.MaxValue)
  def apply[T](maxSize: Int, initial: T): History[T] = History[T](maxSize) use { _ += initial }
  def apply[T](maxSize: Int, initial: Traversable[T]): History[T] = History[T](maxSize) use { _ ++= initial }

}

case class History[T](maxSize: Int) extends Seq[T] {

  private case class Node(var elem: T, var prev: Option[Node], var next: Option[Node])

  private var _first: Option[Node] = None
  private var _last: Option[Node] = None
  private var _size: Int = 0

  def apply(idx: Int): T = iterator.drop(idx).next()

  def current: Option[T] = _last.map(_.elem)
  def replaceCurrent(elem: T): this.type = {
    _last match {
      case Some(node) => node.elem = elem
      case None       => this += elem
    }
    this
  }

  override def length: Int = _size
  override def isEmpty: Boolean = _size == 0

  override def iterator: Iterator[T] = new Iterator[T] {
    private var current: Option[Node] = _first

    override def hasNext: Boolean = current.isDefined
    override def next: T = current.get.use(node => current = node.next).elem
  }

  def ++=(elems: Traversable[T]): this.type = { elems foreach { this += _ }; this }
  def +=(elem: T): this.type = {
    val atLastPos = _last.flatMap(_.next).isEmpty
    if (_size >= maxSize && atLastPos)
      removeFirst()

    if (atLastPos) {
      _size += 1
    } else {
      // We're not at the end when adding a new element. This removes the
      // rest of the elements so we have to recalculate the size
      _last.get.next = None
      _size = iterator.size + 1
    }

    val node = Some(Node(elem, None, None))
    if (_first.isEmpty) {
      _first = node
      _last = node
      return this
    }

    node.get.prev = _last
    _last.get.next = node
    _last = node
    this
  }

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

  private def removeFirst(): Unit = {
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

  override def toString: String = {
    if (isEmpty)
      return "Empty"

    val highlight = Colors.Red + Colors.Underline
    this
      .map { case node@Node(elem, _, _) =>
        val s = elem.toString
        if (node eq _last.get) highlight(s) else s
      }
      .mkString(" -> ")
  }

}

