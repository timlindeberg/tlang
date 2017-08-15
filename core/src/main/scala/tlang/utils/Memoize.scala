package tlang.utils


object Memoize {

  def apply[R](f: => R): Memoized[R] = new Memoized(f)

}

class Memoized[R](f: => R) {

  var value: Option[R] = None

  def apply(): R = {
    if (value.isEmpty)
      value = Some(f)

    value.get
  }

  def reset(): Unit = value = None

}
