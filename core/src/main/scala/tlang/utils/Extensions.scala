package tlang.utils

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future, TimeoutException}
import scala.reflect.{ClassTag, _}
import scala.util.matching.Regex

object Extensions {

  private val AnsiRegex: Regex = """\x1b[^m]*m""".r

  def using[T <: {def close()}, R](resource: T)(block: T => R): R = {
    try {
      block(resource)
    } finally {
      if (resource != null) resource.close()
    }
  }

  def withTimeout[T](duration: Duration)(block: => T): T = {
    if (duration.toNanos == 0)
      return block

    implicit val exec = new CancelableExecutionContext()
    try {
      Await.result(Future(block), duration)
    } catch {
      case e@(_: TimeoutException | _: InterruptedException) =>
        exec.cancel()
        throw e
    }
  }

  def measureTime[T](block: => T): (T, Double) = {
    val t0 = System.nanoTime()
    val res = block
    val t1 = System.nanoTime()
    (res, (t1 - t0) / 1000000000.0)
  }

  def benchmark[T](name: String, iterations: Int = 10)(block: => T): T = {
    // Warmup
    Math.round(iterations * 0.1) times {
      block
    }

    var time = 0.0
    iterations times { time += measureTime(block)._2 }

    time /= iterations
    println(f"Average time for $name: $time%.5f s")
    block
  }

  implicit class OptionExtensions[T](val o: Option[T]) extends AnyVal {
    def ifDefined(f: T => Unit): Unit = if (o.isDefined) f(o.get)
  }

  implicit class IntExtensions(val i: Int) extends AnyVal {
    def times[U](f: => U): Unit = (1 to i) foreach { _ => f }
  }

  implicit class LongExtensions(val i: Long) extends AnyVal {
    def times[U](f: => U): Unit = (1 to i.toInt) foreach { _ => f }
  }

  implicit class RegexExtensions(r: Regex) {
    def matches(s: String): Boolean = r.pattern.matcher(s).matches
  }

  implicit class StringExtensions(val str: String) extends AnyVal {
    def containsAnsi: Boolean = AnsiRegex.matches(str)
    def stripAnsi: String = AnsiRegex.replaceAllIn(str, "")
    def visibleCharacters: Int = {
      val str = stripAnsi
      str.codePointCount(0, str.length)
    }

    def ansiDebugString: String = {
      str
        .map {
          case '\u001b' => "▯"
          case '\n'     => "\\n"
          case '\r'     => "\\r"
          case '\t'     => "\\t"
          case c        => s"$c"
        }
        .map("'" + _ + "'")
        .mkString(" ")
    }

    def trimWhiteSpaces: String = str.leftTrimWhiteSpaces.rightTrimWhiteSpaces
    def leftTrimWhiteSpaces: String = {
      val s = str.replaceAll("^\\s+", "")
      if (s.length < 2 || s(0) != '\u001b' || s(1) == '[')
        return s

      val ansiStop = s.indexOf('m')
      if (ansiStop + 1 >= s.length || !s(ansiStop + 1).isWhitespace)
        return s

      val ansi = s.substring(0, ansiStop + 1)
      val trimmed = s.substring(ansiStop + 2, s.length)
      ansi + trimmed
    }

    def rightTrimWhiteSpaces: String = {
      // TODO: This should trim at the end even if the string ends with ANSI-colors
      str.replaceAll("\\s+$", "")
    }

    def allIndexesOf(pattern: String): List[Int] = {
      val buf = ListBuffer[Int]()

      var index = str.indexOf(pattern)
      if (index != -1)
        buf += index
      while (index >= 0) {
        index = str.indexOf(pattern, index + 1)
        if (index != -1)
          buf += index
      }
      buf.toList
    }

  }

  implicit class StringBuilderExtensions(val sb: scala.collection.mutable.StringBuilder) extends AnyVal {

    def appendTimes(char: Char, times: Int): mutable.StringBuilder = {
      var i = 0
      while (i < times) {
        sb.append(char)
        i += 1
      }
      sb
    }
  }

  implicit class AnyExtensions(val a: Any) extends AnyVal {
    def ifInstanceOf[T: ClassTag](f: T => Unit): Unit = if (classTag[T].runtimeClass.isInstance(a)) f(a.asInstanceOf[T])
  }

  implicit class GenericExtensions[T](val t: T) extends AnyVal {
    def use(f: T => Unit): T = { val x = t; f(t); x }
    def print: T = { println(t); t }
    def print[U](f: T => U): T = { println(f(t)); t }
    def in(seq: TraversableOnce[T]): Boolean = seq.exists(_ == t)
    def notIn(seq: TraversableOnce[T]): Boolean = !t.in(seq)
    def in(set: Set[T]): Boolean = set.contains(t)
    def notIn(set: Set[T]): Boolean = !t.in(set)
    def in(range: Range): Boolean = range.contains(t)
    def notIn(range: Range): Boolean = !t.in(range)
  }

  implicit class TraversableExtensions[Collection[T] <: Traversable[T], T](val collection: Collection[T]) extends AnyVal {
    def filterInstance[A <: T : ClassTag]: Collection[A] = collection.filter(classTag[A].runtimeClass.isInstance(_)).asInstanceOf[Collection[A]]
    def filterNotInstance[A <: T : ClassTag]: Collection[T] = collection.filter(!classTag[A].runtimeClass.isInstance(_)).asInstanceOf[Collection[T]]
    def findInstance[A <: T : ClassTag]: Option[A] = collection.find(classTag[A].runtimeClass.isInstance(_)).asInstanceOf[Option[A]]
    def findDefined[A](f: T => Option[A]): Option[A] = {
      for (v <- collection) {
        val o = f(v)
        if (o.isDefined) return o
      }
      None
    }
    def partitionInstance[A <: T : ClassTag]: (Collection[A], Collection[T]) = {
      val (a, b) = collection.partition(classTag[A].runtimeClass.isInstance(_))
      (a.asInstanceOf[Collection[A]], b.asInstanceOf[Collection[T]])
    }

    def remove(t: T): Collection[T] = collection.filter(_ != t).asInstanceOf[Collection[T]]
  }

  implicit class MutableMapExtensions[K, V](val m: mutable.Map[K, V]) extends AnyVal {
    def getOrElseMaybeUpdate(key: K)(op: => Option[V]): Option[V] =
      m.get(key) match {
        case Some(v) => Some(v)
        case None    => op match {
          case Some(v) =>
            m += key -> v
            Some(v)
          case None    => None
        }
      }
  }

  class CancelableExecutionContext extends AnyRef with ExecutionContext {

    @volatile var lastThread: Option[Thread] = None
    override def execute(runnable: Runnable): Unit = {
      ExecutionContext.Implicits.global.execute(() => {
        lastThread = Some(Thread.currentThread)
        runnable.run()
      })
    }

    def cancel(): Unit = lastThread ifDefined { _.stop }

    override def reportFailure(t: Throwable): Unit = ???
  }

}
