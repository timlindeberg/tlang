package tlang.utils

import tlang.utils.formatting.{ColoredString, Formatting}

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

  def benchmark[T](name: String)(block: => T): T = {
    val iterations = 10
    (iterations - 1) times {
      block
    }

    var res: T = block
    val times = (0 until iterations).map { _ =>
      res = block
      measureTime(block)._2
    }.toList
    println(s"Results for $name:")
    println(times.map(t => f"   $t%.5f s").mkString("\n"))
    println(f"Average time: ${ times.sum / iterations }%.5f s")
    res
  }

  implicit class OptionExtensions[T](val o: Option[T]) extends AnyVal {
    def ifDefined(f: T => Unit): Unit = if (o.isDefined) f(o.get)
  }

  implicit class IntExtensions(val i: Int) extends AnyVal {
    def times(f: => Unit): Unit = 1 to i foreach { _ => f }
  }

  implicit class RegexExtensions(r: Regex) {
    def matches(s: String): Boolean = r.pattern.matcher(s).matches
  }

  implicit class StringExtensions(val str: String) extends AnyVal {
    def isAnsi: Boolean = AnsiRegex.matches(str)
    def clearAnsi: String = AnsiRegex.replaceAllIn(str, "")
    def charCount: Int = {
      val str = clearAnsi
      str.codePointCount(0, str.length)
    }

    def takeChars(num: Int): String = {
      var i = 0
      var s = ""
      while (s.charCount < num) {
        s += str(i)
        i += 1
      }
      s
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

    def toColoredString(formatting: Formatting) = ColoredString(formatting, str)

    def removeSuffix(suffix: String) = str.substring(0, str.length - suffix.length)
  }

  implicit class AnyExtensions(val a: Any) extends AnyVal {
    def ifInstanceOf[T: ClassTag](f: T => Unit): Unit = if (classTag[T].runtimeClass.isInstance(a)) f(a.asInstanceOf[T])
  }

  implicit class GenericExtensions[T](val t: T) extends AnyVal {
    def use(f: T => Unit): T = { val x = t; f(t); x }
    def print: T = { println(t); t }
    def print(prefix: String): T = { println(prefix + ": " + t); t }
    def in(seq: TraversableOnce[T]): Boolean = seq.exists(_ == t)
    def in(set: Set[T]): Boolean = set.contains(t)
    def in(range: Range): Boolean = range.contains(t)
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
