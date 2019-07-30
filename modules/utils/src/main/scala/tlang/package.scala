import java.io.{File, PrintWriter, StringWriter}
import java.nio.file.{Path, Paths}

import scala.collection.mutable
import scala.reflect.{ClassTag, classTag}
import scala.util.Try
import scala.util.matching.Regex

package object tlang {

  private val AnsiRegex: Regex = """\x1b[^m]*m""".r

  def debugPrint(values: sourcecode.Text[_]*): Unit = {
    val maxNameWidth = values.map(_.source.length).max
    values foreach {
      value => printf(s"[%-${ maxNameWidth }s]: '%s'$NL", value.source, value.value.toString)
    }
  }

  val NL: String = System.lineSeparator

  val EscapeCharsNormal: scala.collection.Map[Char, String] =
    Map('\t' -> "t", '\b' -> "b", '\n' -> "n", '\r' -> "r", '\f' -> "f", '\\' -> "\\", '\'' -> "'", '"' -> "\"")

  implicit val EscapeCharsAll: scala.collection.Map[Char, String] =
    EscapeCharsNormal + ('\u001b' -> "\\u001b")

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

  implicit class ClassExtensions[T](val o: Class[T]) extends AnyVal {
    def objectName: String = o.getName.stripSuffix("$")
    def simpleObjectName: String = o.getSimpleName.stripSuffix("$")
  }

  implicit class OptionExtensions[T](val o: Option[T]) extends AnyVal {
    def ifDefined(f: T => Unit): Unit = if (o.isDefined) f(o.get)
  }

  implicit class IntExtensions(val i: Int) extends AnyVal {
    def times[U](f: => U): Unit = (1 to i) foreach { _ => f }
    def clamp(min: Int, max: Int): Int = Math.min(Math.max(i, min), max)
  }

  implicit class LongExtensions(val i: Long) extends AnyVal {
    def times[U](f: => U): Unit = (1 to i.toInt) foreach { _ => f }
  }

  implicit class RegexExtensions(r: Regex) {
    def matches(s: String): Boolean = r.pattern.matcher(s).matches
  }

  implicit class ThrowableExtensions(t: Throwable) {
    def stackTrace: String = {
      val sw = new StringWriter()
      val pw = new PrintWriter(sw)
      t.printStackTrace(pw)
      sw.toString
    }
  }

  implicit class PathExtensions(path: Path) {
    def relativePWD: Path = {
      val absolute = path.toAbsolutePath
      if (absolute.startsWith(Constants.Pwd))
        Constants.Pwd.relativize(absolute)
      else
        absolute
    }
  }


  implicit class StringExtensions(val str: String) extends AnyVal {
    def isNumber: Boolean = Try(str.toInt).isSuccess

    def escapeAnsi: String = str.escape(Map('\u001b' -> "u001b"))

    def escapeMargin: String = str.replaceAll("\n\\|", "\n||")

    def withUnixLineEndings: String = {
      if (NL == "\n")
        return str

      str.replaceAll("\r\n", "\n")
    }

    def withSystemLineEndings: String = {
      if (NL == "\n")
        return str

      val sb = new StringBuilder()
      for (i <- str.indices) {
        if (str(i) == '\n' && (i == 0 || str(i - 1) != '\r'))
          sb ++= NL
        else
          sb += str(i)
      }
      sb.toString
    }

    def insert(s: String, i: Int): String = str.substring(0, i) + s + str.substring(i, str.length)

    def escape(implicit escapeCharacters: scala.collection.Map[Char, String]): String = {
      val sb = new StringBuilder
      str foreach { c =>
        escapeCharacters.get(c) match {
          case Some(x) => sb ++= s"\\$x"
          case None    => sb += c
        }
      }
      sb.toString
    }

    def containsAnsi: Boolean = AnsiRegex.matches(str)
    def stripAnsi: String = AnsiRegex.replaceAllIn(str, "")
    def stripNewlines: String = str.replaceAll("\r?\n", "")
    def visibleCharacters: Int = {
      if (str.isEmpty)
        return 0
      val s = stripAnsi
      s.codePointCount(0, s.length)
    }

    def isValidPath: Boolean = {
      if (!Try(Paths.get(str)).isSuccess)
        return false

      new File(str).exists
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

  }

  implicit class Tuple2Extensions[T](val t: (T, T)) extends AnyVal {
    def map[A](f: T => A): (A, A) = (f(t._1), f(t._2))
  }
  implicit class Tuple3Extensions[T](val t: (T, T, T)) extends AnyVal {
    def map[A](f: T => A): (A, A, A) = (f(t._1), f(t._2), f(t._3))
  }
  implicit class Tuple4Extensions[T](val t: (T, T, T, T)) extends AnyVal {
    def map[A](f: T => A): (A, A, A, A) = (f(t._1), f(t._2), f(t._3), f(t._4))
  }

  implicit class TupleCollectionExtensions[A, B](val tuples: Traversable[(A, B)]) extends AnyVal {
    def aligned: String = {
      if (tuples.isEmpty) return ""

      val maxWidth = tuples.map(_._1.toString.length).max
      tuples
        .map { case (a, b) => java.lang.String.format(s"%-${ maxWidth }s -> %s", a.toString, b.toString) }
        .mkString(NL)
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

  implicit class EnclosingExtensions(val enclosing: sourcecode.Enclosing) extends AnyVal {

    def method: String = enclosing.value.split("#")(1)

  }

  implicit class GenericExtensions[T](val t: T) extends AnyVal {

    def use(f: T => Unit): T = { val x = t; f(t); x }
    def <|(f: => Unit): T = { val x = t; f; x }

    def print: T = { println(t); t }
    def print[U](f: T => U): T = { println(f(t)); t }
    def print[U](prefix: String): T = { println(s"$prefix: '$t'"); t }

    def in[P >: T](seq: TraversableOnce[P]): Boolean = seq.exists(_ == t)
    def notIn[P >: T](seq: TraversableOnce[P]): Boolean = !t.in(seq)
    def in[P >: T](set: Set[P]): Boolean = set.contains(t)
    def notIn[P >: T](set: Set[P]): Boolean = !t.in(set)
    def in(range: Range): Boolean = range.contains(t)
    def notIn(range: Range): Boolean = !t.in(range)

    def ifInstanceOf[A: ClassTag](f: A => Unit): Unit = if (classTag[A].runtimeClass.isInstance(t)) f(t.asInstanceOf[A])
    def partialMatch[U](partialFunction: PartialFunction[T, U]): Unit = {
      if (partialFunction.isDefinedAt(t))
        partialFunction.apply(t)
    }

    def matches(p: PartialFunction[T, Boolean]): Boolean = p.isDefinedAt(t)

  }

  implicit class TraversableExtensions[Collection[T] <: Traversable[T], T](val collection: Collection[T]) extends AnyVal {

    def filterInstance[A <: T : ClassTag]: Collection[A] = collection.filter(classTag[A].runtimeClass.isInstance(_)).asInstanceOf[Collection[A]]
    def filterNotInstance[A <: T : ClassTag]: Collection[T] = collection.filter(!classTag[A].runtimeClass.isInstance(_)).asInstanceOf[Collection[T]]

    def findInstance[A <: T : ClassTag]: Option[A] = collection.find(classTag[A].runtimeClass.isInstance(_)).asInstanceOf[Option[A]]
    def existsInstance[A <: T : ClassTag]: Boolean = collection.exists(classTag[A].runtimeClass.isInstance(_))
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

    def partitionInstances[A <: T : ClassTag, B <: T]: (Collection[A], Collection[B]) = {
      val (a, b) = collection.partitionInstance[A]
      (a, b.asInstanceOf[Collection[B]])
    }

    def remove(t: T): Collection[T] = collection.filter(_ != t).asInstanceOf[Collection[T]]

  }

  implicit class SeqExtensions[T](val seq: Seq[T]) extends AnyVal {

    def cut(n: Int): Vector[Seq[T]] = {
      val m = seq.length
      val targets = (0 to n).map { x => math.round((x.toDouble * m) / n).toInt }

      def snip(xs: Seq[T], ns: Seq[Int], got: Vector[Seq[T]]): Vector[Seq[T]] = {
        if (ns.lengthCompare(2) < 0) got
        else {
          val (i, j) = (ns.head, ns.tail.head)
          snip(xs.drop(j - i), ns.tail, got :+ xs.take(j - i))
        }
      }

      snip(seq, targets, Vector.empty)
    }

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

  implicit class ArrayExtensions[T](val arr: Array[T]) extends AnyVal {
    def binarySearch(key: T): Int = java.util.Arrays.binarySearch(arr.asInstanceOf[Array[AnyRef]], key)
  }

}
