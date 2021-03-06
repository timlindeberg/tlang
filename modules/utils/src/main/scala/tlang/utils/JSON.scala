package tlang
package utils

import scala.collection.{Map, mutable}

object JSON {

  type Json = mutable.LinkedHashMap[String, Any]

  def Json(entries: (String, Any)*): Json = mutable.LinkedHashMap[String, Any](entries: _*)

  private val JSONEscapeChars = Map(
    '"' -> "\"",
    '\\' -> "\\",
    '\b' -> "b",
    '\f' -> "f",
    '\n' -> "n",
    '\r' -> "r",
    '\t' -> "t"
  )

  def apply(any: Any): String = {
    val sb = new StringBuilder()

    def json(any: Any): Unit = any match {
      case null | None           => sb ++= "null"
      case Some(a)               => json(a)
      case p if isPrimitive(any) => jsonPrimitive(p)
      case c: Map[_, _]          => jsonObject(c)
      case c: Array[_]           => jsonList(c)
      case c: Traversable[_]     => jsonList(c)
      case _                     => jsonString(any)
    }

    def isPrimitive(any: Any): Boolean = any matches {
      case _: Int | _: Long | _: Float | _: Double | _: Byte | _: Short | _: Boolean => true
    }

    def jsonObject(obj: Map[_, Any]): Unit = jsonContainer(obj, '{', '}') { case (key, value) =>
      jsonString(key)
      sb += ':'
      json(value)
    }

    def jsonList(list: Traversable[Any]): Unit = jsonContainer(list, '[', ']')(json)

    def jsonContainer[T](elements: Traversable[T], prefix: Char, postfix: Char)(convert: T => Unit): Unit = {
      sb += prefix
      var first = true
      elements.foreach { v =>
        if (!first) sb += ','
        convert(v)
        first = false
      }
      sb += postfix
    }

    def jsonString(s: Any): Unit = {
      sb += '"'
      sb ++= s.toString.escape(JSONEscapeChars)
      sb += '"'
    }

    def jsonPrimitive(any: Any): Unit = sb ++= any.toString

    json(any)
    sb.toString
  }
}
