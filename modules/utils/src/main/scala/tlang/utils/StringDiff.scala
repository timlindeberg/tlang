package tlang
package utils

import tlang.formatting.{Colors, Formatter}

// Code is copied from https://github.com/bitwalker/scaladiff since
// the repository hasn't been updated for Scala 2.12. It has been slightly
// modified (some clean up and added colored method)

object StringDiff {

  def apply(a: String, b: String): String = Diff.create(a, b).colored
  def apply(a: String, b: String)(implicit formatter: Formatter): String = {
    val diff = Diff.create(a, b)
    if (formatter.useColor) diff.colored else diff.humanized
  }

  trait OperationType
  object OperationType {
    case object Insert extends OperationType
    case object Delete extends OperationType
    case object Equals extends OperationType
  }

  case class Operation(op: OperationType, text: String) {
    override def toString: String = {
      import OperationType._
      op match {
        case Insert => s"+$text"
        case Delete => s"-$text"
        case Equals => text
      }
    }
  }

  case class Diff(original: String, modified: String, diffs: List[Operation]) {

    import OperationType._

    /**
      * The unaltered diff result
      */
    override def toString: String = diffs.mkString

    /**
      * Create a nice HTML report of the diff
      */
    def html: String = {
      Diff.clean(diffs).foldLeft("") { (html, diff) =>
        val text = diff.text.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;").replace("\n", "<br>")
        val tag = diff.op match {
          case Insert => s"<ins>$text</ins>"
          case Delete => s"<del>$text</del>"
          case Equals => s"<span>$text</span>"
        }
        html + tag
      }
    }

    /**
      * Convert the diff into a more human-readable format
      */
    def humanized: String = {
      Diff.clean(diffs).foldLeft("") { (res, diff) =>
        val text = diff.op match {
          case Insert => s"+[${ diff.text }]"
          case Delete => s"-[${ diff.text }]"
          case Equals => diff.text
        }
        res + text
      }
    }

    def colored: String = {
      import Colors._
      Diff.clean(diffs).foldLeft("") { (res, diff) =>
        val text = diff.op match {
          case Insert => (Green + Underline) (diff.text)
          case Delete => (Red + Underline) (diff.text)
          case Equals => diff.text
        }
        res + text
      }
    }
  }

  object Diff {

    import OperationType._

    // Cost of an empty edit operation in terms of edit characters
    private val MAX_EDIT_COST = 4

    /**
      * Creates a new Diff
      *
      * @param a First string
      * @param b Second string
      * @return Diff
      */
    def create(a: String, b: String): Diff = {
      import scala.util.control.Breaks._

      var (original: String, modified: String) = (a, b)
      var subsequence: String = lcs(original, modified)
      var result = List.empty[Operation]

      while (subsequence.length > 0) {
        val sFirst = subsequence(0)
        subsequence = subsequence.drop(1)

        breakable {
          while (modified.length > 0) {
            val mFirst = modified(0)
            modified = modified.drop(1)
            if (mFirst == sFirst) break

            result = result :+ Operation(Insert, mFirst.toString)
          }
        }
        breakable {
          while (original.length > 0) {
            val oFirst = original(0)
            original = original.drop(1)
            if (oFirst == sFirst) break
            result = result :+ Operation(Delete, oFirst.toString)
          }
        }
        result = result :+ Operation(Equals, sFirst.toString)
      }
      while (modified.length > 0) {
        val mFirst = modified(0)
        modified = modified.drop(1)
        result = result :+ Operation(Insert, mFirst.toString)
      }
      while (original.length > 0) {
        val oFirst = original(0)
        original = original.drop(1)
        result = result :+ Operation(Delete, oFirst.toString)
      }

      Diff(a, b, result)
    }

    /**
      * Eliminate operationally trivial equalities, then reorder and merge both edits and equalities.
      *
      * @param diffs List[Operation]
      */
    def clean(diffs: List[Operation]): List[Operation] = {
      var buffer = diffs
      var changes = false
      var equalities = List.empty[Int] // Stack of indices where equalities are found
      var lastEquality = "" // Always equal to equalities.last.text
      var currentIndex = 0
      var preInsert = false // Is there an insert op before the last equality
      var preDelete = false // Is there a delete op before the last equality
      var postInsert = false // Is there an insert op after the last equality
      var postDelete = false // Is there a delete op after the last equality

      while (currentIndex < buffer.length) {
        if (buffer(currentIndex).op == Equals) {
          if (buffer(currentIndex).text.length < MAX_EDIT_COST && (postInsert || postDelete)) {
            // Candidate found
            equalities = equalities :+ currentIndex
            preInsert = postInsert
            preDelete = postDelete
            lastEquality = buffer(currentIndex).text
          } else {
            // Not a candidate
            equalities = List.empty[Int]
            lastEquality = ""
          }
          postInsert = false
          postDelete = false
        } else { // An insertion or deletion
          buffer(currentIndex).op match {
            case Delete => postDelete = true
            case Insert => postInsert = true
          }

          /**
            * Five types to be split:
            * +A-BXY+C-D
            * +AX+C-D
            * +A-BX+C
            * +AX+C-D
            * +A-BX-C
            */
          val bools = Seq(preInsert, preDelete, postInsert, postDelete)
          if (!lastEquality.isEmpty && (bools.forall(b => b) || ((lastEquality.length < MAX_EDIT_COST / 2) && bools.count(b => b) == 3))) {
            // Duplicate record
            // Change second copy to insert
            buffer = replace(buffer, equalities.last, 1)(Operation(Delete, lastEquality), Operation(Insert, lastEquality))
            equalities = equalities.dropRight(1)
            lastEquality = ""
            if (preInsert && preDelete) {
              // No changes made which could affect previous entry, keep going
              postInsert = true
              postDelete = true
              equalities = List.empty[Int]
            } else {
              if (equalities.nonEmpty) {
                equalities = equalities.dropRight(1) // Throw away previous equality
                currentIndex = equalities.lastOption.getOrElse(-1)
              }
              postInsert = false
              postDelete = false
            }
            changes = true
          }
        }
        currentIndex += 1
      }

      cleanMerge(buffer)
    }

    /**
      * Reorder and merge edits, equalities. Any edit section can move as long as it doesn't cross an equality.
      *
      * @param diffs List of Operations
      */
    private def cleanMerge(diffs: List[Operation]): List[Operation] = {
      var buffer = diffs :+ Operation(Equals, "")
      var deletes = 0
      var inserts = 0
      var deleted = ""
      var inserted = ""
      var currentIndex = 0

      while (currentIndex < buffer.length) {
        buffer(currentIndex).op match {
          case Insert =>
            inserts += 1
            inserted += buffer(currentIndex).text
            currentIndex += 1
          case Delete =>
            deletes += 1
            deleted += buffer(currentIndex).text
            currentIndex += 1
          case Equals =>
            // Upon reaching an equality, check for prior redundancies
            if (deletes + inserts > 1) {
              if (deletes != 0 && inserts != 0) {
                // Factor out any common prefixes
                val prefixLength = commonPrefix(inserted, deleted)
                if (prefixLength != 0) {
                  val idx = currentIndex - deletes - inserts
                  if (idx > 0 && buffer(idx - 1).op == Equals) {
                    var op = buffer(idx - 1)
                    op = op.copy(op.op, op.text + sliceLeft(inserted, prefixLength))
                    buffer = insert(buffer, idx)(op)
                  } else {
                    buffer = Operation(Equals, sliceLeft(inserted, prefixLength)) +: buffer
                    currentIndex += 1
                  }
                  inserted = inserted.substring(prefixLength, inserted.length - 1)
                  deleted = deleted.substring(prefixLength, deleted.length - 1)
                }
                // Factor out any common suffixes
                val suffixLength = commonSuffix(inserted, deleted)
                if (suffixLength != 0) {
                  var op = buffer(currentIndex)
                  op = op.copy(op.op, sliceRight(inserted, suffixLength) + op.text)
                  buffer = insert(buffer, currentIndex)(op)
                }
              }

              // Delete the offending records and add the merged ones
              if (deletes == 0) {
                val start = currentIndex - deletes - inserts
                buffer = replace(buffer, start, deletes + inserts)(Operation(Insert, inserted))
              } else if (inserts == 0) {
                val start = currentIndex - deletes - inserts
                buffer = replace(buffer, start, deletes + inserts)(Operation(Delete, deleted))
              } else {
                val start = currentIndex - deletes - inserts
                buffer = replace(buffer, start, deletes + inserts)(Operation(Delete, deleted), Operation(Insert, inserted))
              }

              currentIndex = (currentIndex - deletes - inserts) + (if (deletes == 0) 0 else 1) + (if (inserts == 0) 0 else 1) + 1
            } else if (currentIndex != 0 && buffer(currentIndex - 1).op == Equals) {
              // Merge this equality with the previous one
              val previous = buffer(currentIndex - 1)
              val current = buffer(currentIndex)
              buffer = replace(buffer, currentIndex - 1, 2)(previous.copy(previous.op, previous.text + current.text))
              currentIndex -= 1
            } else {
              currentIndex += 1
            }

            inserts = 0
            deletes = 0
            inserted = ""
            deleted = ""
        }
      }

      if (buffer.last.text == "") {
        buffer = buffer.dropRight(1) // Remove the dummy entry at the end
      }

      // Second pass: look for single edits surrounded on both sides by equalities
      // which can be shifted sideways to eliminate an equality
      // e.g. A<ins>BA</ins>C -> <ins>AB</ins>AC
      var changes = false
      currentIndex = 1

      // Intentionally ignore the first and last element (don't need checking)
      while (currentIndex < buffer.length - 1) {
        var previous = buffer(currentIndex - 1)
        var current = buffer(currentIndex)
        var next = buffer(currentIndex + 1)

        if (previous.op == Equals && next.op == Equals) {
          // This is a single edit surrounded by equalities
          if (current.text.endsWith(previous.text)) {
            current = current.copy(current.op, previous.text + sliceLeft(current.text, previous.text.length))
            next = next.copy(next.op, previous.text + next.text)
            // Shift the edit over the previous equality
            buffer = replace(buffer, currentIndex, 2)(current, next)
            changes = true
          }
          else if (current.text.startsWith(next.text)) {
            // Shift the edit over the next equality
            previous = previous.copy(previous.op, previous.text + next.text)
            current = current.copy(current.op, sliceRight(current.text, next.text.length) + next.text)
            buffer = replace(buffer, currentIndex - 1, 2)(previous, current)
            changes = true
          }
        }

        currentIndex += 1
      }

      if (changes) cleanMerge(buffer) else buffer
    }

    /**
      * Generate the longest common subsequence between two strings
      * using the traceback approach to solving this problem
      *
      * @param a First string
      * @param b Second string
      * @return Longest common subsequence
      */
    private def lcs(a: String, b: String): String = {
      // Empty pair of strings? No LCS...
      if (a.isEmpty || b.isEmpty) {
        return ""
      }
      // Same string? LCS is the string itself..
      if (a == b) {
        return a
      }

      // Construct the LCS matrix using the lengths of the subsequents,
      // this is done to reduce the memory needed to solve the problem
      val lengths = Array.ofDim[Int](a.length + 1, b.length + 1)
      for (i <- 0 until a.length) {
        for (j <- 0 until b.length) {
          if (a(i) == b(j)) {
            lengths(i + 1)(j + 1) = lengths(i)(j) + 1
          } else {
            lengths(i + 1)(j + 1) = Math.max(lengths(i + 1)(j), lengths(i)(j + 1))
          }
        }
      }

      // Starting from the last cell in the matrix, trace back towards the origin, accumulating commonalities
      val builder = new StringBuilder()
      var (x, y) = (a.length, b.length)
      do {
        if (lengths(x)(y) == lengths(x - 1)(y)) {
          x -= 1
        } else if (lengths(x)(y) == lengths(x)(y - 1)) {
          y -= 1
        } else {
          builder += a(x - 1)
          x -= 1
          y -= 1
        }
      } while (x != 0 && y != 0)

      // Due to the traceback approach, we built the result in reverse
      builder.toString.reverse
    }

    /**
      * Determine the common prefix of two strings
      *
      * @param text1 First string.
      * @param text2 Second string.
      * @return The number of characters common to the start of each string.
      */
    private def commonPrefix(text1: String, text2: String): Int = {
      // Performance analysis: http://neil.fraser.name/news/2007/10/09/
      val n = Math.min(text1.length, text2.length) - 1
      for (i <- 0 to n) {
        if (text1.charAt(i) != text2.charAt(i)) {
          return i
        }
      }
      n
    }

    /**
      * Determine the common suffix of two strings
      *
      * @param text1 First string.
      * @param text2 Second string.
      * @return The number of characters common to the end of each string.
      */
    private def commonSuffix(text1: String, text2: String): Int = {
      // Performance analysis: http://neil.fraser.name/news/2007/10/09/
      val (text1length, text2length) = (text1.length, text2.length)
      // Return early if strings are empty
      if (text1length == 0 || text2length == 0)
        return 0
      val n = Math.min(text1length, text2length) - 1
      for (i <- 1 to n) {
        if (text1.charAt(text1length - i) != text2.charAt(text2length - i)) {
          return i - 1
        }
      }
      n
    }

    /**
      * Insert one or more elements within a list
      *
      * @param xs       The target list
      * @param index    The index at which to insert the new element
      * @param elements The list of elements to insert
      * @tparam T The type of the list
      * @return List[T]
      */
    def insert[T](xs: List[T], index: Int)(elements: T*): List[T] = {
      xs.take(index) ++ elements ++ xs.drop(index)
    }

    /**
      * Replace one or more elements within a list with another set of elements
      *
      * @param xs       The target list
      * @param start    The index to begin replacement
      * @param num      The number of elements to replace
      * @param elements The elements to swap in
      * @tparam T The type of the list
      * @return List[T]
      */
    def replace[T](xs: List[T], start: Int, num: Int)(elements: T*): List[T] = {
      xs.take(start) ++ elements ++ xs.drop(start + num)
    }

    /**
      * A safe substring operation that starts at the provided index and ends either at the end of the string
      * or after `length` characters have been sliced
      *
      * @param s      The source string
      * @param start  The index to start slicing at
      * @param length The number of characters to slice
      * @return String
      */
    def slice(s: String, start: Int, length: Int = Int.MinValue): String = {
      val len = if (length == Int.MinValue) s.length - start else length
      if (s == null) ""
      else if (start < 0 || len < 0) ""
      else if (start > s.length) ""
      else if (len >= s.length - start) s.substring(start)
      else s.substring(start, start + len)
    }

    /**
      * A safe substring operation that starts at the beginning of the string and selects towards the end
      *
      * @param s The source string
      * @param x The number of characters to select
      * @return String
      */
    def sliceLeft(s: String, x: Int): String = {
      if (s == null) ""
      else if (x <= 0) ""
      else if (x > s.length) s
      else s.substring(0, x)
    }

    /**
      * A safe substring operation that starts at the end of the string and selects towards the beginning
      *
      * @param s The source string
      * @param x The number of characters to select
      * @return String
      */
    def sliceRight(s: String, x: Int): String = {
      if (s == null) ""
      else if (x <= 0) ""
      else if (x > s.length) s
      else s.substring(s.length - x)
    }
  }

}
