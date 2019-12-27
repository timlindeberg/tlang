package tlang
package testutils

import org.scalactic.source
import org.scalatest.{AppendedClues, Assertion, Matchers}
import tlang.utils.Position

object TestPosition extends Matchers with AppendedClues {
  def compare(position: Position, test: TestPosition, clue: Option[String] = None): Assertion = {
    import test.sourcePos
    if (test.description.isEmpty) {
      position shouldBe test.expectedPos withClue s" for ${ test.description }"
    } else if (clue.nonEmpty) {
      position shouldBe test.expectedPos withClue clue.get
    } else {
      position shouldBe test.expectedPos
    }
  }
}

case class TestPosition(line: Int, col: Int, endLine: Int, endCol: Int, description: String = "")
  (implicit val sourcePos: source.Position) {
  val expectedPos: Position = Position(line, col, endLine, endCol)
}
