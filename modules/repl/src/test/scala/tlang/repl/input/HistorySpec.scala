package tlang
package repl
package input

import tlang.testutils.UnitSpec

class HistorySpec extends UnitSpec {

  behavior of "A history"

  it should "be added to" in {
    val history = History[Int]()

    history.length shouldBe 0

    history.current shouldBe None

    history += 1

    history.length shouldBe 1
    history.current shouldBe Some(1)

    history += 2
    history += 3

    history.isEmpty shouldBe false
    history.nonEmpty shouldBe true
    history.length shouldBe 3

    history.current shouldBe Some(3)

    history ++= Seq(4, 5, 6)

    history.isEmpty shouldBe false
    history.nonEmpty shouldBe true
    history.length shouldBe 6

    history.current shouldBe Some(6)
    history(0) shouldBe 1
    history(4) shouldBe 5

    history.clear()

    history.isEmpty shouldBe true
    history.nonEmpty shouldBe false
    history.length shouldBe 0

    history.current shouldBe None
  }

  it should "be handle undo and redo" in {
    val history = History[Int]()

    history += 1
    history += 2
    history += 3

    history.current shouldBe Some(3)
    history.undo shouldBe true
    history.current shouldBe Some(2)

    history.undo shouldBe true
    history.current shouldBe Some(1)

    history.undo shouldBe false
    history.current shouldBe Some(1)

    history.redo shouldBe true
    history.current shouldBe Some(2)

    history.redo shouldBe true
    history.current shouldBe Some(3)

    history.redo shouldBe false
    history.current shouldBe Some(3)
  }

  it should "be iterable" in {
    val history = History[Int]()

    history ++= Seq(1, 2, 3, 4, 5)
    val it = history.iterator

    it.hasNext shouldBe true
    it.next shouldBe 1

    it.hasNext shouldBe true
    it.next shouldBe 2

    it.hasNext shouldBe true
    it.next shouldBe 3

    it.hasNext shouldBe true
    it.next shouldBe 4

    it.hasNext shouldBe true
    it.next shouldBe 5

    it.hasNext shouldBe false
  }

  it should "replace future history when modified in the middle" in {
    val history = History[Int]()

    history += 1
    history += 2
    history += 3

    history.undo shouldBe true
    history.undo shouldBe true

    history += 4
    history += 5
    history += 6

    history.size shouldBe 4
    history.toList shouldBe List(1, 4, 5, 6)
  }

  it should "remove first element if exceeding maximum size" in {
    val history = History[Int](maxSize = 3)

    history += 1
    history += 2
    history += 3

    history.size shouldBe 3

    history += 4
    history += 5
    history += 6

    history.size shouldBe 3
    history.toList shouldBe List(4, 5, 6)

    history.undo shouldBe true

    history += 7
    history.size shouldBe 3
    history.toList shouldBe List(4, 5, 7)

    history += 8
    history.size shouldBe 3
    history.toList shouldBe List(5, 7, 8)

    history += 9
    history.size shouldBe 3
    history.toList shouldBe List(7, 8, 9)

    history.undo shouldBe true
    history.undo shouldBe true
    history += 10

    history.size shouldBe 2
    history.toList shouldBe List(7, 10)
  }
}
