package tlang
package utils

import tlang.testutils.UnitSpec

class CircularBufferSpec extends UnitSpec {

  it should "insert elements" in {
    val circularBuffer = CircularBuffer[Int](1, 2, 3, 4)
    circularBuffer += 5
    circularBuffer should contain theSameElementsInOrderAs Seq(1, 2, 3, 4, 5)
  }

  it should "delete elements" in {
    val circularBuffer = CircularBuffer[Int](1, 2, 3, 4, 5)
    circularBuffer -= 5
    circularBuffer should contain theSameElementsInOrderAs Seq(1, 2, 3, 4)
  }

  it should "use a circular index" in {
    val circularBuffer = CircularBuffer[Int](1, 2, 3, 4)

    circularBuffer.setPosition(2)
    circularBuffer.index shouldBe 2
    circularBuffer.current shouldBe 3

    circularBuffer.advance(1)
    circularBuffer.index shouldBe 3
    circularBuffer.current shouldBe 4

    circularBuffer.advance(2)
    circularBuffer.index shouldBe 1
    circularBuffer.current shouldBe 2

    circularBuffer.advance(-5)
    circularBuffer.index shouldBe 0
    circularBuffer.current shouldBe 1
  }
}
