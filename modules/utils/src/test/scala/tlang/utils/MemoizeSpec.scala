package tlang
package utils

import tlang.testutils.UnitSpec

class MemoizeSpec extends UnitSpec {

  it should "save the calculated value" in {
    val sleepTime = 5L

    val memoized = Memoize {
      Thread.sleep(sleepTime)
      25
    }

    var startTime = System.currentTimeMillis
    var res = memoized()
    var elapsedTime = System.currentTimeMillis - startTime

    elapsedTime should be >= sleepTime
    res shouldBe 25

    startTime = System.currentTimeMillis
    res = memoized()
    elapsedTime = System.currentTimeMillis - startTime

    res shouldBe 25
    elapsedTime should be < sleepTime

    memoized.reset()

    startTime = System.currentTimeMillis
    res = memoized()
    elapsedTime = System.currentTimeMillis - startTime
    res shouldBe 25
    elapsedTime should be >= sleepTime
  }
}
