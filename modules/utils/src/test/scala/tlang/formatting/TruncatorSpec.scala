package tlang
package formatting

import tlang.formatting.textformatters.Truncator
import tlang.testutils.UnitSpec

class TruncatorSpec extends UnitSpec {

  val truncator = Truncator()

  it should "truncate regular text" in {
    // ABCDEFGHIJKLMNO is 15 chars long
    truncator("ABCDEFGHIJKLMNO", 0) shouldBe ""
    truncator("ABCDEFGHIJKLMNO", 1) shouldBe "."
    truncator("ABCDEFGHIJKLMNO", 2) shouldBe ".."
    truncator("ABCDEFGHIJKLMNO", 3) shouldBe "..."
    truncator("ABCDEFGHIJKLMNO", 4) shouldBe "A..."
    truncator("ABCDEFGHIJKLMNO", 5) shouldBe "AB..."
    truncator("ABCDEFGHIJKLMNO", 10) shouldBe "ABCDEFG..."
    truncator("ABCDEFGHIJKLMNO", 14) shouldBe "ABCDEFGHIJK..."
    truncator("ABCDEFGHIJKLMNO", 15) shouldBe "ABCDEFGHIJKLMNO"
    truncator("ABCDEFGHIJKLMNO", 16) shouldBe "ABCDEFGHIJKLMNO"
  }

  it should "truncate ansi colored text" in {
    val text = "\u001b[32mABC\u001b[33mDEF\u001b[34mGHI\u001b[35mJKL\u001b[36mMNO\u001b[0m" // 15 visible chars long
    truncator(text, 1) should matchWithAnsi(".")
    truncator(text, 2) should matchWithAnsi("..")
    truncator(text, 3) should matchWithAnsi("...")
    truncator(text, 4) should matchWithAnsi("\u001b[32mA\u001b[0m...")
    truncator(text, 5) should matchWithAnsi("\u001b[32mAB\u001b[0m...")
    truncator(text, 10) should matchWithAnsi("\u001b[32mABC\u001b[33mDEF\u001b[34mG\u001b[0m...")
    truncator(text, 14) should matchWithAnsi("\u001b[32mABC\u001b[33mDEF\u001b[34mGHI\u001b[35mJK\u001b[0m...")
    truncator(text, 15) should matchWithAnsi("\u001b[32mABC\u001b[33mDEF\u001b[34mGHI\u001b[35mJKL\u001b[36mMNO\u001b[0m")
    truncator(text, 15) should be theSameInstanceAs text
    truncator(text, 16) should matchWithAnsi("\u001b[32mABC\u001b[33mDEF\u001b[34mGHI\u001b[35mJKL\u001b[36mMNO\u001b[0m")
    truncator(text, 16) should be theSameInstanceAs text

    // Some special cases where we break right at the formatting
    truncator("ABCDEF\u001b[32mGHIJ\u001b[0m", 9) should matchWithAnsi("ABCDEF...")
    truncator("ABCDEF\u001b[32mGHIJK\u001b[0m", 10) should matchWithAnsi("ABCDEF\u001b[32mG\u001b[0m...")
    truncator("\u001b[31mABCDEF\u001b[32mGHIJ\u001b[0m", 9) should matchWithAnsi("\u001b[31mABCDEF\u001b[0m...")
    truncator("\u001b[31mABCDEF\u001b[0mGHIJ", 9) should matchWithAnsi("\u001b[31mABCDEF\u001b[0m...")
    truncator("\u001b[31mABC\u001b[0mDEFGHIJ", 9) should matchWithAnsi("\u001b[31mABC\u001b[0mDEF...")
  }

  it should "throw when given an invalid width" in {
    intercept[IllegalArgumentException] {
      truncator("ABC", -1)
    }.getMessage should include("-1")
  }
}
