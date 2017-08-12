package tlang.utils.formatting

import org.scalatest.{FlatSpec, Matchers}
import tlang.testutils.AnsiMatchers._
import tlang.utils.Extensions._

class WordWrapperSpec extends FlatSpec with Matchers {


  val wordWrapper = WordWrapper()


  it should "wrap words" in {
    wordWrapper("ABC DEFG", 10) shouldBe Seq("ABC DEFG")
    wordWrapper("ABC DEFG", 5) shouldBe Seq("ABC", "DEFG")
    wordWrapper("A DEFGHIJ", 6) shouldBe Seq("A DEFG", "HIJ")
    wordWrapper("ABC DEFGHIJKL", 5) shouldBe Seq("ABC D", "EFGHI", "JKL")
    wordWrapper("ABC DEFG", 2) shouldBe Seq("AB", "C", "DE", "FG")

    wordWrapper("A DEFG", 2) shouldBe Seq("A", "DE", "FG")
    wordWrapper("A BCDEFGHIJKLMNOPQRSTUVXYZ", 25) shouldBe Seq("A", "BCDEFGHIJKLMNOPQRSTUVXYZ")
  }

  it should "put the word on the next line if it fits" in {
    wordWrapper("ABC DEFGHI", 6) shouldBe Seq("ABC", "DEFGHI")
    wordWrapper("ABC   DEFGHI", 6) shouldBe Seq("ABC", "DEFGHI")

    wordWrapper("ABC DEFGHI JKL MNOPQR", 6) shouldBe Seq("ABC", "DEFGHI", "JKL", "MNOPQR")
  }

  it should "fit as much as possible of the next on the current line if it can't fit on its own line" in {
    wordWrapper("ABC DEFGHIJ", 6) shouldBe Seq("ABC DE", "FGHIJ")
    wordWrapper("ABC   DEFGHIJ", 6) shouldBe Seq("ABC", "DEFGHI", "J")

    wordWrapper("A BCDEFGHIJ KL MNOPQRST", 6) shouldBe Seq("A BCDE", "FGHIJ", "KL MNO", "PQRST")
  }

  it should "handle newline characters" in {
    wordWrapper("A\nB\nC\nD\nE\nF\nG", 100) shouldBe Seq("A", "B", "C", "D", "E", "F", "G")
    wordWrapper("ABCDEF\nB\nCDEF\nD", 3) shouldBe Seq("ABC", "DEF", "B", "CDE", "F", "D")

    wordWrapper("A\r\nB\r\nC\r\nD\r\nE\r\nF\r\nG", 100) shouldBe Seq("A", "B", "C", "D", "E", "F", "G")
    wordWrapper("ABCDEF\r\nB\r\nCDEF\r\nD", 3) shouldBe Seq("ABC", "DEF", "B", "CDE", "F", "D")
  }

  it should "keep indentation if possible" in {
    wordWrapper("    ABC DEFG", 7) shouldBe Seq("    ABC", "DEFG")
    wordWrapper("    ABC DEFG", 5) shouldBe Seq("  ABC", "DEFG")
    wordWrapper("    ABC DEFG", 3) shouldBe Seq("ABC", "DEF", "G")
    wordWrapper("    ABC DEFG", 3) shouldBe Seq("ABC", "DEF", "G")

    wordWrapper("    ABC\n    DEF\n  F", 7) shouldBe Seq("    ABC", "    DEF", "  F")
    wordWrapper("    ABC\n    DEF\n  F", 5) shouldBe Seq("  ABC", "  DEF", "  F")
    wordWrapper("    ABC\n    DEF\n  F", 3) shouldBe Seq("ABC", "DEF", "  F")
  }

  it should "keep spaces in the middle of a line" in {
    wordWrapper("ABC   DEFG   HIJ", 8) shouldBe Seq("ABC", "DEFG", "HIJ")
    wordWrapper("AB    DE   HIJ", 8) shouldBe Seq("AB    DE", "DEFG", "HIJ")
    wordWrapper("ABC   DEFGHIJKLM   NOP", 8) shouldBe Seq("ABC   DE", "FGHIJKLM", "NOP")
  }

  it should "split words at special characters if possible" in {
    wordWrapper("ABC-DEFG", 7) shouldBe Seq("ABC-", "DEFG")
    wordWrapper("ABC/DEFG", 7) shouldBe Seq("ABC/", "DEFG")
    wordWrapper("ABC.DEFG", 7) shouldBe Seq("ABC.", "DEFG")
    wordWrapper("ABC_DEFG", 7) shouldBe Seq("ABC_", "DEFG")
    wordWrapper("ABC\\DEFG", 7) shouldBe Seq("ABC\\", "DEFG")
    wordWrapper("ABC,DEFG", 7) shouldBe Seq("ABC,", "DEFG")
    wordWrapper("ABC;DEFG", 7) shouldBe Seq("ABC;", "DEFG")
    wordWrapper("ABC:DEFG", 7) shouldBe Seq("ABC:", "DEFG")
    wordWrapper("ABC(DEFG", 7) shouldBe Seq("ABC(", "DEFG")
    wordWrapper("ABC)DEFG", 7) shouldBe Seq("ABC)", "DEFG")

    wordWrapper("ABC-DEF", 3) shouldBe Seq("ABC", "-", "DEF")
    wordWrapper("ABC/DEF", 3) shouldBe Seq("ABC", "/", "DEF")
  }

  it should "put the first part of the next word on the same line if can split on special characters" in {
    wordWrapper("ABC D.EFGHIJKL", 6) shouldBe Seq("ABC D.", "EFGHIJ", "KL")
    wordWrapper("A BCD-GHIJ-KLM NO PQRS-TUV", 6) shouldBe Seq("A BCD-", "GHIJ-", "KLM", "NO", "PQRS-", "TUV")
  }

  it should "takes as much as possible of a word when splitting words" in {
    wordWrapper("ABCDEFG", 5) shouldBe Seq("ABCDE", "FG")
    wordWrapper("ABCDEFGHIJKLMNOPQRSTUVXYZ", 5) shouldBe Seq("ABCDE", "FGHIJ", "KLMNO", "PQRST", "UVXYZ")
    wordWrapper("ABCDEFGHIJKLMNOPQRSTUVXYZ", 10) shouldBe Seq("ABCDEFGHIJ", "KLMNOPQRST", "UVXYZ")
    wordWrapper("ABCDE ABCDEFGHIJKLMNOPQRSTUVXYZ FGHI", 10) shouldBe Seq("ABDE", "ABCDEFGHIJ", "KLMNOPQRST", "UVXYZ", "FGHI")
  }

  it should "never keep spaces at the end of a line" in {
    wordWrapper("ABCDE      FGHIJKL  ", 5) shouldBe Seq("ABCDE", "FGHIJ", "KL")
    wordWrapper("ABCDE  f   FGHIJKL  M", 5) shouldBe Seq("ABCDE", "f", "FGHIJ", "KL  M")
  }


  it should "wrap correctly with ansi characters" in {
    wordWrapper("\u001b[31mABCD\u001b[0m EFGHIJ \u001b[1mKLMNO\u001b[0m", 10) should
      allMatchWithAnsi(
        "\u001b[31mABCD\u001b[0m",
        "EFGHIJ",
        "\u001b[1mKLMNO\u001b[0m"
      )

    wordWrapper("\u001b[31mABCD DEFGHIJ KLMNOP QRSTU\u001b[0m", 5) should
      allMatchWithAnsi(
        "\u001b[31mABCD\u001b[0m",
        "\u001b[31mDEFGH\u001b[0m",
        "\u001b[31mIJ KL\u001b[0m",
        "\u001b[31mMNOP\u001b[0m",
        "\u001b[31mQRSTU\u001b[0m"
      )

    wordWrapper("\u001b[31mABCD\u001b[32mEFGH\u001b[33mIJKL\u001b[34mMNOP\u001b[0m", 4) should
      allMatchWithAnsi(
        "\u001b[31mABCD\u001b[0m",
        "\u001b[32mEFGH\u001b[0m",
        "\u001b[33mIJKL\u001b[0m",
        "\u001b[34mMNOP\u001b[0m"
      )

    wordWrapper("\u001b[31mABCD\u001b[32mEFGH\u001b[33mIJKL\u001b[34mMN\u001b[0m", 3) should
      allMatchWithAnsi(
        "\u001b[31mABC\u001b[0m",
        "\u001b[31mD\u001b[32mEF\u001b[0m",
        "\u001b[32mGH\u001b[33mI\u001b[0m",
        "\u001b[33mJKL\u001b[0m",
        "\u001b[34mMN\u001b[0m"
      )
  }

  it should "wrap multiple ansi formattings correctly" in {
    wordWrapper("\u001b[1m\u001b[31mABCD\u001b[41m\u001b[32mEFGH\u001b\u001b[4m[33mIJKL\u001b[34mMNOP\u001b[0m", 4).print should
      allMatchWithAnsi(
        "\u001b[1m\u001b[31mABCD\u001b[0m",
        "\u001b[1m\u001b[32m\u001b[41mEFGH\u001b[0m",
        "\u001b[1m\u001b[4m\u001b[33m\u001b[41mIJKL\u001b[0m",
        "\u001b[1m\u001b[4m\u001b[34m\u001b[41mMNOP\u001b[0m"
      )
  }

}
