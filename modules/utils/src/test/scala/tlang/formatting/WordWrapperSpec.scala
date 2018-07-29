package tlang
package formatting

import tlang.formatting.textformatters.WordWrapper
import tlang.testutils.UnitSpec

class WordWrapperSpec extends UnitSpec {


  val wordWrapper = WordWrapper()


  behavior of "A word wrapper"


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
    wordWrapper("ABC   DEFGHIJ", 6) shouldBe Seq("ABC DE", "FGHIJ")

    wordWrapper("A BCDEFGHIJ KL MNOPQRST", 6) shouldBe Seq("A BCDE", "FGHIJ", "KL MNO", "PQRST")
  }


  it should "handle newline characters" in {
    wordWrapper("A\nB\nC\nD\nE\nF\nG", 100) shouldBe Seq("A", "B", "C", "D", "E", "F", "G")
    wordWrapper("ABCDEF\nB\nCDEF\nD", 3) shouldBe Seq("ABC", "DEF", "B", "CDE", "F", "D")

    wordWrapper("A\r\nB\r\nC\r\nD\r\nE\r\nF\r\nG", 100) shouldBe Seq("A", "B", "C", "D", "E", "F", "G")
    wordWrapper("ABCDEF\r\nB\r\nCDEF\r\nD", 3) shouldBe Seq("ABC", "DEF", "B", "CDE", "F", "D")

    wordWrapper("ABC\n\n\nB\n\nCDEF\n", 3) shouldBe Seq("ABC", "", "", "B", "", "CDE", "F", "")
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


  it should "keep ansi characters before indentation" in {
    wordWrapper("\u001b[31m    ABC DEFG\u001b[0m", 7) should allMatchWithAnsi(
      "\u001b[31m    ABC\u001b[0m",
      "\u001b[31mDEFG\u001b[0m"
    )
  }


  it should "replace tabs with spaces" in {
    wordWrapper("ABC\tDEF\tGHI\t  JKL", 100) shouldBe Seq("ABC  DEF  GHI    JKL")
    wordWrapper.copy(tabSize = 5)("ABC\tDEF\tGHI\t  JKL", 100) shouldBe Seq("ABC     DEF     GHI       JKL")
  }


  it should "wrap with width 1" in {
    wordWrapper("ABC DEFG HI.J/K-L MnoPqrSTUV", 1) shouldBe Seq(
      "A", "B", "C", "D", "E", "F", "G", "H", "I", ".", "J", "/", "K", "-", "L", "M", "n", "o", "P", "q", "r", "S", "T", "U", "V"
    )
  }


  it should "handle larger input" in {
    wordWrapper(
      """|Lorem ipsum dolor sit amet, consectetur adipiscing elit. Suspendisse bibendum tempor mi, ut ullamcorper lacus lobortis a. Maecenas nisi nisl, pretium eu sagittis quis, elementum vitae ante. Morbi nec augue efficitur, vestibulum diam non, suscipit ipsum. Vivamus dapibus turpis ac placerat tempus. Nam ultricies tincidunt felis. Aenean fermentum, nisi sodales euismod condimentum, nibh nibh rutrum mi, nec eleifend eros lectus dapibus diam. Praesent rhoncus est sit amet pharetra malesuada. Phasellus vitae interdum leo. Nam viverra, mi non accumsan luctus, diam ex sodales urna, quis mollis metus mauris et quam. Maecenas varius, ligula vitae egestas convallis, felis urna ullamcorper leo, eget maximus leo dui quis magna. Nunc elit nisl, vulputate at faucibus et, molestie in arcu.
         |
         |Quisque quis nibh in velit iaculis pellentesque ornare ut purus. Cras vel rhoncus metus. Praesent eget commodo justo. Ut et rutrum libero. Fusce suscipit malesuada sem. Suspendisse potenti. Praesent scelerisque lectus mi, eu elementum nulla vulputate non. Sed cursus libero sed turpis sollicitudin venenatis. Curabitur eu feugiat ex.
      """.stripMargin.trim, 50) shouldBe
      Seq(
        "Lorem ipsum dolor sit amet, consectetur adipiscing",
        "elit. Suspendisse bibendum tempor mi, ut",
        "ullamcorper lacus lobortis a. Maecenas nisi nisl,",
        "pretium eu sagittis quis, elementum vitae ante.",
        "Morbi nec augue efficitur, vestibulum diam non,",
        "suscipit ipsum. Vivamus dapibus turpis ac placerat",
        "tempus. Nam ultricies tincidunt felis. Aenean",
        "fermentum, nisi sodales euismod condimentum, nibh",
        "nibh rutrum mi, nec eleifend eros lectus dapibus",
        "diam. Praesent rhoncus est sit amet pharetra",
        "malesuada. Phasellus vitae interdum leo. Nam",
        "viverra, mi non accumsan luctus, diam ex sodales",
        "urna, quis mollis metus mauris et quam. Maecenas",
        "varius, ligula vitae egestas convallis, felis urna",
        "ullamcorper leo, eget maximus leo dui quis magna.",
        "Nunc elit nisl, vulputate at faucibus et, molestie",
        "in arcu.",
        "",
        "Quisque quis nibh in velit iaculis pellentesque",
        "ornare ut purus. Cras vel rhoncus metus. Praesent",
        "eget commodo justo. Ut et rutrum libero. Fusce",
        "suscipit malesuada sem. Suspendisse potenti.",
        "Praesent scelerisque lectus mi, eu elementum nulla",
        "vulputate non. Sed cursus libero sed turpis",
        "sollicitudin venenatis. Curabitur eu feugiat ex."
      )
  }


  it should "split words at special characters" in {
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
    wordWrapper("ABC[DEFG", 7) shouldBe Seq("ABC[", "DEFG")
    wordWrapper("ABC]DEFG", 7) shouldBe Seq("ABC]", "DEFG")
    wordWrapper("ABC{DEFG", 7) shouldBe Seq("ABC{", "DEFG")
    wordWrapper("ABC}DEFG", 7) shouldBe Seq("ABC}", "DEFG")
  }


  it should "split words at camel cases" in {
    wordWrapper("AbcDefghi", 7) shouldBe Seq("Abc", "Defghi")
    wordWrapper("AbcdeFghi", 7) shouldBe Seq("Abcde", "Fghi")
    wordWrapper("AbcdeFGHI", 7) shouldBe Seq("Abcde", "FGHI")
  }


  it should "split at the last special case" in {
    wordWrapper("ABC.-/DEFG", 7) shouldBe Seq("ABC.-/", "DEFG")
    wordWrapper("A.B.C.D.E.F.G.H", 7) shouldBe Seq("A.B.C.", "D.E.F.", "G.H")

    wordWrapper("AbcDefGhi", 8) shouldBe Seq("AbcDef", "Ghi")
    wordWrapper("ABC.DefGhi", 8) shouldBe Seq("ABC.Def", "Ghi")
  }


  it should "put the first part of the next word on the same line if can split on special characters" in {
    wordWrapper("ABC D.EFGHIJKL", 6) shouldBe Seq("ABC D.", "EFGHIJ", "KL")
    wordWrapper("A B-CDE-FGH IJK MNOP-QRS", 6) shouldBe Seq("A B-", "CDE-", "FGH", "IJK MN", "OP-QRS")
  }


  it should "take as much as possible of a word when splitting words" in {
    wordWrapper("ABCDEFG", 5) shouldBe Seq("ABCDE", "FG")
    wordWrapper("ABCDEFGHIJKLMNOPQRSTUVXYZ", 5) shouldBe Seq("ABCDE", "FGHIJ", "KLMNO", "PQRST", "UVXYZ")
    wordWrapper("ABCDEFGHIJKLMNOPQRSTUVXYZ", 10) shouldBe Seq("ABCDEFGHIJ", "KLMNOPQRST", "UVXYZ")
    wordWrapper("ABCDE ABCDEFGHIJKLMNOPQRSTUVXYZ FGHI", 10) shouldBe Seq("ABCDE ABCD", "EFGHIJKLMN", "OPQRSTUVXY", "Z FGHI")
  }


  it should "never keep spaces at the end of a line" in {
    wordWrapper("ABCDE      FGHIJKL  ", 5) shouldBe Seq("ABCDE", "FGHIJ", "KL")
    wordWrapper("ABCDE  f   FGHIJKL  M", 5) shouldBe Seq("ABCDE", "f FGH", "IJKL", "M")
  }


  it should "wrap correctly with ansi characters" in {
    wordWrapper("\u001b[31mABCD\u001b[32mEFGH\u001b[33mIJKL\u001b[34mMNOP\u001b[0m", 16) should
      allMatchWithAnsi("\u001b[31mABCD\u001b[32mEFGH\u001b[33mIJKL\u001b[34mMNOP\u001b[0m")

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

    wordWrapper("\u001b[31mABCD\nDEFGHIJ\nKLMNOP\nQRSTU\u001b[0m", 5) should
      allMatchWithAnsi(
        "\u001b[31mABCD\u001b[0m",
        "\u001b[31mDEFGH\u001b[0m",
        "\u001b[31mIJ\u001b[0m",
        "\u001b[31mKLMNO\u001b[0m",
        "\u001b[31mP\u001b[0m",
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


  it should "handle disabling ansi wrapping" in {
    val wordWrapperWithoutAnsi = WordWrapper(wrapAnsiColors = false)
    wordWrapperWithoutAnsi("\u001b[31mABCD\u001b[32mEFGH\u001b[33mIJKL\u001b[34mMN\u001b[0m", 3) should
      allMatchWithAnsi(
        "\u001b[31mABC",
        "D\u001b[32mEF",
        "GH\u001b[33mI",
        "JKL",
        "\u001b[34mMN\u001b[0m"
      )
  }


  it should "wrap multi-ansi escape codes correctly" in {
    wordWrapper("\u001b[1;4;31;42mABCD DEFGHIJ KLMNOP QRSTU\u001b[0m", 5) should
      allMatchWithAnsi(
        "\u001b[1;4;31;42mABCD\u001b[0m",
        "\u001b[1;4;31;42mDEFGH\u001b[0m",
        "\u001b[1;4;31;42mIJ KL\u001b[0m",
        "\u001b[1;4;31;42mMNOP\u001b[0m",
        "\u001b[1;4;31;42mQRSTU\u001b[0m"
      )
  }


  it should "simplify ansi escape sequences at the start of a line" in {
    wordWrapper("\u001b[1m\u001b[4m\u001b[42m\u001b[33m\u001b[31mABCD\u001b[0m", 16) should
      allMatchWithAnsi("\u001b[1;4;31;42mABCD\u001b[0m")
  }


  it should "wrap multiple ansi escape codes correctly" in {
    wordWrapper("\u001b[1m\u001b[31mABCD\u001b[41m\u001b[32mEFGH\u001b[4m\u001b[33mIJKL\u001b[34mMNOP\u001b[0m", 4) should
      allMatchWithAnsi(
        "\u001b[1;31mABCD\u001b[0m",
        "\u001b[1;32;41mEFGH\u001b[0m",
        "\u001b[1;4;33;41mIJKL\u001b[0m",
        "\u001b[1;4;34;41mMNOP\u001b[0m"
      )

    wordWrapper("\u001b[1;31mABCD\u001b[41;32mEFGH\u001b[33;4mIJKL\u001b[34mMNOP\u001b[0m", 4) should
      allMatchWithAnsi(
        "\u001b[1;31mABCD\u001b[0m",
        "\u001b[1;32;41mEFGH\u001b[0m",
        "\u001b[1;4;33;41mIJKL\u001b[0m",
        "\u001b[1;4;34;41mMNOP\u001b[0m"
      )
  }


  it should "return an empty line when line only contains ansi characters" in {
    wordWrapper("\u001b[31m\n\u001b[0m", 3) should allMatchWithAnsi("", "")
  }


  it should "return a line when line only contains whitespaces" in {
    wordWrapper("\t\t  \n    \t\n", 3) should allMatchWithAnsi("      ", "      ", "")
  }


  it should "handle real life test cases" in {
    wordWrapper("\u001b[32mLjava/io/PrintStream;\u001b[0m", 3) should
      allMatchWithAnsi(
        "\u001b[32mLja\u001b[0m",
        "\u001b[32mva/\u001b[0m",
        "\u001b[32mio/\u001b[0m",
        "\u001b[32mPri\u001b[0m",
        "\u001b[32mnt\u001b[0m",
        "\u001b[32mStr\u001b[0m",
        "\u001b[32meam\u001b[0m",
        "\u001b[32m;\u001b[0m"
      )

    wordWrapper("\u001b[32mio/PrintStream;\u001b[0m", 3) should
      allMatchWithAnsi(
        "\u001b[32mio/\u001b[0m",
        "\u001b[32mPri\u001b[0m",
        "\u001b[32mnt\u001b[0m",
        "\u001b[32mStr\u001b[0m",
        "\u001b[32meam\u001b[0m",
        "\u001b[32m;\u001b[0m"
      )

    wordWrapper("\u001b[32mSystem \u001b[33mout \u001b[36mLjava/io/PrintStream;\u001b[0m", 3) should
      allMatchWithAnsi(
        "\u001b[32mSys\u001b[0m",
        "\u001b[32mtem\u001b[0m",
        "\u001b[33mout\u001b[0m",
        "\u001b[36mLja\u001b[0m",
        "\u001b[36mva/\u001b[0m",
        "\u001b[36mio/\u001b[0m",
        "\u001b[36mPri\u001b[0m",
        "\u001b[36mnt\u001b[0m",
        "\u001b[36mStr\u001b[0m",
        "\u001b[36meam\u001b[0m",
        "\u001b[36m;\u001b[0m"
      )

    wordWrapper("\u001b[32mjava/lang/System \u001b[33mout \u001b[36mLjava/io/PrintStream;\u001b[0m", 3) should
      allMatchWithAnsi(
        "\u001b[32mjav\u001b[0m",
        "\u001b[32ma/\u001b[0m",
        "\u001b[32mlan\u001b[0m",
        "\u001b[32mg/\u001b[0m",
        "\u001b[32mSys\u001b[0m",
        "\u001b[32mtem\u001b[0m",
        "\u001b[33mout\u001b[0m",
        "\u001b[36mLja\u001b[0m",
        "\u001b[36mva/\u001b[0m",
        "\u001b[36mio/\u001b[0m",
        "\u001b[36mPri\u001b[0m",
        "\u001b[36mnt\u001b[0m",
        "\u001b[36mStr\u001b[0m",
        "\u001b[36meam\u001b[0m",
        "\u001b[36m;\u001b[0m"
      )
  }

}
