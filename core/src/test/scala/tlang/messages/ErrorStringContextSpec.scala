package tlang.messages

import tlang.testutils.UnitSpec

class ErrorStringContextSpec extends UnitSpec {


  it should "make text bold" in {
    val errorStringContext = makeErrorStringContext(useColor = true)
    import errorStringContext._

    err"ABC DEF" should matchWithAnsi("\u001b[1mABC DEF\u001b[0m")
  }


  it should "should mark interpolated values with color" in {
    val errorStringContext = makeErrorStringContext(useColor = true)
    import errorStringContext._

    val x = "x"
    val y = "y"
    val z = "z"

    err"" should matchWithAnsi("")

    err"$x" should matchWithAnsi("\u001b[1;35mx\u001b[0m")
    err"$x$y$z" should matchWithAnsi("\u001b[1;35mxyz\u001b[0m")

    err"$x ABC $y DEF $z" should matchWithAnsi("\u001b[1;35mx\u001b[0m\u001b[1m ABC \u001b[35my\u001b[0m\u001b[1m DEF \u001b[35mz\u001b[0m")
  }


  it should "should mark interpolated values without color" in {
    val errorStringContext = makeErrorStringContext(useColor = false)
    import errorStringContext._

    val x = "x"
    val y = "y"
    val z = "z"

    err"" should matchWithAnsi("")

    err"$x" should matchWithAnsi("'x'")
    err"$x$y$z" should matchWithAnsi("'x''y''z'")

    err"$x ABC $y DEF $z" should matchWithAnsi("'x' ABC 'y' DEF 'z'")
  }


  it should "transform interpolated values" in {
    test("One transform") {
      val transforms = List[String => String](
        s => if (s == "ABC") "DEF" else s
      )
      val errorStringContext = makeErrorStringContext(useColor = false, transforms = transforms)
      import errorStringContext._

      val abc = "ABC"

      err"ABC DEF" should matchWithAnsi("ABC DEF")
      err"$abc DEF" should matchWithAnsi("'DEF' DEF")
    }

    test("Multiple transforms") {
      val transforms = List[String => String](
        s => if (s == "ABC") "DEF" else s,
        s => s.toLowerCase,
        s => s"---$s---",
        s => s * 3
      )
      val errorStringContext = makeErrorStringContext(useColor = false, transforms = transforms)
      import errorStringContext._

      val abc = "ABC"

      err"ABC DEF" should matchWithAnsi("ABC DEF")
      err"$abc DEF" should matchWithAnsi("'---def------def------def---' DEF")
    }

  }


  it should "suggest corrections" in {
    val alternatives = List("ABC", "DEF", "GHI")

    test("No suggestion") {
      val alternativeSuggestor = mock[AlternativeSuggestor]
      alternativeSuggestor.apply("ABCD", alternatives) returns Suggestion(Nil)

      test("With color") {
        val errorStringContext = makeErrorStringContext(useColor = true, alternativeSuggestor = alternativeSuggestor)
        import errorStringContext._

        err"Some text.${ suggestion("ABCD", alternatives) }More text." should matchWithAnsi(
          "\u001b[1mSome text. More text.\u001b[0m"
        )
      }

      test("Without color") {
        val errorStringContext = makeErrorStringContext(useColor = false, alternativeSuggestor = alternativeSuggestor)
        import errorStringContext._

        err"Some text.${ suggestion("ABCD", alternatives) }More text." should matchWithAnsi("Some text. More text.")
      }
    }

    test("One suggestion") {
      val alternativeSuggestor = mock[AlternativeSuggestor]
      alternativeSuggestor.apply("ABCD", alternatives) returns Suggestion(List("ABC"))

      test("With color") {
        val errorStringContext = makeErrorStringContext(useColor = true, alternativeSuggestor = alternativeSuggestor)
        import errorStringContext._

        err"Some text.${ suggestion("ABCD", alternatives) }More text." should matchWithAnsi(
          "\u001b[1mSome text. Did you mean \u001b[35mABC\u001b[0m\u001b[1m? More text.\u001b[0m"
        )
      }

      test("Without color") {
        val errorStringContext = makeErrorStringContext(useColor = false, alternativeSuggestor = alternativeSuggestor)
        import errorStringContext._

        err"Some text.${ suggestion("ABCD", alternatives) }More text." should matchWithAnsi("Some text. Did you mean 'ABC'? More text.")
      }
    }

    test("Multiple suggestions") {
      val alternatives = List("ABC", "DEF", "GHI")
      val alternativeSuggestor = mock[AlternativeSuggestor]
      alternativeSuggestor.apply("ABCD", alternatives) returns Suggestion(List("ABC", "DEF", "GHI"))

      test("With color") {
        val errorStringContext = makeErrorStringContext(useColor = true, alternativeSuggestor = alternativeSuggestor)
        import errorStringContext._

        err"Some text.${ suggestion("ABCD", alternatives) }More text." should matchWithAnsi(
          s"""|\u001b[1mSome text. Did you mean?
              |   * \u001b[35mABC
              |\u001b[0m\u001b[1m   * \u001b[35mDEF
              |\u001b[0m\u001b[1m   * \u001b[35mGHI
              |\u001b[0m\u001b[1mMore text.\u001b[0m""".stripMargin
        )
      }

      test("Without color") {
        val errorStringContext = makeErrorStringContext(useColor = false, alternativeSuggestor = alternativeSuggestor)
        import errorStringContext._

        err"Some text.${ suggestion("ABCD", alternatives) }More text." should matchWithAnsi(
          s"""|Some text. Did you mean?
              |   * ABC
              |   * DEF
              |   * GHI
              |More text.""".stripMargin
        )
      }
    }

    test("Without followup text.") {
      val alternatives = List("ABC", "DEF", "GHI")
      val alternativeSuggestor = mock[AlternativeSuggestor]
      alternativeSuggestor.apply("A", alternatives) returns Suggestion(Nil)
      alternativeSuggestor.apply("AB", alternatives) returns Suggestion(List("ABC"))
      alternativeSuggestor.apply("ABC", alternatives) returns Suggestion(List("ABC", "DEF", "GHI"))

      val errorStringContext = makeErrorStringContext(useColor = false, alternativeSuggestor = alternativeSuggestor)
      import errorStringContext._

      err"Some text.${ suggestion("A", alternatives) }" should matchWithAnsi("Some text.")
      err"Some text.${ suggestion("AB", alternatives) }" should matchWithAnsi("Some text. Did you mean 'ABC'?")
      err"Some text.${ suggestion("ABC", alternatives) }" should matchWithAnsi(
        s"""|Some text. Did you mean?
            |   * ABC
            |   * DEF
            |   * GHI""".stripMargin
      )

      val x = "x"
      err"Some text.${ suggestion("A", alternatives) }$x" should matchWithAnsi("Some text. 'x'")
      err"Some text.${ suggestion("AB", alternatives) }$x" should matchWithAnsi("Some text. Did you mean 'ABC'? 'x'")
      err"Some text.${ suggestion("ABC", alternatives) }$x" should matchWithAnsi(
        s"""|Some text. Did you mean?
            |   * ABC
            |   * DEF
            |   * GHI
            |'x'""".stripMargin
      )
    }

  }

  private def makeErrorStringContext(
    useColor: Boolean,
    alternativeSuggestor: AlternativeSuggestor = mock[AlternativeSuggestor],
    transforms: List[String => String] = Nil
  ): ErrorStringContext = {
    val formatter = createMockFormatter(useColor = useColor)
    ErrorStringContext(formatter, alternativeSuggestor = alternativeSuggestor, transforms = transforms)
  }


}
