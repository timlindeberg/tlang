package tlang.compiler.error

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
    err"$x$y$z" should matchWithAnsi("\u001b[1;35mx\u001b[0m\u001b[1;35my\u001b[0m\u001b[1;35mz\u001b[0m")

    err"$x ABC $y DEF $z" should matchWithAnsi("\u001b[1;35mx\u001b[0m\u001b[1m ABC \u001b[0m\u001b[1;35my\u001b[0m\u001b[1m DEF \u001b[0m\u001b[1;35mz\u001b[0m")
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
    val alternativeSuggestor = mock[AlternativeSuggestor]
    (alternativeSuggestor.apply _).expects("ABCD", alternatives).twice().returning(Suggestion("ABC"))

    test("Colored suggestion") {
      val errorStringContext = makeErrorStringContext(useColor = true, alternativeSuggestor = alternativeSuggestor)
      import errorStringContext._

      err"ABCD DEF.${ suggestion("ABCD", alternatives) }" should matchWithAnsi("\u001b[1mABCD DEF.\u001b[0m\u001b[1m Did you mean \u001b[0m\u001b[1;35mABC\u001b[0m\u001b[1m?\u001b[0m")
    }

    test("Non colored suggestion") {
      val errorStringContext = makeErrorStringContext(useColor = false, alternativeSuggestor = alternativeSuggestor)
      import errorStringContext._

      err"ABCD DEF.${ suggestion("ABCD", alternatives) }" should matchWithAnsi("ABCD DEF. Did you mean 'ABC'?")
    }

  }

  private def makeErrorStringContext(
    useColor: Boolean,
    alternativeSuggestor: AlternativeSuggestor = mock[AlternativeSuggestor],
    transforms: List[String => String] = Nil
  ): ErrorStringContext = {
    val formatter = createMockFormatter(useColor = useColor)
    ErrorStringContext(formatter.formatting, alternativeSuggestor = alternativeSuggestor, transforms = transforms)
  }


}
