package tlang
package formatting

import tlang.testutils.UnitSpec

class AlternativeSuggestorSpec extends UnitSpec {

  val suggestor = AlternativeSuggestor()

  behavior of "AlternativeSuggestor"

  it should "calculate the correct distance between words" in {
    Distance("", "test") shouldBe 4
    Distance("test", "") shouldBe 4
    Distance("", "") shouldBe 0
    Distance(" ", " ") shouldBe 0
    Distance("", " ") shouldBe 1
    Distance(" ", "") shouldBe 1
    Distance("test", "test") shouldBe 0
    Distance("Test", "test") shouldBe 1
    Distance("test", "testy") shouldBe 1
    Distance("testy", "test") shouldBe 1
    Distance("test", "testing") shouldBe 3
    Distance("test", "tets") shouldBe 1
    Distance("test", "tsety") shouldBe 2
    Distance("Test", "tsety") shouldBe 3
    Distance("test", "test ") shouldBe 1
    Distance("file", "test") shouldBe 4
    Distance("file", "testy") shouldBe 5
    Distance("Pomatomus", "Pomatomus") shouldBe 0
    Distance(" Pomatomus", "Pomatomus") shouldBe 1
    Distance("Pomatomus", "pomatomus") shouldBe 1
    Distance("Pomatomus", "") shouldBe 9
    Distance("", "Pomatomus") shouldBe 9
    Distance("P", "p") shouldBe 1
    Distance("ab", "a") shouldBe 1
    Distance("-b", "-") shouldBe 1
    Distance("a", "ab") shouldBe 1
    Distance("L", "Linneaus") shouldBe 7
    Distance("Pomatomus", "Pomatomux") shouldBe 1
    Distance("Pmatomus", "Pomatomus") shouldBe 1
    Distance("Pomatomus", "Pmatomus") shouldBe 1
    Distance("Rpmatomus", "Pomatomus") shouldBe 2
    Distance("Pommtomus", "Pomatomus") shouldBe 1
    Distance("Potamomus", "Pomatomus") shouldBe 2
    Distance("Cedarinia scabra Sjöstedt 1921", "Cedarinia scabra Sjostedt 1921") shouldBe 1
    Distance("Pomatomus", "oPmatomus") shouldBe 1
    Distance("Pomatomus", "Pomatomsu") shouldBe 1
    Distance("Pomtaomus", "Pomatomus") shouldBe 1
    Distance("Pomatoums", "Pomatomus") shouldBe 1
    Distance("Poamtosus", "Pomatomus") shouldBe 2
    Distance("Cedarinia scabra Sjöstedt 1921", "Cedarinia scabra Söjstedt 1921") shouldBe 1
    Distance("vesiculosus", "vecusilosus") shouldBe 4
    Distance("trimerophyton", "mertriophyton") shouldBe 6


    // Special case where case doesn't match but is similar otherwise
    Distance("ABCDEFGH", "abcdefgh") shouldBe 1
  }

  it should "suggest matching alternatives" in {
    suggestor("ABCDEF", List("ABDEF", "CDAB")) shouldBe Suggestion(List("ABDEF"))
    suggestor("ABD", List("ABC", "CDAB")) shouldBe Suggestion(List("ABC"))
    suggestor("ABCDEFGHKLMNO", List("ABCDEFGHIJKLMNO", "CDAB")) shouldBe Suggestion(List("ABCDEFGHIJKLMNO"))
    suggestor("ABCDEFGHIJKLMNO", List("ABCDEFGHIJKLMNOPQRSTUVXYZ", "CDAB")) shouldBe Suggestion(List("ABCDEFGHIJKLMNOPQRSTUVXYZ"))
  }

  it should "not suggest alternatives shorter than 3 characters" in {
    suggestor("AB", List("AC", "CB")) shouldBe Suggestion(Nil)
    suggestor("a", List("b", "c")) shouldBe Suggestion(Nil)
  }

  it should "suggest alternatives where case doesn't match but is similar otherwise" in {
    suggestor("ABCDEFGHI", List("abcdefghi")) shouldBe Suggestion(List("abcdefghi"))
  }

  it should "suggest multiple alternatives in order of similarity" in {
    suggestor("ABCDEF", List("ABDEF", "ABDCFE", "ABCDE")) shouldBe Suggestion(List("ABDEF", "ABCDE", "ABDCFE"))
  }

  it should "return a maximum of 5 suggestions" in {
    suggestor("ABCDEF", List("BCDEF", "ACDEF", "ABDEF", "ABCEF", "ABCDF", "ABCDE")) shouldBe
      Suggestion(List("BCDEF", "ACDEF", "ABDEF", "ABCEF", "ABCDF"))
  }
}
