package tlang
package utils

import tlang.testutils.UnitSpec

import scala.collection.mutable

class JSONSpec extends UnitSpec {

  case class TestClass() {
    override def toString(): String = "\"ABC\""
  }

  it should "convert primitive types" in {
    JSON(5) shouldBe "5"
    JSON(5l) shouldBe "5"
    JSON(5.0) shouldBe "5.0"
    JSON(5.0f) shouldBe "5.0"
    JSON(-5) shouldBe "-5"

    JSON(true) shouldBe "true"
    JSON(false) shouldBe "false"
    JSON(null) shouldBe "null"

    JSON('c') shouldBe "\"c\""
    JSON("ABC") shouldBe "\"ABC\""
  }

  it should "convert lists" in {
    JSON(Array("A", "B", "C")) shouldBe """["A","B","C"]"""
    JSON(List("A", 1, false)) shouldBe """["A",1,false]"""
  }

  it should "convert maps" in {
    JSON(Map("A" -> "a")) shouldBe """{"A":"a"}"""
    JSON(Map("A" -> "a", "B" -> "b", "C" -> "c")) shouldBe """{"A":"a","B":"b","C":"c"}"""
    JSON(mutable.Map("A" -> 1, "B" -> false)) shouldBe """{"A":1,"B":false}"""
  }

  it should "convert optionals" in {
    JSON(None) shouldBe "null"
    JSON(Some(123)) shouldBe "123"
  }

  it should "escape strings" in {
    JSON("\"ABC\"") shouldBe """"\"ABC\"""""
    JSON(Map("\"ABC\"" -> 1)) shouldBe """{"\"ABC\"":1}"""

    JSON("A\nB\nC") shouldBe """"A\nB\nC""""
    JSON("\"\\\b\f\n\r\t'/'") shouldBe """"\"\\\b\f\n\r\t'/'""""
  }

  it should "convert unknown types to string" in {
    JSON(TestClass()) shouldBe """"\"ABC\"""""
    JSON(Map(TestClass() -> TestClass())) shouldBe """{"\"ABC\"":"\"ABC\""}"""
  }

  it should "do everything!" in {
    JSON(Map(
      "A" -> List(1, "2", false, true, null),
      "B" -> Map(
        "C" -> None,
        "D" -> 'a',
        "E" -> List(TestClass(), Some("abc"))
      ),
      TestClass() -> "ABC"
    )) shouldBe
      """{"A":[1,"2",false,true,null],"B":{"C":null,"D":"a","E":["\"ABC\"","abc"]},"\"ABC\"":"ABC"}"""
  }
}
