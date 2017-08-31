package tlang.options

import tlang.formatting.{Formatter, SimpleFormatting}
import tlang.messages.ErrorStringContext
import tlang.testutils.UnitSpec

class OptionsSpec extends UnitSpec {

  implicit val errorContext: ErrorStringContext = ErrorStringContext(SimpleFormatting)


  behavior of "An option parser"


  it should "parse positional arguments" in {
    val positionalArgument = new PositionalArgument[Set[String]]() {
      override def parseValue(args: Set[String]): Set[String] = args
    }

    val args = "ABC DEF GHI".split(" ")
    val options = Options(Nil, Some(positionalArgument), args)
    options(positionalArgument) shouldBe Set("ABC", "DEF", "GHI")
  }


  it should "parse boolean flags" in {
    val a = new BooleanFlag {
      override def name = "a"
      override def description(formatter: Formatter): Nothing = ???
    }
    val b = new BooleanFlag {
      override def name = "b"
      override def description(formatter: Formatter): Nothing = ???
    }
    val c = new BooleanFlag {
      override def name = "c"
      override def description(formatter: Formatter): Nothing = ???
    }

    val args = "--a --b".split(" ")
    val options = Options(List(a, b, c), None, args)

    options(a) shouldBe true
    options(b) shouldBe true
    options(c) shouldBe false
  }


  it should "parse argument flags" in {
    val a = new ArgumentFlag[Set[String]] {
      override def name = "a"
      override def parseValue(args: Set[String]): Set[String] = args
      override def argDescription: Nothing = ???
      override def description(formatter: Formatter): Nothing = ???
    }
    val b = new ArgumentFlag[Set[String]] {
      override def name = "b"
      override def parseValue(args: Set[String]): Set[String] = args
      override def argDescription: Nothing = ???
      override def description(formatter: Formatter): Nothing = ???
    }
    val c = new ArgumentFlag[Set[String]] {
      override def name = "c"
      override def parseValue(args: Set[String]): Set[String] = args
      override def argDescription: Nothing = ???
      override def description(formatter: Formatter): Nothing = ???
    }

    val args = "--a abc --b abc --a def,ghi,jkl,,".split(" ")
    val options = Options(List(a, b, c), None, args)

    options(a) shouldBe Set("abc", "def", "ghi", "jkl")
    options(b) shouldBe Set("abc")
    options(c) shouldBe Set()
  }


  it should "parse optional argument flags" in {
    val a = new OptionalArgumentFlag[Set[String]] {
      override def name = "a"
      override def defaultArg = "ABC"
      override def parseValue(args: Set[String]): Set[String] = args
      override def isValidArg(arg: String): Boolean = arg.startsWith("a")
      override def argDescription: Nothing = ???
      override def description(formatter: Formatter): Nothing = ???
    }

    val b = new OptionalArgumentFlag[Set[String]] {
      override def name = "b"
      override def defaultArg = "DEF"
      override def parseValue(args: Set[String]): Set[String] = args
      override def isValidArg(arg: String): Boolean = arg.startsWith("a")
      override def argDescription: Nothing = ???
      override def description(formatter: Formatter): Nothing = ???
    }

    val c = new OptionalArgumentFlag[Set[String]] {
      override def name = "c"
      override def defaultArg = "GHI"
      override def parseValue(args: Set[String]): Set[String] = args
      override def isValidArg(arg: String): Boolean = arg.startsWith("c")
      override def argDescription: Nothing = ???
      override def description(formatter: Formatter): Nothing = ???
    }

    val args = "--a abc --b --a abcdef".split(" ")
    val options = Options(List(a, b, c), None, args)

    options(a) shouldBe Set("abc", "abcdef")
    options(b) shouldBe Set("DEF")
    options(c) shouldBe Set()

  }


  it should "parse number flags" in {
    val a = new NumberFlag {
      override def name = "a"
      override def defaultValue = 123
      override def description(formatter: Formatter): Nothing = ???
    }
    val b = new NumberFlag {
      override def name = "b"
      override def defaultValue = 456
      override def description(formatter: Formatter): Nothing = ???
    }
    val c = new NumberFlag {
      override def name = "c"
      override def defaultValue = 789
      override def description(formatter: Formatter): Nothing = ???
    }

    val args = "--a 1 --b 1 --a 1337".split(" ")
    val options = Options(List(a, b, c), None, args)

    options(a) shouldBe 1337
    options(b) shouldBe 1
    options(c) shouldBe 789
  }


  it should "parse dictionary flags" in {
    val a = new DictionaryFlag[Map[String, String]] {
      override def name = "a"
      override def parseValue(args: Map[String, String]): Map[String, String] = args
      override def description(formatter: Formatter): Nothing = ???
      override def argDescription: Nothing = ???
    }
    val b = new DictionaryFlag[Map[String, String]] {
      override def name = "b"
      override def parseValue(args: Map[String, String]): Map[String, String] = args
      override def description(formatter: Formatter): Nothing = ???
      override def argDescription: Nothing = ???
    }
    val c = new DictionaryFlag[Map[String, String]] {
      override def name = "c"
      override def parseValue(args: Map[String, String]): Map[String, String] = args
      override def description(formatter: Formatter): Nothing = ???
      override def argDescription: Nothing = ???
    }

    val args = "--a abc=a --b abc=b,def=b --a def=a".split(" ")
    val options = Options(List(a, b, c), None, args)
    options(a) shouldBe Map(
      "abc" -> "a",
      "def" -> "a"
    )
    options(b) shouldBe Map(
      "abc" -> "b",
      "def" -> "b"
    )
    options(c) shouldBe Map()
  }


  it should "parse flags in any order" in {
    val positionalArgument = new PositionalArgument[Set[String]]() {
      override def parseValue(args: Set[String]): Set[String] = args
    }

    val argFlag = new ArgumentFlag[Set[String]] {
      override def name = "arg"
      override def parseValue(args: Set[String]): Set[String] = args
      override def description(formatter: Formatter): Nothing = ???
      override def argDescription: Nothing = ???
    }

    val booleanFlag = new BooleanFlag {
      override def name = "bool"
      override def description(formatter: Formatter): Nothing = ???
    }

    val optionalArgFlag = new OptionalArgumentFlag[Set[String]] {
      override def name = "opt"
      override def parseValue(args: Set[String]): Set[String] = args
      override def defaultArg = "ABC"
      override def isValidArg(arg: String): Boolean = arg.startsWith("a")
      override def description(formatter: Formatter): Nothing = ???
      override def argDescription: Nothing = ???
    }

    val dictionaryFlag = new DictionaryFlag[Map[String, String]] {
      override def name = "dict"
      override def parseValue(args: Map[String, String]): Map[String, String] = args
      override def description(formatter: Formatter): Nothing = ???
      override def argDescription: Nothing = ???
    }

    val args = "--arg ABC ABC --opt DEF GHI --opt abc JKL --bool --dict a=b,b=c MNO --dict c=d --arg GHI".split(" ")
    val options = Options(List(argFlag, booleanFlag, optionalArgFlag, dictionaryFlag), Some(positionalArgument), args)

    options(positionalArgument) shouldBe Set("ABC", "DEF", "GHI", "JKL", "MNO")
    options(argFlag) shouldBe Set("ABC", "GHI")
    options(booleanFlag) shouldBe true
    options(optionalArgFlag) shouldBe Set("ABC", "abc")
    options(dictionaryFlag) shouldBe Map(
      "a" -> "b",
      "b" -> "c",
      "c" -> "d"
    )
  }

  it should "be case insensitive" in {
    val argFlag = new ArgumentFlag[Set[String]] {
      override def name = "arg"
      override def parseValue(args: Set[String]): Set[String] = args
      override def description(formatter: Formatter): Nothing = ???
      override def argDescription: Nothing = ???
    }

    val booleanFlag = new BooleanFlag {
      override def name = "bool"
      override def description(formatter: Formatter): Nothing = ???
    }

    val optionalArgFlag = new OptionalArgumentFlag[Set[String]] {
      override def name = "opt"
      override def parseValue(args: Set[String]): Set[String] = args
      override def defaultArg = "ABC"
      override def isValidArg(arg: String): Boolean = arg.startsWith("a")
      override def description(formatter: Formatter): Nothing = ???
      override def argDescription: Nothing = ???
    }

    val dictionaryFlag = new DictionaryFlag[Map[String, String]] {
      override def name = "dict"
      override def parseValue(args: Map[String, String]): Map[String, String] = args
      override def description(formatter: Formatter): Nothing = ???
      override def argDescription: Nothing = ???
    }

    val args = "--ArG abC --oPt aBc --booL --DiCt A=b,b=C --dICt A=d".split(" ")
    val options = Options(List(argFlag, booleanFlag, optionalArgFlag, dictionaryFlag), None, args)

    options(argFlag) shouldBe Set("abC")
    options(booleanFlag) shouldBe true
    options(optionalArgFlag) shouldBe Set("aBc")
    options(dictionaryFlag) shouldBe Map(
      "a" -> "d",
      "b" -> "C"
    )
  }


  it should "use short flag" in {
    val argFlag = new ArgumentFlag[Set[String]] {
      override def name = "arg"
      override def shortFlag: Option[String] = Some("a")
      override def parseValue(args: Set[String]): Set[String] = args
      override def description(formatter: Formatter): Nothing = ???
      override def argDescription: Nothing = ???
    }

    val args = "--arg ABC -a DEF".split(" ")
    val options = Options(List(argFlag), None, args)

    options(argFlag) shouldBe Set("ABC", "DEF")
  }


  it should "remove duplicates" in {
    val positionalArgument = new PositionalArgument[Set[String]]() {
      override def parseValue(args: Set[String]): Set[String] = args
    }

    val argFlag = new ArgumentFlag[Set[String]] {
      override def name = "arg"
      override def parseValue(args: Set[String]): Set[String] = args
      override def description(formatter: Formatter): Nothing = ???
      override def argDescription: Nothing = ???
    }

    val args = "--arg ABC --arg ABC ABC ABC ABC".split(" ")
    val options = Options(List(argFlag), Some(positionalArgument), args)

    options(argFlag) shouldBe Set("ABC")
    options(positionalArgument) shouldBe Set("ABC")
  }


  it should "throw when given an invalid flag" in {
    val a = new BooleanFlag {
      override def name = "a"
      override def description(formatter: Formatter): Nothing = ???
    }
    val b = new BooleanFlag {
      override def name = "b"
      override def description(formatter: Formatter): Nothing = ???
    }
    val c = new BooleanFlag {
      override def name = "c"
      override def description(formatter: Formatter): Nothing = ???
    }
    val args = "--a --b --d".split(" ")
    intercept[IllegalArgumentException] { Options(List(a, b, c), None, args) }
      .getMessage should include("--d")

  }


  it should "throw when given an invalid argument to a dictionary flag" in {
    val flags = List(
      new DictionaryFlag[Map[String, String]] {
        override def name = "a"
        override def parseValue(args: Map[String, String]): Map[String, String] = args
        override def description(formatter: Formatter): Nothing = ???
        override def argDescription: Nothing = ???
      }
    )

    intercept[IllegalArgumentException] { Options(flags, None, "--a abc".split(" ")) }
      .getMessage should include("abc")

    intercept[IllegalArgumentException] { Options(flags, None, "--a abc=1,def=2,ghi".split(" ")) }
      .getMessage should include("ghi")

    intercept[IllegalArgumentException] { Options(flags, None, "--a abc=,def=2".split(" ")) }
      .getMessage should include("abc")
  }

  it should "throw when given an invalid argument to a number flag" in {
    val flags = List(
      new NumberFlag {
        override def name = "a"
        override def description(formatter: Formatter): Nothing = ???
        override def defaultValue = 1
      }
    )

    intercept[IllegalArgumentException] { Options(flags, None, "--a abc".split(" ")) }
      .getMessage should include("abc")

    intercept[IllegalArgumentException] { Options(flags, None, "--a 1234567891011121314151617181920".split(" ")) }
      .getMessage should include("1234567891011121314151617181920")
  }

}
