package tlang.options

import better.files.File
import tlang.compiler.imports.Imports
import tlang.formatting._
import tlang.messages.{AlternativeSuggestor, ErrorStringContext, Suggestion}
import tlang.options.arguments._
import tlang.testutils.UnitSpec
import tlang.utils.LogLevel

class CompilerArgumentsSpec extends UnitSpec {


  behavior of "Compiler arguments"


  it should "use class path flag" in {
    test("No arguments") {
      val options = createOptions("")
      options(ClassPathFlag) shouldBe Set()
    }

    test("With arguments") {
      val options = createOptions("--classpath core,macros --classpath stdlib")
      options(ClassPathFlag) shouldBe Set("core", "macros", "stdlib")
    }

    test("Invalid directory") {
      intercept[IllegalArgumentException] { createOptions("--classpath /INVALIDPATH") }
        .getMessage should include("/INVALIDPATH")

      intercept[IllegalArgumentException] { createOptions("--classpath 12345") }
        .getMessage should include("12345")
    }
  }


  it should "use color scheme flag" in {
    import tlang.formatting.Colors._
    test("Default color scheme") {
      val options = createOptions("")
      val colorScheme = options(ColorSchemeFlag)
      colorScheme.Keyword shouldBe BLUE
      colorScheme.Variable shouldBe CYAN
      colorScheme.Class shouldBe GREEN
      colorScheme.Method shouldBe YELLOW
      colorScheme.String shouldBe YELLOW
      colorScheme.Number shouldBe MAGENTA
      colorScheme.Comment shouldBe BLACK
      colorScheme.Symbol shouldBe WHITE
    }

    test("Custom color scheme") {
      val options = createOptions(
        "--colorscheme keyword=red,variable=1,class=4,method=magenta,string=green,number=bold,comment=underlined,symbol=black"
      )
      val colorScheme = options(ColorSchemeFlag)
      colorScheme.Keyword shouldBe RED
      colorScheme.Variable shouldBe BOLD
      colorScheme.Class shouldBe UNDERLINED
      colorScheme.Method shouldBe MAGENTA
      colorScheme.String shouldBe GREEN
      colorScheme.Number shouldBe BOLD
      colorScheme.Comment shouldBe UNDERLINED
      colorScheme.Symbol shouldBe BLACK
    }

    test("Invalid color key") {
      val suggestor = mock[AlternativeSuggestor]
      suggestor.apply("beyword", ColorScheme.Keys) returns Suggestion(List("Did you mean 'keyword'?"))

      val error = intercept[IllegalArgumentException] { createOptions("--colorscheme beyword=red", suggestor) }
      error.getMessage should include("beyword")
      error.getMessage should include("Did you mean 'keyword'?")
    }

    test("Invalid color") {
      val suggestor = mock[AlternativeSuggestor]
      suggestor.apply("screen", Colors.ColorNames) returns Suggestion(List("Did you mean 'green'?"))

      val error = intercept[IllegalArgumentException] { createOptions("--colorscheme keyword=screen", suggestor) }
      error.getMessage should include("screen")
      error.getMessage should include("Did you mean 'green'?")
    }
  }


  it should "use directory flag" in {
    test("No arguments should return PWD") {
      val options = createOptions("")
      options(DirectoryFlag) shouldBe Set(File("."))
    }

    test("With arguments") {
      val options = createOptions("--directory core,macros -d stdlib")
      options(DirectoryFlag) shouldBe Set(File("core"), File("macros"), File("stdlib"))
    }

    test("Invalid directory") {
      intercept[IllegalArgumentException] { createOptions("--directory /INVALIDPATH") }
        .getMessage should include("/INVALIDPATH")

      intercept[IllegalArgumentException] { createOptions("-d 12345") }
        .getMessage should include("12345")
    }
  }


  it should "use help flag" in {
    test("No arguments should be empty") {
      val options = createOptions("")
      options(CompilerHelpFlag) shouldBe Set()
    }

    test("Default argument") {
      val options = createOptions("--help")
      options(CompilerHelpFlag) shouldBe Set("all")
    }

    test("With arguments") {
      val options = createOptions("--help phases,colorscheme --help exec")
      options(CompilerHelpFlag) shouldBe Set("phases", "colorscheme", "exec")
    }

    test("Invalid argument should result in default argument") {
      val options = createOptions("--help --phases")
      options(CompilerHelpFlag) shouldBe Set("all")
      options(PhasesFlag) shouldBe true
    }
  }


  it should "use ignore default imports flag" in {
    test("No arguments should be empty") {
      val options = createOptions("")
      options(IgnoreDefaultImportsFlag) shouldBe Set()
    }

    test("With arguments") {
      val options = createOptions("--ignoreimport java::lang::object,T::LaNG::Int --ignoreimport t::lang::bool")
      options(IgnoreDefaultImportsFlag) shouldBe Set("java::lang::Object", "T::lang::Int", "T::lang::Bool")
    }

    test("Invalid arguments") {
      val suggestor = mock[AlternativeSuggestor]
      suggestor.apply("java::Lang::obdect", Imports.DefaultImportNames) returns Suggestion(List("Did you mean 'java::lang::Object'?"))

      val error = intercept[IllegalArgumentException] { createOptions("--ignoreimport java::Lang::obdect", suggestor) }
      error.getMessage should include("java::Lang::obdect")
      error.getMessage should include("Did you mean 'java::lang::Object'?")
    }
  }


  it should "use line width flag" in {
    test("No arguments should give default width if not connected to a terminal") {
      // The default value is actually -1 which results in the width of the terminal window
      // if connected to a terminal or 120 otherwise. It's kind of hard to test though.
      val options = createOptions("")
      options(LineWidthFlag) shouldBe LineWidthFlag.DefaultWidth
    }

    test("With arguments should pick largest value") {
      val options = createOptions("--linewidth 5,10 --linewidth 25")
      options(LineWidthFlag) shouldBe 25
    }

    test("Invalid argument") {
      intercept[IllegalArgumentException] { createOptions("--linewidth abc") }
        .getMessage should include("abc")

      intercept[IllegalArgumentException] { createOptions("--linewidth -5") }
        .getMessage should include("-5")
    }
  }


  it should "use max errors flag" in {
    test("No arguments should give -1") {
      val options = createOptions("")
      options(MaxErrorsFlag) shouldBe MaxErrorsFlag.defaultValue
    }

    test("With arguments should pick largest value") {
      val options = createOptions("--maxerrors 5,10 --maxerrors 25")
      options(MaxErrorsFlag) shouldBe 25
    }

    test("Invalid argument") {
      intercept[IllegalArgumentException] { createOptions("--maxerrors abc") }
        .getMessage should include("abc")
    }
  }


  it should "use message context flag" in {
    test("No arguments should give default number of context lines") {
      val options = createOptions("")
      options(MessageContextFlag) shouldBe MessageContextFlag.defaultValue
    }

    test("With arguments should pick largest value") {
      val options = createOptions("--messagecontext 5,10 --messagecontext 25")
      options(MessageContextFlag) shouldBe 25
    }

    test("Invalid argument") {
      intercept[IllegalArgumentException] { createOptions("--messagecontext abc") }
        .getMessage should include("abc")

      intercept[IllegalArgumentException] { createOptions("--messagecontext -5") }
        .getMessage should include("-5")
    }
  }


  it should "use print output flag" in {
    test("No arguments should be empty") {
      val options = createOptions("")
      options(PrintOutputFlag) shouldBe Set()
    }

    test("Default argument") {
      val options = createOptions("--printoutput")
      options(PrintOutputFlag) shouldBe Set("lowering")
    }

    test("With arguments") {
      val options = createOptions("--printoutput lowering,codegeneration --printoutput typing")
      options(PrintOutputFlag) shouldBe Set("lowering", "codegeneration", "typing")
    }

    test("Invalid argument should result in default argument") {
      val options = createOptions("--printoutput --phases")
      options(PrintOutputFlag) shouldBe Set("lowering")
      options(PhasesFlag) shouldBe true
    }
  }


  it should "use tfiles argument" in {
    test("No arguments should result in no files") {
      val options = createOptions("")
      options(TFilesArgument) shouldBe Set()
    }

    test("Valid files") {
      val options = createOptions(
        "core/src/test/resources/positions/LexerPositions.t core/src/test/resources/errortests/flowing/DeadCode.t"
      )
      options(TFilesArgument) shouldBe Set(
        File("core/src/test/resources/positions/LexerPositions.t"),
        File("core/src/test/resources/errortests/flowing/DeadCode.t")
      )
    }

    test("Valid directories") {
      val options = createOptions(
        "core/src/test/resources/positions core/src/test/resources/errortests/templating"
      )
      options(TFilesArgument) shouldBe Set(
        File("core/src/test/resources/positions/LexerPositions.t"),
        File("core/src/test/resources/positions/ParserPositions.t"),
        File("core/src/test/resources/errortests/templating/GenericParameterDuplicate.t"),
        File("core/src/test/resources/errortests/templating/NoSuchClass.t"),
        File("core/src/test/resources/errortests/templating/WrongNumParams.t")
      )
    }

    test("Not a T-file") {
      intercept[IllegalArgumentException] { createOptions("core/src/main/scala/tlang/Constants.scala") }
        .getMessage should include("Constants.scala")
    }


    test("Directory without T-Files") {
      intercept[IllegalArgumentException] { createOptions("core") }
        .getMessage should include("core")
    }


    test("Non existant file") {
      intercept[IllegalArgumentException] { createOptions("dasdasd") }
        .getMessage should include("dasdasd")
    }

  }

  it should "use loglevel argument" in {
    test("No arguments should result in loglevel off") {
      val options = createOptions("")
      options(LogLevelFlag) shouldBe LogLevel.Off
    }

    test("Valid log levels") {
      createOptions("--loglevel Off")(LogLevelFlag) shouldBe LogLevel.Off
      createOptions("--loglevel tRaCe")(LogLevelFlag) shouldBe LogLevel.Trace
      createOptions("--loglevel debug")(LogLevelFlag) shouldBe LogLevel.Debug
      createOptions("--loglevel info")(LogLevelFlag) shouldBe LogLevel.Info
      createOptions("--loglevel WARN")(LogLevelFlag) shouldBe LogLevel.Warn
      createOptions("--loglevel error")(LogLevelFlag) shouldBe LogLevel.Error
    }

    test("Invalid log levels") {
      intercept[IllegalArgumentException] { createOptions("--loglevel abc") }
        .getMessage should include("abc")
    }
  }


  private def createOptions(args: String, suggestor: AlternativeSuggestor = mock[AlternativeSuggestor]) = {
    val errorContext = ErrorStringContext(Formatter(SimpleFormatting), suggestor)
    val flags = tlang.compiler.Main.CompilerFlags
    Options(flags, Some(TFilesArgument), args.split(" "))(errorContext)
  }

}
