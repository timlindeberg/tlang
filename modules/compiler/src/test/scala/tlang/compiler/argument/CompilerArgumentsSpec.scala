package tlang
package compiler
package argument

import java.util.concurrent.TimeUnit

import better.files.File
import tlang.compiler.imports.Imports
import tlang.formatting._
import tlang.options.Options
import tlang.options.argument._
import tlang.testutils.{TestConstants, UnitSpec}
import tlang.utils.{LogLevel, ParallellExecutor, SingleThreadExecutor}

import scala.concurrent.duration.Duration

class CompilerArgumentsSpec extends UnitSpec {

  behavior of "Compiler arguments"

  it should "use class path flag" in {
    test("No arguments") {
      val options = createOptions("")
      options(ClassPathFlag) shouldBe Set()
    }

    test("With arguments") {
      val options = createOptions("--classpath src,modules --classpath stdlib")
      options(ClassPathFlag) shouldBe Set("src", "modules", "stdlib")
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
      val options = createOptions("--directory src,modules -d stdlib")
      options(DirectoryFlag) shouldBe Set(File("src"), File("modules"), File("stdlib"))
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
      val options = createOptions("--help --exec")
      options(CompilerHelpFlag) shouldBe Set("all")
      options(ExecFlag) shouldBe true
    }
  }

  it should "use ignore default imports flag" in {
    test("No arguments should be empty") {
      val options = createOptions("")
      options(IgnoredDefaultImportsFlag) shouldBe Set()
    }

    test("With arguments") {
      val options = createOptions("--ignoreimport java::lang::object,T::LaNG::Int --ignoreimport t::lang::bool")
      options(IgnoredDefaultImportsFlag) shouldBe Set("java::lang::Object", "T::lang::Int", "T::lang::Bool")
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

    test("With arguments") {
      val options = createOptions("--printoutput lowering,codegeneration --printoutput typing")
      options(PrintOutputFlag) shouldBe Set("lowering", "codegeneration", "typing")
    }

    test("No arguments is invalid") {
      intercept[IllegalArgumentException] { createOptions("--printoutput") }
    }

    test("Invalid argument") {
      intercept[IllegalArgumentException] { createOptions("--printoutput abc") }
        .getMessage should include("abc")
    }

    test("Should show suggestions") {
      val suggestor = mock[AlternativeSuggestor]
      suggestor.apply(*, *) returns Suggestion(List("Did you mean 'ABC'?"))
      intercept[IllegalArgumentException] { createOptions("--printoutput abc", suggestor) }
        .getMessage should include("ABC")
    }
  }

  it should "use tfiles argument" in {
    test("No arguments should result in no files") {
      val options = createOptions("")
      options(TFilesArgument) shouldBe Set()
    }

    val resourcesFolder = TestConstants.Resources
    test("Valid files") {
      val options = createOptions(
        s"$resourcesFolder/positions/LexerPositions.t $resourcesFolder/errortests/flowing/DeadCode.t"
      )
      options(TFilesArgument) shouldBe Set(
        File(s"$resourcesFolder/positions/LexerPositions.t"),
        File(s"$resourcesFolder/errortests/flowing/DeadCode.t")
      )
    }

    test("Valid directories") {
      val options = createOptions(
        s"$resourcesFolder/positions $resourcesFolder/errortests/templating"
      )
      options(TFilesArgument) shouldBe Set(
        File(s"$resourcesFolder/positions/LexerPositions.t"),
        File(s"$resourcesFolder/positions/ParserPositions.t"),
        File(s"$resourcesFolder/errortests/templating/GenericParameterDuplicate.t"),
        File(s"$resourcesFolder/errortests/templating/NoSuchClass.t"),
        File(s"$resourcesFolder/errortests/templating/WrongNumParams.t")
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

  it should "use threads argument" in {
    test("No arguments should result in a SingleThreadExecutor") {
      val options = createOptions("")
      options(ThreadsFlag) shouldBe SingleThreadExecutor
    }

    test("Valid number of threads") {
      createOptions("--threads 0")(ThreadsFlag) shouldBe ParallellExecutor(Runtime.getRuntime.availableProcessors)
      createOptions("--threads 1")(ThreadsFlag) shouldBe SingleThreadExecutor
      createOptions("--threads 2")(ThreadsFlag) shouldBe ParallellExecutor(2)
      createOptions("--threads 8")(ThreadsFlag) shouldBe ParallellExecutor(8)
    }

    test("Invalid arguments") {
      intercept[IllegalArgumentException] { createOptions("--threads abc") }
        .getMessage should include("abc")

      intercept[IllegalArgumentException] { createOptions("--threads -1") }
        .getMessage should include("-1")
    }
  }

  it should "use tabwidth argument" in {
    test("No arguments should give default width") {
      val options = createOptions("")
      options(TabWidthFlag) shouldBe TabWidthFlag.defaultValue
    }

    test("With arguments should pick largest value") {
      val options = createOptions("--tabwidth 5,10 --tabwidth 25")
      options(TabWidthFlag) shouldBe 25
    }

    test("Invalid argument") {
      intercept[IllegalArgumentException] { createOptions("--tabwidth abc") }
        .getMessage should include("abc")

      intercept[IllegalArgumentException] { createOptions("--tabwidth -5") }
        .getMessage should include("-5")
    }
  }

  it should "use exec-timeout argument" in {
    test("No arguments should give default width") {
      val options = createOptions("")
      options(ExecTimeoutFlag) shouldBe Duration.Inf
    }

    test("With arguments should pick largest value") {
      val options = createOptions("--exec-timeout 5.0,10.1 --exec-timeout 25.25")
      options(ExecTimeoutFlag) shouldBe Duration(25.25, TimeUnit.SECONDS)
    }

    test("Invalid argument") {
      intercept[IllegalArgumentException] { createOptions("--exec-timeout abc") }
        .getMessage should include("abc")

      intercept[IllegalArgumentException] { createOptions("--exec-timeout -5") }
        .getMessage should include("-5")
    }
  }

  private def createOptions(args: String, suggestor: AlternativeSuggestor = mock[AlternativeSuggestor]) = {
    val errorContext = ErrorStringContext(suggestor)(Formatter.SimpleFormatter)
    val flags = tlang.compiler.Main.CompilerFlags
    Options(flags, Some(TFilesArgument), args.split(" "))(errorContext)
  }
}
