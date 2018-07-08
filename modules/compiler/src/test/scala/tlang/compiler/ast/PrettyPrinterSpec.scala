package tlang.compiler.ast

import better.files.File
import tlang.compiler.{CompilerIntegrationTestSpec, Context}
import tlang.compiler.lexer.Lexing
import tlang.compiler.messages.{CompilationException, MessageType}
import tlang.compiler.output.ErrorMessageOutput
import tlang.formatting.SimpleFormatting
import tlang.formatting.textformatters.TabReplacer
import tlang.testutils.TestConstants._
import tlang.utils.Extensions._
import tlang.utils.{FileSource, StringSource}

class PrettyPrinterSpec extends CompilerIntegrationTestSpec {


  private val TestFile   : File    = File(s"$Resources/positions/ParserPositions.t")
  private val testContext: Context = testContext(Some(TestFile))

  "A pretty printer should " - {
    "produce the same tree after being pretty printed and reparsed" in {
      val file = FileSource(TestFile) :: Nil

      val parser = (Lexing andThen Parsing).execute(testContext) _
      val prettyPrinter = PrettyPrinter(SimpleFormatting)

      val CU = try parser(file).head
      catch {
        case e: CompilationException =>
          val errors = ErrorMessageOutput(testContext.formatter, TabReplacer(2), e.messages, messageTypes = List(MessageType.Error))
          fail(s"Could not parse file $TestFile:" + NL + errors.pretty)
      }

      val printedCU = prettyPrinter(CU)

      val reparsedCU = try parser(StringSource(printedCU, "ParserPositions") :: Nil).head
      catch {
        case e: CompilationException =>
          val errors = ErrorMessageOutput(testContext.formatter, TabReplacer(2), e.messages, messageTypes = List(MessageType.Error))
          fail(
            s"""
               |Could not reparse output from file $TestFile:
               |${errors.pretty}
               |
               |Printed output:
               |$printedCU
           """.stripMargin
          )
      }

      CU shouldBe reparsedCU
      printedCU shouldBe prettyPrinter(reparsedCU)
    }
  }

}
