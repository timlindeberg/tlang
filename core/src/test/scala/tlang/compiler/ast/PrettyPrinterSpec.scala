package tlang.compiler.ast

import better.files.File
import tlang.Context
import tlang.compiler.CompilerIntegrationTestSpec
import tlang.compiler.lexer.Lexing
import tlang.formatting.SimpleFormatting
import tlang.messages.{CompilationException, MessageType}
import tlang.testutils.TestConstants._
import tlang.utils.Extensions._
import tlang.utils.{FileSource, StringSource}

class PrettyPrinterSpec extends CompilerIntegrationTestSpec {


  private val TestFile   : File    = File(s"$Resources/positions/ParserPositions.t")
  private val TestContext: Context = testContext(Some(TestFile))

  "A pretty printer should " - {
    "produce the same tree after being pretty printed and reparsed" in {
      val file = FileSource(TestFile) :: Nil

      val parser = (Lexing andThen Parsing).execute(TestContext) _
      val prettyPrinter = PrettyPrinter(SimpleFormatting)

      val CU = try {
        parser(file).head
      } catch {
        case e: CompilationException =>
          fail(s"Could not parse file $TestFile:" + NL + e.messages.formatMessages(MessageType.Error))
      }

      val printedCU = prettyPrinter(CU)
      val reparsedCU = try {
        parser(StringSource(printedCU, "ParserPositions") :: Nil).head
      } catch {
        case e: CompilationException =>
          fail(
            s"""
               |Could not reparse output from file $TestFile:
               |${ e.messages.formatMessages(MessageType.Error) }
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
