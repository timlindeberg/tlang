package tlang
package compiler
package ast

import better.files.File
import tlang.compiler.lexer.Lexing
import tlang.compiler.messages.{CompilationException, MessageType}
import tlang.compiler.output.ErrorMessageOutput
import tlang.utils.{FileSource, StringSource}

class PrettyPrinterSpec extends CompilerIntegrationTestSpec {

  import tlang.testutils.TestConstants._

  private val TestFile: File = File(s"$Resources/positions/ParserPositions.t")
  private val testContext: Context = testContext(Some(TestFile))

  "A pretty printer should " - {
    "produce the same tree after being pretty printed and reparsed" in {
      val file = FileSource(TestFile) :: Nil

      val parser = (Lexing andThen Parsing).execute(testContext) _
      val prettyPrinter = PrettyPrinter()

      val CU = try parser(file).head
      catch {
        case e: CompilationException =>
          val errors = ErrorMessageOutput(e.messages, messageTypes = List(MessageType.Error))
          fail(s"Could not parse file $TestFile:" + NL + errors.pretty)
      }

      val printedCU = prettyPrinter(CU)

      val reparsedCU = try parser(StringSource(printedCU, "ParserPositions") :: Nil).head
      catch {
        case e: CompilationException =>
          val errors = ErrorMessageOutput(e.messages, messageTypes = List(MessageType.Error))
          fail(
            s"""
               |Could not reparse output from file $TestFile:
               |${ errors }
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
