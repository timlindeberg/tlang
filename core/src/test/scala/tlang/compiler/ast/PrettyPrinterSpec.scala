package tlang.compiler.ast

import java.io.File

import org.scalatest.{FlatSpec, Matchers}
import tlang.Context
import tlang.compiler.error.{CompilationException, MessageType}
import tlang.compiler.lexer.Lexing
import tlang.testutils.Tester
import tlang.utils.formatting.SimpleFormatting
import tlang.utils.{FileSource, StringSource}

class PrettyPrinterSpec extends FlatSpec with Matchers {

  private val TestFile   : File    = new File(Tester.Resources + "positions/ParserPositions.t")
  private val TestContext: Context = Tester.getTestContext(Some(TestFile))

  it should "produce the same tree after being pretty printed and reparsed" in {
    val file = FileSource(TestFile) :: Nil

    val parser = (Lexing andThen Parsing).execute(TestContext) _
    val prettyPrinter = PrettyPrinter(SimpleFormatting)

    val CU = try {
      parser(file).head
    } catch {
      case e: CompilationException =>
        fail(s"Could not parse file ${ TestFile.getName }: \n" + e.messages.formatMessages(MessageType.Error))
    }

    val printedCU = prettyPrinter(CU)
    val reparsedCU = try {
      parser(StringSource(printedCU, "ParserPositions") :: Nil).head
    } catch {
      case e: CompilationException =>
        fail(
          s"""
             |Could not reparse output from file ${ TestFile.getName }:
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
