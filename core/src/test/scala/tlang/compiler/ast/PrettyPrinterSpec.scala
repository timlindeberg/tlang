package tlang.compiler.ast

import java.io.File

import org.scalatest.{FunSuite, Matchers}
import tlang.Context
import tlang.compiler.Tester
import tlang.compiler.error.CompilationException
import tlang.compiler.lexer.Lexer
import tlang.utils.formatting.SimpleFormatting
import tlang.utils.{FileSource, StringSource}

class PrettyPrinterSpec extends FunSuite with Matchers {

  private val TestFile   : File    = new File(Tester.Resources + "positions/ParserPositions.t")
  private val TestContext: Context = Tester.getTestContext(Some(TestFile))

  test("Tree is the same after being pretty printed and reparsed") {
    val file = FileSource(TestFile) :: Nil

    val parser = (Lexer andThen Parser).execute(TestContext) _
    val prettyPrinter = PrettyPrinter(SimpleFormatting)

    val CU = try {
      parser(file).head
    } catch {
      case e: CompilationException =>
        fail(s"Could not parse file ${ TestFile.getName }: \n" + e.messages.formattedErrors)
    }

    val printedCU = prettyPrinter(CU)
    val reparsedCU = try {
      parser(StringSource(printedCU, "ParserPositions") :: Nil).head
    } catch {
      case e: CompilationException =>
        fail(
          s"""
             |Could not reparse output from file ${ TestFile.getName }:
             |${ e.messages.formattedErrors }
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