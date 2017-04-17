package tlang.compiler.ast

import java.io.File

import org.scalatest.{FunSuite, Matchers}
import tlang.Context
import tlang.compiler.Tester
import tlang.compiler.lexer.Lexer
import tlang.utils.formatting.SimpleFormatting
import tlang.utils.{FileSource, StringSource}

class PrettyPrinterSpec extends FunSuite with Matchers {

  private val TestFile   : String  = Tester.Resources + "positions/ParserPositions.t"
  private val TestContext: Context = Tester.testContext


  private val parser        = (Lexer andThen Parser).run(TestContext) _
  private val prettyPrinter = PrettyPrinter(SimpleFormatting)

  test("Tree is the same after being pretty printed and reparsed") {
    val file = FileSource(new File(TestFile)) :: Nil

    val CU = parser(file).head

    val printedCU = prettyPrinter(CU)
    val reparsedCU = parser(StringSource(printedCU, "ParserPositions") :: Nil).head

    CU shouldBe reparsedCU
    printedCU shouldBe prettyPrinter(reparsedCU)
  }

}