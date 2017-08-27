package tlang.testutils

import org.scalamock.function._
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}
import tlang.compiler.ast.{PrettyPrinter, TreePrinter}
import tlang.formatting.BoxStyles.{BoxStyle, Unicode}
import tlang.formatting.Colors.ColorScheme
import tlang.formatting.Colors.ColorScheme.DefaultColorScheme
import tlang.formatting._
import tlang.utils.Extensions._

trait UnitSpec extends FlatSpec with Matchers with AnsiMatchers with MockFactory {

  def mockCalls[R](f: MockFunction0[R], calls: R*) =
    calls.foreach { r => f.expects().returning(r) }

  def mockCalls[T1, R](f: MockFunction1[T1, R], calls: (T1, R)*) =
    calls.foreach { case (t1, r) => f.expects(t1).returning(r) }

  def mockCalls[T1, T2, R](f: MockFunction2[T1, T2, R], calls: ((T1, T2), R)*) =
    calls.foreach { case ((t1, t2), r) => f.expects(t1, t2).returning(r) }

  def mockCalls[T1, T2, T3, R](f: MockFunction3[T1, T2, T3, R], calls: ((T1, T2, T3), R)*) =
    calls.foreach { case ((t1, t2, t3), r) => f.expects(t1, t2, t3).returning(r) }

  def mockCalls[T1, T2, T3, T4, R](f: MockFunction4[T1, T2, T3, T4, R], calls: ((T1, T2, T3, T4), R)*) =
    calls.foreach { case ((t1, t2, t3, t4), r) => f.expects(t1, t2, t3, t4).returning(r) }

  def mockCalls[T1, T2, T3, T4, T5, R](f: MockFunction5[T1, T2, T3, T4, T5, R], calls: ((T1, T2, T3, T4, T5), R)*) =
    calls.foreach { case ((t1, t2, t3, t4, t5), r) => f.expects(t1, t2, t3, t4, t5).returning(r) }

  def mockCalls[T1, T2, T3, T4, T5, T6, R](f: MockFunction6[T1, T2, T3, T4, T5, T6, R], calls: ((T1, T2, T3, T4, T5, T6), R)*) =
    calls.foreach { case ((t1, t2, t3, t4, t5, t6), r) => f.expects(t1, t2, t3, t4, t5, t6).returning(r) }

  def mockCalls[T1, T2, T3, T4, T5, T6, T7, R](f: MockFunction7[T1, T2, T3, T4, T5, T6, T7, R], calls: ((T1, T2, T3, T4, T5, T6, T7), R)*) =
    calls.foreach { case ((t1, t2, t3, t4, t5, t6, t7), r) => f.expects(t1, t2, t3, t4, t5, t6, t7).returning(r) }

  def mockCalls[T1, T2, T3, T4, T5, T6, T7, T8, R](f: MockFunction8[T1, T2, T3, T4, T5, T6, T7, T8, R], calls: ((T1, T2, T3, T4, T5, T6, T7, T8), R)*) =
    calls.foreach { case ((t1, t2, t3, t4, t5, t6, t7, t8), r) => f.expects(t1, t2, t3, t4, t5, t6, t7, t8).returning(r) }

  // Add more if needed


  // For scoping and readability
  def test[U](description: String = "")(f: => U): U = f

  def createMockFormatting(
    width: Int = 80,
    boxStyle: BoxStyle = Unicode,
    useColor: Boolean = true,
    colorScheme: ColorScheme = DefaultColorScheme): Formatting = {
    Formatting(boxStyle, width, useColor = useColor, colorScheme = colorScheme)
  }

  def mockedWordWrapperReturningSameLine: WordWrapper = {
    mock[WordWrapper] use { wordWrapper =>
      (wordWrapper.apply _).expects(*, *).onCall { (line, _) => List(line) }.anyNumberOfTimes()
    }
  }

  def createMockFormatter(
    width: Int = 80,
    boxStyle: BoxStyle = Unicode,
    useColor: Boolean = true,
    formatting: Option[Formatting] = None,
    wordWrapper: WordWrapper = mock[WordWrapper],
    truncator: Truncator = mock[Truncator],
    prettyPrinter: PrettyPrinter = mock[PrettyPrinter],
    treePrinter: TreePrinter = mock[TreePrinter],
    syntaxHighlighter: SyntaxHighlighter = mock[SyntaxHighlighter],
    stackTraceHighlighter: StackTraceHighlighter = mock[StackTraceHighlighter]
  ): Formatter = {

    Formatter(
      formatting = formatting.getOrElse(createMockFormatting(width, boxStyle, useColor = useColor)),
      wordWrapper,
      truncator,
      prettyPrinter,
      treePrinter,
      syntaxHighlighter,
      stackTraceHighlighter
    )
  }

}
