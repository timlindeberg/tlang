package tlang
package repl

import akka.actor.ActorRef
import com.googlecode.lanterna.input.{KeyStroke, KeyType}
import org.scalatest.{AsyncFlatSpec, BeforeAndAfterAll, Matchers}
import tlang.compiler.TestContext
import tlang.compiler.utils.TLangSyntaxHighlighter
import tlang.formatting.Formatter
import tlang.formatting.textformatters.SyntaxHighlighter
import tlang.options.Options
import tlang.repl.actors.ReplActor.{Start, Stop}
import tlang.testutils.AnsiMatchers
import tlang.testutils.snapshot.AsyncSnapshotTesting

class ReplIntegrationSpec extends AsyncFlatSpec
  with AsyncSnapshotTesting
  with Matchers
  with AnsiMatchers
  with BeforeAndAfterAll
  with TestContext {

  def Width = 80
  def Height = 300

  val testTerminal: TestTerminal = new TestTerminal(Width, Height)
  var repl: ActorRef = _

  override def beforeAll(): Unit = {
    super.beforeAll()
    implicit val formatter = Formatter.PrettyFormatter

    val ctx = testContext(None)(formatter)

    repl = ReplMain.createRepl(ctx, testTerminal, Options.Empty, killProcessOnTerminate = false)
    repl ! Start
  }

  override def afterAll(): Unit = {
    super.afterAll()
    repl ! Stop
  }

  // These tests are executed in order and can depend on the previous tests.
  // For instance the use existing variables test uses the calculation from
  // the test before

  behavior of "A repl"

  it should "execute simple commands" in {
    testTerminal
      .executeCommand("4 + 4")
      .stopWhen { display => display.contains("Result") }
      .display
      .map { _ should matchSnapshot }
  }

  it should "use existing variables" in {
    testTerminal
      .executeCommand("5 * res0")
      .stopWhen { display => display.contains("Result") }
      .display
      .map { _ should matchSnapshot }
  }

  it should "define and use classes" in {
    testTerminal
      .executeCommand(
        s"""|class A =
            |\tDef Times(t: Int) =
            |\tfor(var i = 0; i < t; i++)
            |\tprintln("A" + i)
            |""".stripMargin,
        KeyType.Backspace,
        KeyType.Backspace,
        KeyType.Backspace,
        "new A().Times(10)"
      )
      .stopWhen { display => display.contains("Result") }
      .display
      .map { _ should matchSnapshot }
  }

  it should "define and use functions" in {
    testTerminal
      .executeCommand(
        s"""|def Fun(a: Int, b: String) = b * a
            |
            |Fun(5, "ABC")
            |""".stripMargin
      )
      .stopWhen { display => display.contains("Result") }
      .display
      .map { _ should matchSnapshot }
  }

  it should "handle imports" in {
    testTerminal
      .executeCommand(
        s"""|import java::util::Date
            |
            |new Date(0)
            |""".stripMargin
      )
      .stopWhen { display => display.contains("Result") }
      .display
      .map { _ should matchSnapshot }
  }

  it should "show compilation errors" in {
    testTerminal
      .executeCommand(""""ABC" + a + res0 + b + "ABC"""")
      .stopWhen { display => display.contains("Error") }
      .display
      .map { _ should matchSnapshot }
  }

  it should "show stack traces on errors" in {
    testTerminal
      .executeCommand("""error("ABC")""")
      .stopWhen { display => display.contains("Error") }
      .display
      .map { _ should matchSnapshot }
  }

  it should "exit when pressing CTRL+C" in {
    testTerminal
      .executeCommand(new KeyStroke('c', true, false))
      .stopWhen { display => display.contains("Thanks") }
      .display
      .map { _ should matchSnapshot }
  }
}
