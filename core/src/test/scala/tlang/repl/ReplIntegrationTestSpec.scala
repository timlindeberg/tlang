package tlang.repl

import akka.actor.ActorRef
import org.scalatest._
import tlang.formatting.DefaultFormatting
import tlang.options.Options
import tlang.repl.actors.ReplActor.{Start, Stop}
import tlang.testutils.AnsiMatchers

object ReplIntegrationTestSpec {

  val CaptureScreens: Boolean = sys.env.get("captureScreens").contains("true")

}

trait ReplIntegrationTestSpec extends AsyncFlatSpec with Matchers with AnsiMatchers with BeforeAndAfterAll {

  def Width = 80
  def Height = 300
  def TimeoutMilliseconds = 20000
  lazy val testTerminal: TestTerminal = new TestTerminal(Width, Height, TimeoutMilliseconds)

  private var repl: ActorRef = _

  override def beforeAll(): Unit = {
    super.beforeAll()
    val formatting = DefaultFormatting.copy(lineWidth = Width)
    repl = Main.createRepl(testTerminal, Options.Empty, formatting)
    repl ! Start
  }

  override def afterAll(): Unit = {
    super.afterAll()
    repl ! Stop
  }


  override def withFixture(test: NoArgAsyncTest): FutureOutcome = {
    _currentTestName.set(test.name)
    super.withFixture(test)
  }

  private val _currentTestName = new ThreadLocal[String]
  implicit protected def currentTestName: String = _currentTestName.get()


}
