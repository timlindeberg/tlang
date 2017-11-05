package tlang.testutils

import better.files._
import org.scalatest._
import org.scalatest.matchers.{MatchResult, Matcher}
import tlang.formatting.Colors
import tlang.formatting.Colors.Color
import tlang.utils.Extensions._

import scala.collection.mutable
import scala.util.matching.Regex

object SnapshotTesting {

  val UpdateSnapshotsKey   : String                   = "updateSnapshots"
  val RemoveSnapshotsKey   : String                   = "removeUnusedSnapshots"
  val UpdateSnapshots      : String                   = sys.env.getOrElse(UpdateSnapshotsKey, "")
  val RemoveUnusedSnapshots: Boolean                  = sys.env.get(RemoveSnapshotsKey).contains("true")
  val UpdateRegex          : Regex                    = if (UpdateSnapshots.isEmpty) new Regex("\\b\\B") else UpdateSnapshots.r
  val SnapshotIndex        : mutable.Map[String, Int] = mutable.Map().withDefaultValue(0)
  val Directory            : String                   = TestConstants.Resources + "/snapshots"
  val Extension            : String                   = ".snap"

  private val HighlightColor: Color = Colors.Magenta
  private val FailColor     : Color = Colors.Red + Colors.Bold

}

trait SnapshotTesting extends TestSuite with BeforeAndAfterAll {

  import SnapshotTesting._

  override def afterAll(): Unit = {
    super.afterAll()
    snapshots.save()
  }

  private val snapshots: Snapshots = {
    val parts = getClass.getName.split("\\.")
    val dir = parts.dropRight(1).mkString("/")
    val fileName = parts.last

    val file = Directory / dir / (fileName + Extension)
    Snapshots(file)
  }

  private val _currentTestName: ThreadLocal[String]       = new ThreadLocal[String]()
  private val _localTestNames : ThreadLocal[List[String]] = new ThreadLocal[List[String]]()

  // For readability mostly. Appends the description to the snapshot name if
  // a snapshot test is executed within the block
  def test[U](description: String)(f: => U): U = {
    val testNames = description :: Option(_localTestNames.get()).getOrElse(Nil)
    _localTestNames.set(testNames)
    try { f } finally { _localTestNames.set(testNames.tail) }
  }

  def snapshotName: String = {
    val testName = fullTestName
    val index = SnapshotIndex(testName)

    val postfix = if (index == 1) "" else s" $index"

    testName + postfix
  }

  def matchSnapshot: SnapshotMatcher = {
    SnapshotIndex(fullTestName) += 1
    new SnapshotMatcher(snapshotName)
  }

  override protected def withFixture(test: NoArgTest): Outcome = {
    _currentTestName.set(test.name)
    try {
      handleUnusedSnapshots(super.withFixture(test))
    }
    finally {
      _currentTestName.set(null)
    }
  }

  private def fullTestName = {
    val localTestNames = _localTestNames.get()
    val localName = if (localTestNames == null || localTestNames.isEmpty) "" else " " + localTestNames.reverse.mkString(" ")
    _currentTestName.get() + localName
  }

  private def handleUnusedSnapshots(outcome: Outcome): Outcome = {
    val unusedSnapshots = snapshots.unused(_currentTestName.get())
    if (unusedSnapshots.isEmpty)
      return outcome

    def formatUnusedSnapshots = unusedSnapshots.map(HighlightColor(_)).mkString(NL)

    val className = getClass.getName
    if (RemoveUnusedSnapshots) {
      println(
        s"""|Removing unused snapshots from ${ HighlightColor(className) }:
            |
            |$formatUnusedSnapshots
         """.stripMargin
      )
      unusedSnapshots.foreach(snapshots -= _)
      return outcome
    }

    val msg =
      s"""|${ FailColor(className) } contains unused snapshots:
          |
          |$formatUnusedSnapshots
          |
          |Rerun with environment variable
          |   $RemoveSnapshotsKey=${ HighlightColor("true") }
          |to remove all unused snapshots.
       """.stripMargin
    println(msg)
    Failed(msg.stripAnsi)
  }


  class SnapshotMatcher(snapshotName: String) extends Matcher[String] {

    private case class Result(isSuccess: Boolean, message: String)

    def apply(newSnapshot: String): MatchResult = {
      val Result(isSuccess, message) = matchSnapshot(newSnapshot)
      if (message.nonEmpty)
        println(message)

      val successMessage = if (isSuccess) message.stripAnsi else ""
      val failureMessage = if (!isSuccess) message.stripAnsi else ""
      MatchResult(matches = isSuccess, failureMessage, successMessage)
    }

    private def matchSnapshot(newSnapshot: String): Result = snapshots(snapshotName) match {
      case None                                                                                       =>
        snapshots += snapshotName -> newSnapshot
        Result(isSuccess = true,
          s"""|No existing snapshot for test ${ HighlightColor(snapshotName) }, creating a new one:
              |---------------------------------------------------------------------------------------------------------
              |$newSnapshot
              |---------------------------------------------------------------------------------------------------------
           """.stripMargin)
      case Some(oldSnapshot) if newSnapshot == oldSnapshot                                            =>
        Result(isSuccess = true, "")
      case Some(oldSnapshot) if UpdateSnapshots == snapshotName || (UpdateRegex matches snapshotName) =>
        snapshots += snapshotName -> newSnapshot

        val matches = if (UpdateSnapshots == snapshotName)
          s"Updating existing snapshot."
        else
          s"${ HighlightColor(snapshotName) } matches '${ HighlightColor(UpdateSnapshots) }', updating existing snapshot."
        Result(isSuccess = true,
          s"""|$matches
              |Old snapshot:
              |---------------------------------------------------------------------------------------------------------
              |$oldSnapshot
              |---------------------------------------------------------------------------------------------------------
              |
              |New snapshot:
              |---------------------------------------------------------------------------------------------------------
              |$newSnapshot
              |---------------------------------------------------------------------------------------------------------
           """.stripMargin)
      case Some(snapshot)                                                                             =>
        var failure = s"${ FailColor(snapshotName) } failed, the new snapshot did not match the existing snapshot."
        if (UpdateSnapshots.nonEmpty)
          failure += s"\nTest name did not match '${ HighlightColor(UpdateSnapshots) }'."
        Result(isSuccess = false,
          s"""|$failure
              |Existing snapshot:
              |---------------------------------------------------------------------------------------------------------
              |$snapshot
              |---------------------------------------------------------------------------------------------------------
              |
              |New snapshot:
              |---------------------------------------------------------------------------------------------------------
              |$newSnapshot
              |---------------------------------------------------------------------------------------------------------
              |
              |If the new display is correct rerun the test with environment variable
              |   $UpdateSnapshotsKey=${ HighlightColor(snapshotName) }
              |You can also use regexes to match any number of snapshots:
              |   $UpdateSnapshotsKey=${ HighlightColor(".*") }
              |   $UpdateSnapshotsKey=${ HighlightColor("MySpec/.*") }
              |This will update the matching snapshots in the executed tests.
            """.stripMargin
        )
    }
  }


}
