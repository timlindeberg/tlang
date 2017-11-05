package tlang.testutils

import better.files._
import org.scalatest.matchers.{MatchResult, Matcher}
import org.scalatest.{BeforeAndAfterAll, Status, Suite}
import tlang.formatting.Colors
import tlang.formatting.Colors.Color
import tlang.utils.Extensions._

import scala.collection.mutable
import scala.util.matching.Regex

object SnapshotTesting {

  val UpdateSnapshotsKey: String                   = "updateSnapshots"
  val UpdateSnapshots   : String                   = sys.env.getOrElse(UpdateSnapshotsKey, "")
  val UpdateRegex       : Regex                    = if (UpdateSnapshots.isEmpty) new Regex("\\b\\B") else UpdateSnapshots.r
  val SnapshotIndex     : mutable.Map[String, Int] = mutable.Map().withDefaultValue(0)
  val Directory         : String                   = TestConstants.Resources + "/snapshots"
  val Extension         : String                   = ".snap"

}

trait SnapshotTesting extends Suite with BeforeAndAfterAll {

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
    var list = _localTestNames.get()
    if (list == null)
      list = Nil
    list = description :: list

    _localTestNames.set(list)
    val u = f
    _localTestNames.set(list.tail)
    u
  }

  def snapshotName: String = {
    val testName = getTestName
    val index = SnapshotIndex(testName)

    val postfix = if (index == 1) "" else s" $index"

    testName + postfix
  }

  def matchSnapshot: SnapshotMatcher = {
    SnapshotIndex(getTestName) += 1
    new SnapshotMatcher(snapshotName)
  }


  protected override def runTest(testName: String, args: org.scalatest.Args): Status = {
    _currentTestName.set(testName)
    val status = super.runTest(testName, args)
    _currentTestName.set(null)
    status
  }


  private def getTestName = {
    val localTestNames = _localTestNames.get()
    val localName = if (localTestNames == null || localTestNames.isEmpty) "" else " " + localTestNames.reverse.mkString(" ")
    _currentTestName.get() + localName
  }


  class SnapshotMatcher(snapshotName: String) extends Matcher[String] {

    private case class Result(isSuccess: Boolean, message: String)

    private val testColor: Color    = Colors.Magenta
    private val coloredSnapshotName = "'" + testColor(snapshotName) + "'"

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
          s"""|No existing snapshot for test $coloredSnapshotName, creating a new one.
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
          s"$coloredSnapshotName matches '${ testColor(UpdateSnapshots) }', updating existing snapshot."
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
        var failure = s"$coloredSnapshotName failed, the new snapshot did not match the existing snapshot."
        if (UpdateSnapshots.nonEmpty)
          failure += s"\nTest name did not match '${ testColor(UpdateSnapshots) }'."
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
              |   $UpdateSnapshotsKey=${ testColor(snapshotName) }
              |You can also use regexes to match any number of snapshots:
              |   $UpdateSnapshotsKey=${ testColor(".*") }
              |   $UpdateSnapshotsKey=${ testColor("MySpec/.*") }
              |This will update the matching snapshots in the executed tests.
            """.stripMargin
        )
    }
  }


}
