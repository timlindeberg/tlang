package tlang.testutils

import better.files._
import org.scalatest.matchers.{MatchResult, Matcher}
import org.scalatest.{Status, Suite}
import tlang.formatting.Colors
import tlang.formatting.Colors.Color
import tlang.utils.Extensions._

import scala.collection.mutable
import scala.util.matching.Regex

object SnapshotTesting {

  val Directory         : String = TestConstants.Resources + "/snapshots"
  val Extension         : String = ".snapshot"
  val UpdateSnapshotsKey: String = "updateSnapshots"
  val UpdateSnapshots   : String = sys.env.getOrElse(UpdateSnapshotsKey, "")
  val UpdateRegex       : Regex  = if (UpdateSnapshots.isEmpty) new Regex("\\b\\B") else UpdateSnapshots.r

  val SnapshotIndex: mutable.Map[String, Int] = mutable.Map().withDefaultValue(0)

}

trait SnapshotTesting extends Suite {

  import SnapshotTesting._

  private val _currentTestName: ThreadLocal[String] = new ThreadLocal[String]()
  private val _localTestName  : ThreadLocal[String] = new ThreadLocal[String]()

  // For readability mostly. Appends the description to the snapshot name if
  // a snapshot test is executed within the block
  def test[U](description: String)(f: => U): U = {
    _localTestName.set(description)
    val u = f
    _localTestName.set(null)
    u
  }

  def snapshotName: String = {
    val testName = getTestName
    val index = SnapshotIndex(testName)

    val postfix = if (index == 1) "" else s" $index"

    val name = testName + postfix
    name.replaceAll(" ", "_")
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
    var localTestName = _localTestName.get()
    localTestName = if (localTestName == null) "" else s" $localTestName"
    getClass.getName.replaceAll("\\.", "/") + '/' + _currentTestName.get() + localTestName
  }


  class SnapshotMatcher(snapshotName: String) extends Matcher[String] {


    private case class Result(isSuccess: Boolean, message: String)

    private val testColor: Color    = Colors.Magenta
    private val coloredSnapshotName = "'" + testColor(snapshotName) + "'"

    private val snapshotFile: File = (Directory / (snapshotName + Extension)) use {
      _.parent.createIfNotExists(asDirectory = true, createParents = true)
    }

    def apply(newSnapshot: String): MatchResult = {
      val Result(isSuccess, message) = matchSnapshot(newSnapshot)
      if (message.nonEmpty)
        println(message)

      val successMessage = if (isSuccess) message.stripAnsi else ""
      val failureMessage = if (!isSuccess) message.stripAnsi else ""
      MatchResult(matches = isSuccess, failureMessage, successMessage)
    }

    private def matchSnapshot(newSnapshot: String): Result = readSnapshot match {
      case None                                                                                       =>
        saveSnapshot(newSnapshot)
        Result(isSuccess = true,
          s"""|No existing snapshot for test $coloredSnapshotName, creating a new one.
              |---------------------------------------------------------------------------------------------------------
              |$newSnapshot
              |---------------------------------------------------------------------------------------------------------
           """.stripMargin)
      case Some(oldSnapshot) if newSnapshot == oldSnapshot                                            =>
        Result(isSuccess = true, "")
      case Some(oldSnapshot) if UpdateSnapshots == snapshotName || (UpdateRegex matches snapshotName) =>
        saveSnapshot(newSnapshot)
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

    private def readSnapshot: Option[String] = {
      val file = snapshotFile
      if (file.exists) Some(file.contentAsString) else None
    }
    private def saveSnapshot(snapshot: String): Unit = snapshotFile.write(snapshot)

  }


}
