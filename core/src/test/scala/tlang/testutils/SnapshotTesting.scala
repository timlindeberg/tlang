package tlang.testutils

import better.files._
import org.scalatest.matchers.{MatchResult, Matcher}
import org.scalatest.{Status, Suite}
import tlang.formatting.Colors
import tlang.formatting.Colors.Color
import tlang.utils.Extensions._

import scala.util.matching.Regex

object SnapshotTesting {

  val Directory         : String = TestConstants.Resources + "/snapshots"
  val Extension         : String = ".snapshot"
  val UpdateSnapshotsKey: String = "updateSnapshots"
  val UpdateSnapshots   : String = sys.env.getOrElse(UpdateSnapshotsKey, "")
  val UpdateRegex       : Regex  = if (UpdateSnapshots.isEmpty) new Regex("\\b\\B") else UpdateSnapshots.r

}

trait SnapshotTesting extends Suite {

  import SnapshotTesting._


  def snapshotName: String = getClass.getSimpleName + '/' + _currentTestName.get()
  def matchSnapshot = new SnapshotMatcher(snapshotName)


  private val _currentTestName: ThreadLocal[String] = new ThreadLocal[String]()

  protected override def runTest(testName: String, args: org.scalatest.Args): Status = {
    _currentTestName.set(testName)
    val status = super.runTest(testName, args)
    _currentTestName.set(null)
    status
  }


  class SnapshotMatcher(testName: String) extends Matcher[String] {


    private case class Result(isSuccess: Boolean, message: String)

    private val testColor   : Color  = Colors.Magenta
    private val testFileName: String = testName.replaceAll(" ", "_") + Extension

    private val snapshotFile: File = {
      (Directory / testFileName) use {
        _.parent.createIfNotExists(asDirectory = true, createParents = true)
      }
    }

    def apply(newSnapshot: String): MatchResult = {
      val Result(isSuccess, message) = matchSnapshot(newSnapshot)
      if (message.nonEmpty)
        println(message)

      val successMessage = if (isSuccess) message.stripAnsi else ""
      val failureMessage = if (!isSuccess) message.stripAnsi else ""
      MatchResult(matches = isSuccess, failureMessage, successMessage)
    }

    private def matchSnapshot(newSnapshot: String): Result = snapshot match {
      case None                                                                               =>
        saveSnapshot(newSnapshot)
        Result(isSuccess = true,
          s"""|No existing snapshot for test '${ testColor(testName) }', creating a new one.
              |$newSnapshot
           """.stripMargin)
      case Some(oldSnapshot) if newSnapshot == oldSnapshot                                    =>
        Result(isSuccess = true, "")
      case Some(oldSnapshot) if UpdateSnapshots == testName || (UpdateRegex matches testName) =>
        saveSnapshot(newSnapshot)
        val matches = if (UpdateSnapshots == testName)
          s"Updating existing snapshot."
        else
          s"${ testColor(testName) } matches ${ testColor(UpdateSnapshots) }, updating existing snapshot."
        Result(isSuccess = true,
          s"""|$matches
              |Old snapshot:
              |$oldSnapshot
              |
              |New snapshot:
              |$newSnapshot
           """.stripMargin)
      case Some(snapshot)                                                                     =>
        var failure = s"'${ testColor(testName) }' failed, the new snapshot did not match the existing snapshot."
        if (UpdateSnapshots.nonEmpty)
          failure += s"\nTest name did not match '${ testColor(UpdateSnapshots) }'."
        Result(isSuccess = false,
          s"""|$failure
              |Existing snapshot:
              |$snapshot
              |
              |New snapshot:
              |$newSnapshot
              |
              |
              |If the new display is correct rerun the test with environment variable
              |   $UpdateSnapshotsKey=${ testColor(testName) }
              |You can also use regexes to match any number of snapshots:
              |   $UpdateSnapshotsKey=.*
              |   $UpdateSnapshotsKey=MyTests/.*
            """.stripMargin
        )
    }


    private def snapshot: Option[String] = {
      val file = snapshotFile
      if (file.exists) Some(file.contentAsString) else None
    }
    private def saveSnapshot(snapshot: String): Unit = snapshotFile.write(snapshot)

  }


}
