package tlang
package testutils
package snapshot

import better.files._
import org.scalatest._
import org.scalatest.matchers.{MatchResult, Matcher}
import tlang.formatting.Colors
import tlang.formatting.Colors.Color

import scala.collection.mutable
import scala.util.matching.Regex

object SnapshotTestingLike {

  val UpdateSnapshotsKey: String = "updateSnapshots"
  val RemoveSnapshotsKey: String = "removeUnusedSnapshots"
  val UpdateSnapshots: String = sys.env.getOrElse(UpdateSnapshotsKey, "")
  val RemoveUnusedSnapshots: Boolean = sys.env.get(RemoveSnapshotsKey).contains("true")
  val UpdateRegex: Regex = if (UpdateSnapshots.isEmpty) new Regex("\\b\\B") else UpdateSnapshots.r
  val SnapshotIndex: mutable.Map[String, Int] = mutable.Map().withDefaultValue(0)
  val Directory: String = TestConstants.Resources + "/snapshots"
  val Extension: String = ".snap"
  val LocalNameSeparator: String = " "
  val Separator: String = "---------------------------------------------------------------------------------------------------------"

  private val HighlightColor: Color = Colors.Magenta
  private val FailColor: Color = Colors.Red + Colors.Bold
}

class UnusedSnapshotsException(msg: String) extends Throwable(msg)

trait SnapshotTestingLike extends Suite with BeforeAndAfterAll {

  import SnapshotTestingLike._

  private val snapshots: Snapshots = {
    val parts = getClass.getName.split("\\.")
    val dir = parts.dropRight(1).mkString("/")
    val fileName = parts.last

    val file = Directory / dir / (fileName + Extension)
    Snapshots(file)
  }

  private val _currentTestName: ThreadLocal[String] = new ThreadLocal[String]()
  private val _localTestNames: ThreadLocal[List[String]] = new ThreadLocal[List[String]]()

  def matchSnapshot: SnapshotMatcher = {
    SnapshotIndex(fullTestName) += 1
    new SnapshotMatcher(snapshotName)
  }

  // For readability mostly. Appends the description to the snapshot name if
  // a snapshot test is executed within the block
  def test[U](description: String, ignore: Boolean = false)(testFun: => U): Unit = {
    if (ignore)
      return

    withLocalName(description) { testFun }
  }

  def snapshotName: String = {
    val testName = fullTestName
    val index = SnapshotIndex(testName)

    val postfix = if (index == 1) "" else s" $index"

    testName + postfix
  }

  override def afterAll(): Unit = {
    super.afterAll()
    val outcome = checkForUnusedSnapshots()
    outcome match {
      case Failed(exception) => throw exception
      case _                 => snapshots.save()
    }
  }

  protected def currentTestName: String = _currentTestName.get()
  protected def currentTestName_=(name: Option[String]): Unit = _currentTestName.set(name.orNull)

  protected def localTestNames: List[String] = Option(_localTestNames.get()).getOrElse(Nil)
  protected def localTestNames_=(names: List[String]): Unit = _localTestNames.set(names)

  // Will return a failure outcome instead of the given outcome if the test
  // contained unused snapshots
  protected def checkForUnusedSnapshots(): Outcome = checkForUnusedSnapshots(None)
  protected def checkForUnusedSnapshots(testName: String): Outcome = checkForUnusedSnapshots(Some(testName))
  protected def checkForUnusedSnapshots(testName: Option[String]): Outcome = {
    val unusedSnapshots = snapshots.unused(testName)
    if (unusedSnapshots.isEmpty)
      return Succeeded

    def formatUnusedSnapshots: String = unusedSnapshots.map(HighlightColor(_)).mkString(NL)

    if (RemoveUnusedSnapshots) {
      println(
        s"""|Removing unused snapshots from ${ HighlightColor(getClass.getName) }:
            |
            |$formatUnusedSnapshots
       """.stripMargin
      )
      snapshots --= unusedSnapshots
      return Succeeded
    }

    val msg =
      s"""|${ FailColor(getClass.getName) } contains unused snapshots:
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

  private def withLocalName[U](name: String)(testFun: => U): Unit = {
    val testNames = name :: localTestNames
    localTestNames = testNames
    try { testFun } finally { localTestNames = testNames.tail }
  }

  private def fullTestName: String = {
    val local = localTestNames
    val localName = if (local.isEmpty) "" else local.reverse.mkString(LocalNameSeparator, LocalNameSeparator, "")
    currentTestName + localName
  }

  class SnapshotMatcher(snapshotName: String) extends Matcher[String] {

    private case class Result(isSuccess: Boolean, message: String, comparison: String)

    def apply(newSnapshot: String): MatchResult = {
      val Result(isSuccess, message, comparison) = matchSnapshot(newSnapshot)
      if (message.nonEmpty)
        println(message + comparison)

      val successMessage = if (isSuccess) message else ""
      val failureMessage = if (!isSuccess) message else ""
      MatchResult(matches = isSuccess, failureMessage.stripAnsi, successMessage.stripAnsi)
    }

    private def matchSnapshot(newSnapshot: String): Result = snapshots(snapshotName) match {
      case None                                                                                       =>
        snapshots += snapshotName -> newSnapshot
        val message =
          s"""|No existing snapshot for test ${ HighlightColor(snapshotName) }, creating a new one:
              |$Separator
              |${ newSnapshot.escapeMargin }
              |$Separator
           """.stripMargin
        Result(isSuccess = true, message, "")
      case Some(oldSnapshot) if newSnapshot == oldSnapshot                                            =>
        Result(isSuccess = true, "", "")
      case Some(oldSnapshot) if UpdateSnapshots == snapshotName || (UpdateRegex matches snapshotName) =>
        snapshots += snapshotName -> newSnapshot

        val message = if (UpdateSnapshots == snapshotName)
          s"Updating existing snapshot."
        else
          s"${ HighlightColor(snapshotName) } matches '${ HighlightColor(UpdateSnapshots) }', updating existing snapshot."
        Result(isSuccess = true, message, compareSnapshots(oldSnapshot, newSnapshot))
      case Some(oldSnapshot)                                                                          =>
        var message = s"Failure: ${ FailColor(snapshotName) }, the new snapshot did not match the existing snapshot."
        if (UpdateSnapshots.nonEmpty)
          message += s"\nTest name did not match '${ HighlightColor(UpdateSnapshots) }'."
        message +=
          s"""|
              |If the new snapshot is correct rerun the test with environment variable
              |   $UpdateSnapshotsKey=${ HighlightColor(snapshotName) }
              |You can also use regexes to match any number of snapshots:
              |   $UpdateSnapshotsKey=${ HighlightColor(".*") }
              |   $UpdateSnapshotsKey=${ HighlightColor("MySpec/.*") }
              |This will update the matching snapshots in the executed tests.
           """.stripMargin
        Result(isSuccess = false, message, compareSnapshots(oldSnapshot, newSnapshot))
    }

    private def compareSnapshots(oldSnapshot: String, newSnapshot: String): String = {
      s"""|
          |Existing snapshot:
          |$Separator
          |${ oldSnapshot.escapeMargin }
          |$Separator
          |
          |New snapshot:
          |$Separator
          |${ newSnapshot.escapeMargin }
          |$Separator
          |
          |Difference:
          |${ StringDifference(newSnapshot, oldSnapshot) }
       """.stripMargin
    }
  }

}
