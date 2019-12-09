package tlang
package testutils
package snapshot

import org.scalatest._

trait SnapshotTesting extends TestSuite with SnapshotTestingLike {

  override protected def withFixture(test: NoArgTest): Outcome = {
    currentTestName = Some(test.name)
    try {
      val outcome = super.withFixture(test)

      if (outcome.isFailed)
        return outcome

      checkForUnusedSnapshots(currentTestName)
    } finally {
      currentTestName = None
    }
  }
}
