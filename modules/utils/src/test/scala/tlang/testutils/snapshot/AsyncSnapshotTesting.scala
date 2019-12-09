package tlang
package testutils
package snapshot

import org.scalatest._

trait AsyncSnapshotTesting extends AsyncTestSuite with SnapshotTestingLike {

  override def withFixture(test: NoArgAsyncTest): FutureOutcome = {
    currentTestName = Some(test.name)
    super.withFixture(test)
      .change { outcome =>
        val realOutcome = if (outcome.isFailed) outcome else checkForUnusedSnapshots(currentTestName)
        currentTestName = None
        realOutcome
      }
  }
}
