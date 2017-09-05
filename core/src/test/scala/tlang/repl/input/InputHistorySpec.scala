package tlang.repl.input

import better.files.File
import tlang.testutils.UnitSpec

class InputHistorySpec extends UnitSpec {

  behavior of "Input history"

  it should "create a new history file if it does not exist" in {
    val historyFile = mock[File]
    historyFile.exists returns false

    val inputHistory = InputHistory(historyFile, 1, 1)

    there.was.one(historyFile).createIfNotExists()
  }

  it should "save input history to an empty file" in { pending }

  it should "load input history from file" in { pending }

  it should "append input history to file" in { pending }

  it should "be able to go back and forth between inputs" in { pending }

  it should "have an empty input buffer when starting" in { pending }

}
