package tlang.utils

trait CanHaveError {
  var hasError: Boolean = false
  def setError(): this.type = {
    hasError = true
    this
  }
}
