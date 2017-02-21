package tlang.compiler.error

/**
  * Created by Tim Lindeberg on 2/18/2017.
  */
class CompilationException(val header: String, val message: String) extends Exception(message)
