package tlang.compiler

import tlang.compiler.ast.Trees

/**
  * Created by Tim Lindeberg on 1/14/2017.
  */
class MacroSpec {

  def main(args: Array[String]): Unit = {
    println(new Trees.Copier())
  }
}
