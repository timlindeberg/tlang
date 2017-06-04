package tlang.compiler.ast

import tlang.compiler.ast.Trees._
import tlang.utils.formatting.Formatting

case class TreePrinter(formatting: Formatting, width: Int = 1) {

  import formatting._
  import formatting.box._

  private val seperator = "\n\n"
  private val indent    = List.fill(width)(' ')

  def apply(ts: Traversable[Tree]): String = ts.map(apply).mkString(seperator)
  def apply(t: Tree): String = {
    val sb = new StringBuilder
    val stack = List[Char]()
    printTree(t, sb, stack, first = true)
    sb.toString()
  }

  private def printTree(tree: Tree, sb: StringBuilder, stack: List[Char], first: Boolean): Unit = {
    val children = tree.children
    if (children.isEmpty) {
      sb ++= ─ + " " + formatTree(tree) + "\n"
      return
    }

    if (!first)
      sb ++= ┬ + " "

    sb ++= formatTree(tree) + "\n"
    for ((child, i) <- children.zipWithIndex) {
      sb ++= stack.reverseIterator.mkString("") + ((if (i == children.size - 1) └ else ├) + ─ * width)

      val c = if (i != children.size - 1) │.head else ' '

      printTree(child, sb, (indent :+ c) ::: stack, first = false)
    }
  }

  private def formatTree(t: Tree): String = {
    val content = t match {
      case c: CompilationUnit  => formatFileName(c.source.mainName)
      case p: Package          => VarColor(if (p.isEmpty) "None" else p.name)
      case i: Import           => ClassColor(i.writtenName)
      case v: VariableID       => VarColor(v.name)
      case m: MethodID         => MethodColor(m.name)
      case c: ClassID          => ClassColor(c.name)
      case s: StringLit        => StringColor('"' + s.value + '"')
      case c: CharLit          => StringColor(''' + c.value + ''')
      case n: NumberLiteral[_] => NumColor(n.value)
      case _                   => ""
    }
    Bold(t.getClass.getSimpleName) + (if (content.nonEmpty) Bold("(") + content + Bold(")") else "")
  }

}
