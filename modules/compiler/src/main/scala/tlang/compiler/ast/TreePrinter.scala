package tlang.compiler.ast

import java.lang.System.identityHashCode

import tlang.compiler.analyzer.Symbols.{ClassSymbol, ExtensionClassSymbol, FieldSymbol, MethodSymbol, OperatorSymbol, Symbol, Symbolic, VariableSymbol}
import tlang.compiler.analyzer.Types._
import tlang.compiler.ast.Trees._
import tlang.formatting.Colors.Color
import tlang.formatting.Formatter
import tlang.utils.Extensions._

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object TreePrinter {

  def idFunction(any: Any): Int = identityHashCode(any)
}

case class TreePrinter(idFunction: Any => Int = TreePrinter.idFunction, spacing: Int = 1)(implicit formatter: Formatter) {

  type TreePrinterRow = (String, String, String, String, String)

  import formatter.formatting._

  private var symbolId                                     = -1
  private var symbolMap: mutable.Map[Symbol, (Int, Color)] = _

  def apply(t: Tree): List[TreePrinterRow] = {
    val Indent = List.fill(spacing)(' ')
    val Continuation = Vertical.head :: Indent
    val Whitespace = ' ' :: Indent

    symbolId = 0
    symbolMap = new java.util.IdentityHashMap[Symbol, (Int, Color)].asScala

    var first = true
    val lines: ListBuffer[TreePrinterRow] = ListBuffer()
    val sb = new StringBuilder


    def printTree(tree: Tree, stack: List[Char]): Unit = {
      def addLine(): Unit = {

        val line = (Magenta(tree.line), sb.toString, reference(tree), symbolContent(tree), typeContent(tree))
        lines += line
        sb.clear()
      }

      val children = tree.children
      if (children.isEmpty) {
        sb ++= Horizontal + " " + formatTree(tree)
        addLine()
        return
      }

      if (first)
        first = false
      else
        sb ++= HorizontalDown + " "

      sb ++= formatTree(tree)
      addLine()
      children.zipWithIndex foreach { case (child, i) =>
        val last = i == children.size - 1
        sb ++= stack.mkString("")
        sb ++= (if (last) BottomLeft else VerticalRight) + (Horizontal * spacing)

        val c = if (last) Whitespace else Continuation

        printTree(child, stack ::: c)
      }
    }

    printTree(t, Nil)
    lines.toList
  }

  private def formatTree(tree: Tree): String = {
    val content = tree match {
      case c: CompilationUnit  => formatter.fileName(c.sourceDescription)
      case p: Package          => VarColor(if (p.isEmpty) "None" else p.name)
      case i: Import           => ClassColor(i.writtenName)
      case v: VariableID       => VarColor(v.name)
      case m: MethodID         => MethodColor(m.name)
      case c: ClassID          => ClassColor(c.name)
      case s: StringLit        => StringColor('"' + s.value.escape + '"')
      case c: CharLit          => StringColor("'" + c.value.toString.escape + "'")
      case n: NumberLiteral[_] => NumColor(n.value)
      case _                   => ""
    }
    val name = Bold(tree.getClass.getSimpleName)
    val contentString = if (content.nonEmpty) Bold("(") + content + Bold(")") else ""
    name + contentString
  }

  private def reference(tree: Tree): String = {
    val id = idFunction(tree)
    val color = AllColors(id % AllColors.length)
    color(id.toHexString.padTo(8, '0'))
  }

  private def typeContent(tree: Tree): String = tree match {
    case typed: Typed if typed.getType != TUntyped =>
      val tpe = typed.getType
      val color = typeColor(tpe)
      val s = tpe.toString
        .replaceAll(".*::", "")
        .replaceAll("\\$ERROR", "Error")

      color(s)
    case _: Typed                                  => Red(Bold(Cross))
    case _                                         => ""
  }


  private def symbolContent(tree: Tree): String = tree match {
    case symbolic: Symbolic[_] if symbolic.hasSymbol =>
      val symbol = symbolic.getSymbol
      val (id, color) = symbolMap.getOrElseUpdate(symbol, newSymbolFormatting)
      val fullColor = tree match {
        case _: MethodDeclTree | _: ClassDeclTree | _: VarDecl => Underline + color
        case _                                                 => color
      }
      val symbolLetter = getSymbolLetter(symbol)
      fullColor(symbolLetter + id)
    case _: Symbolic[_]                              => Red(Bold(Cross))
    case _                                           => ""
  }


  private def typeColor(tpe: Type): Color = tpe match {
    case TUnit                       => NoColor
    case TError                      => Bold + Red
    case Int | Long | Float | Double => NumColor
    case Char | String               => StringColor
    case Bool | TNull                => VarColor
    case _: TObject                  => ClassColor
    case TArray(t)                   => typeColor(t)
    case TUntyped                    => Red
  }

  private def getSymbolLetter(symbol: Symbol): String = symbol match {
    case _: ExtensionClassSymbol        => "E"
    case c: ClassSymbol if c.isAbstract => "T"
    case _: ClassSymbol                 => "C"
    case _: OperatorSymbol              => "O"
    case _: MethodSymbol                => "M"
    case _: FieldSymbol                 => "F"
    case _: VariableSymbol              => "V"
  }

  private def newSymbolFormatting: (Int, Color) = {
    val id = symbolId
    val colorIndex = id % FGColors.length
    symbolId += 1
    (id, FGColors(colorIndex))
  }


}
