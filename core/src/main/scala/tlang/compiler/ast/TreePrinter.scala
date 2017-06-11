package tlang.compiler.ast

import tlang.compiler.analyzer.Symbols.{ClassSymbol, ExtensionClassSymbol, FieldSymbol, MethodSymbol, OperatorSymbol, Symbol, Symbolic, VariableSymbol}
import tlang.compiler.analyzer.Types._
import tlang.compiler.ast.Trees._
import tlang.utils.Extensions._
import tlang.utils.formatting.Colors.Color
import tlang.utils.formatting.Formatting

import scala.collection.JavaConverters._
import scala.collection.mutable

case class TreePrinter(formatting: Formatting, spacing: Int = 1) {

  import formatting._
  import formatting.box._

  private val Seperator    = "\n\n"
  private val Indent       = List.fill(spacing)(' ')
  private val Whitespace   = ' ' :: Indent
  private val Continuation = │.head :: Indent
  private val PaddingChar  = " "
  private val SymbolWidth  = 8
  private val Missing      = Some(Cross, Red + Bold)

  private var maxWidth                                     = -1
  private var symbolId                                     = -1
  private var symbolMap: mutable.Map[Symbol, (Int, Color)] = _

  var header: String = _

  def apply(ts: Traversable[Tree]): String = ts.map(apply).mkString(Seperator)
  def apply(t: Tree): String = {
    maxWidth = widthOf(t)
    symbolId = 0
    symbolMap = new java.util.IdentityHashMap[Symbol, (Int, Color)].asScala

    var first = true
    val sb = new StringBuilder

    header = Bold(s"%-${ maxWidth }s %-${ SymbolWidth }s %s").format("Tree", "Symbol", "Type")

    def printTree(tree: Tree, stack: List[Char]): Unit = {
      val children = tree.children
      val content = getContent(tree, stack)
      if (children.isEmpty) {
        sb ++= ─ + " " + formatTree(tree) + content + '\n'
        return
      }

      if (first)
        first = false
      else
        sb ++= ┬ + " "

      sb ++= formatTree(tree) + content + '\n'
      children.zipWithIndex foreach { case (child, i) =>
        val last = i == children.size - 1
        sb ++= stack.mkString("")
        sb ++= (if (last) └ else ├) + (─ * spacing)

        val c = if (last) Whitespace else Continuation

        printTree(child, stack ::: c)
      }
    }

    printTree(t, Nil)
    sb.toString
  }

  private def getPadding(tree: Tree, stack: List[Char]) = {
    val width = formattedWidth(tree) + stack.size + (1 + spacing)
    val diff = maxWidth - width

    if (diff == 0) " " else " " + (PaddingChar * (diff - 1)) + " "
  }

  private def widthOf(t: Tree) = {
    var maxWidth = -1
    val traverser = new DepthTraverser((tree, depth) => {
      var width = formattedWidth(tree)
      if (depth > 0) width += (1 + spacing) * depth + (1 + spacing)
      maxWidth = math.max(maxWidth, width)
    })
    traverser.traverse(t)
    maxWidth
  }

  private def formattedWidth(t: Tree) = formatTree(t).charCount

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


  private def getContent(t: Tree, stack: List[Char]): String = {
    val content = (getSymbolContent(t), getTypeContent(t)) match {
      case (None, None)                                       =>
        ""
      case (Some((s, color)), None)                           =>
        color(s)
      case (None, Some((s, color)))                           =>
        " " * SymbolWidth + color(s)
      case (Some((sString, sColor)), Some((tString, tColor))) =>
        sColor(sString) + " " * (SymbolWidth - sString.length) + tColor(tString)
    }

    if (content.isEmpty)
      return ""

    val padding = getPadding(t, stack)
    padding + content
  }

  private def getTypeContent(t: Tree): Option[(String, Color)] = {
    t match {
      case typed: Typed if typed.getType != TUntyped =>
        val tpe = typed.getType
        val color = typeColor(tpe)
        val s = tpe.toString
          .replaceAll(".*::", "")
          .replaceAll("\\$ERROR", "Error")
        Some(s, color)
      case _: Typed                                  =>
        Missing
      case _                                         => None
    }
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

  private def getSymbolContent(t: Tree): Option[(String, Color)] = t match {
    case symbolic: Symbolic[_] if symbolic.hasSymbol =>
      val symbol = symbolic.getSymbol
      val (id, color) = symbolMap.getOrElseUpdate(symbol, newSymbolFormatting)
      val fullColor = t match {
        case _: MethodDeclTree | _: ClassDeclTree | _: VarDecl => Underline + color
        case _                                                 => color
      }
      val symbolLetter = getSymbolLetter(symbol)
      Some(symbolLetter + id, fullColor)
    case _: Symbolic[_]                              =>
      Missing
    case _                                           => None
  }

  private def getSymbolLetter(symbol: Symbol) = symbol match {
    case _: ExtensionClassSymbol        => "E"
    case c: ClassSymbol if c.isAbstract => "T"
    case _: ClassSymbol                 => "C"
    case _: OperatorSymbol              => "O"
    case _: MethodSymbol                => "M"
    case _: FieldSymbol                 => "F"
    case _: VariableSymbol              => "V"
  }

  private def newSymbolFormatting = {
    val id = symbolId
    val colorIndex = id % AllColors.length
    symbolId += 1
    (id, AllColors(colorIndex))
  }


  private class DepthTraverser[U] private(depth: Int, f: (Tree, Int) => U) extends Trees.Traverser {

    def this(f: (Tree, Int) => U) = this(0, f)

    private def superTraverse(t: Tree): Unit = super._traverse(t)

    override def _traverse(t: Tree): Unit = {
      f(t, depth)
      new DepthTraverser(depth + 1, f).superTraverse(t)
    }
  }

}
