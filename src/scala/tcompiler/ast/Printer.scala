package tcompiler
package ast

import java.util.regex.Matcher

import org.apache.commons.lang3.StringEscapeUtils._
import tcompiler.lexer.Tokens


object Printer {

  import Trees._

  var PrintInColor = true

  val Indentation = 4
  def KeywordColor = Console.RED
  def IdentifierColor = Console.CYAN
  def StringColor = Console.YELLOW
  def NumColor = Console.MAGENTA
  def ColorReset = Console.RESET

  val Keywords      = Tokens.Keywords.keys.toList.sortBy(-_.length)
  val KeywordsRegex = s"(${Keywords.mkString("|")})".r

  private var indent: Int = 0

  def apply(t: Tree, printInColor: Boolean = true): String = {
    PrintInColor = printInColor
    indent = 0
    prettyPrint(t)
  }


  def prettyPrint(t: Tree): String = {
    val s = t match {
      case Program(pack, imports, classes)                               => p"$pack$N$imports$N$classes"
      case Package(identifiers)                                          => p"package ${Separated(identifiers, ".")}"
      case RegularImport(identifiers)                                    => p"import ${Separated(identifiers, ".")}"
      case WildCardImport(identifiers)                                   => p"import ${Separated(identifiers, ".")}.*"
      case GenericImport(identifiers)                                    => p"import <${Separated(identifiers, ".")}>"
      case ClassDecl(id, parent, vars, methods)                          => p"$N${N}class $id${optional(parent)(t => p" extends $t")} ${varsAndMethods(vars, methods)}"
      case VarDecl(tpe, id, expr, modifiers)                             => p"${varDecl(modifiers)} $id${optional(tpe)(t => p" : $t")}${optional(expr)(t => p" = $t")}"
      case MethodDecl(retType, id, args, stat, modifiers)                => p"${definition(modifiers)} $id(${Separated(args, ", ")})${optional(retType)(t => p": $t")} = $stat$N"
      case ConstructorDecl(_, id, args, stat, modifiers)                 => p"${definition(modifiers)} new(${Separated(args, ", ")}) = $stat$N"
      case OperatorDecl(operatorType, retType, args, stat, modifiers, _) => p"${definition(modifiers)} ${operatorString(operatorType)}(${Separated(args, ", ")})${optional(retType)(t => p": $t")} = $stat$N"
      case Formal(tpe, id)                                               => p"$id: $tpe"
      // Types
      case ArrayType(tpe) => p"$tpe[]"
      case IntType()      => p"Int"
      case LongType()     => p"Long"

      case FloatType()   => p"Float"
      case DoubleType()  => p"Double"
      case BooleanType() => p"Bool"
      case CharType()    => p"Char"
      case StringType()  => p"String"
      case UnitType()    => p"Unit"
      // Statements
      case Block(stats)                     => if (stats.isEmpty) "{}" else p"$L$stats$R"
      case If(expr, thn, els)               => p"if($expr) ${Stat(thn)}${optional(els)(stat => p"${N}else ${Stat(stat)}")}"
      case While(expr, stat)                => p"while($expr) ${Stat(stat)}"
      case For(init, condition, post, stat) => p"for(${Separated(init, ", ")} ; $condition ; ${Separated(post, ", ")}) ${Stat(stat)}"
      case Print(expr)                      => p"print($expr)"
      case Println(expr)                    => p"println($expr)"
      case Assign(id, expr)                 => p"$id = $expr"
      case ArrayAssign(id, index, expr)     => p"$id[$index] = $expr"
      case Return(expr)                     => p"return $expr"
      case Error(expr)                      => p"error($expr)"
      // Expressions
      case And(lhs, rhs)                   => p"($lhs && $rhs)"
      case Or(lhs, rhs)                    => p"($lhs || $rhs)"
      case Plus(lhs, rhs)                  => p"($lhs + $rhs)"
      case Minus(lhs, rhs)                 => p"($lhs - $rhs)"
      case LogicAnd(lhs, rhs)              => p"($lhs & $rhs)"
      case LogicOr(lhs, rhs)               => p"($lhs | $rhs)"
      case LogicXor(lhs, rhs)              => p"($lhs ^ $rhs)"
      case LeftShift(lhs, rhs)             => p"($lhs << $rhs)"
      case RightShift(lhs, rhs)            => p"($lhs >> $rhs)"
      case Times(lhs, rhs)                 => p"($lhs * $rhs)"
      case Div(lhs, rhs)                   => p"($lhs / $rhs)"
      case Modulo(lhs, rhs)                => p"($lhs % $rhs)"
      case LessThan(lhs, rhs)              => p"($lhs < $rhs)"
      case LessThanEquals(lhs, rhs)        => p"($lhs <= $rhs)"
      case GreaterThan(lhs, rhs)           => p"($lhs > $rhs)"
      case GreaterThanEquals(lhs, rhs)     => p"($lhs >= $rhs)"
      case Equals(lhs, rhs)                => p"($lhs == $rhs)"
      case NotEquals(lhs, rhs)             => p"($lhs != $rhs)"
      case Instance(expr, id)              => p"($expr is $id)"
      case As(expr, tpe)                   => p"($expr as $tpe)"
      case Not(expr)                       => p"!($expr)"
      case Negation(expr)                  => p"-($expr)"
      case LogicNot(expr)                  => p"~($expr)"
      case Hash(expr)                      => p"#($expr)"
      case ArrayRead(arr, index)           => p"$arr[$index]"
      case FieldRead(obj, id)              => p"$obj.$id"
      case FieldAssign(obj, id, expr)      => p"$obj.$id = $expr"
      case MethodCall(obj, meth, args)     => p"$obj.$meth(${Separated(args, ", ")})"
      case IntLit(value)                   => p"$value"
      case LongLit(value)                  => p"${value}L"
      case FloatLit(value)                 => p"${value}F"
      case DoubleLit(value)                => p"$value"
      case CharLit(value)                  => p"'${escapeJava(p"$value")}'"
      case StringLit(value)                => "\"" + p"${escapeJava(p"$value")}" + "\""
      case ArrayLit(expressions)           => p"{ ${Separated(expressions, ", ")} }"
      case True()                          => p"true"
      case False()                         => p"false"
      case Identifier(value)               => p"$value"
      case id@ClassIdentifier(value, list) => p"$value${templateList(id)}"
      case This()                          => p"this"
      case NewArray(tpe, sizes)            => p"new $tpe${arrayList(sizes)}"
      case New(tpe, exprs)                 => p"new $tpe(${Separated(exprs, ", ")})"
      case PreIncrement(id)                => p"++$id"
      case PostIncrement(id)               => p"$id++"
      case PreDecrement(id)                => p"--$id"
      case PostDecrement(id)               => p"$id--"
      case Ternary(condition, thn, els)    => p"$condition ? $thn : $els"
      case Empty()                         => ""
    }
    s
  }

  private def varsAndMethods(vars: List[VarDecl], methods: List[FuncTree]): String = {
    if (vars.isEmpty && methods.isEmpty)
      return "{ }"

    if (vars.isEmpty)
      return p"$L$N$methods$R"

    if (methods.isEmpty)
      return p"$L$N$vars$R$R"

    p"$L$N$vars$N$N$methods$R"
  }

  private def arrayList(sizes: List[ExprTree]) = sizes.map(s => p"[$s]").mkString("")

  private def templateList(id: ClassIdentifier) = if (id.isTemplated) p"<${Separated(id.templateTypes, ", ")}>" else ""

  private def definition(modifiers: Set[Modifier]) = {
    val decl = modifiers.find(_.isInstanceOf[Accessability]).get match {
      case Private()   => p"def"
      case Public()    => p"Def"
      case Protected() => p"def protected"
    }

    decl + mods(modifiers)
  }

  private def varDecl(modifiers: Set[Modifier]) = {
    val decl = modifiers.find(_.isInstanceOf[Accessability]).get match {
      case Private()   => p"var"
      case Public()    => p"Var"
      case Protected() => p"var protected"
    }

    decl + mods(modifiers)
  }

  private def mods(modifiers: Set[Modifier]) =
    modifiers.map {
      case Static()   => p"static"
      case Implicit() => p"implicit"
      case _          => ""
    }.mkString(" ")

  private def optional[T](t: Option[T])(f: (T => String)) = if (t.isDefined) f(t.get) else ""


  trait Formatter {
    def apply(): String
  }

  object L extends Formatter {
    def apply() = {
      indent += 1
      "{" + N()
    }
  }

  object R extends Formatter {
    def apply() = {
      indent -= 1
      N() + "}"
    }
  }

  object N extends Formatter {
    def apply() = "\n" + " " * (Indentation * indent)
  }

  case class Stat(stat: StatTree) extends Formatter {

    def apply() = {
      stat match {
        case Block(_) => p"$stat"
        case _        =>
          indent += 1
          val s = p"$N$stat"
          indent -= 1
          s
      }
    }

  }

  case class Separated(list: List[Tree], seperator: String) extends Formatter {
    def apply() = list.map(t => p"$t").mkString(seperator)
  }

  implicit class PrinterContext(val sc: StringContext) extends AnyVal {

    def p(args: Any*): String = {
      val strings = sc.parts.iterator
      val expressions = args.iterator
      val sb = new StringBuilder(colorKeywords(strings.next))
      while (strings.hasNext) {
        sb ++= evaluate(expressions.next)
        sb ++= colorKeywords(strings.next)
      }
      sb.toString
    }

    private def evaluate(obj: Any): String = obj match {
      case f: Formatter  => f()
      case t: Tree       =>
        t match {
          case _: Identifier |
               _: ClassIdentifier => color(t, IdentifierColor)
          case _: StringLit |
               _: CharLit         => color(t, StringColor)
          case _: IntLit |
               _: LongLit |
               _: FloatLit |
               _: DoubleLit       => color(t, NumColor)
          case _                  => prettyPrint(t)
        }
      case Some(t: Tree) => evaluate(t)
      case None          => ""
      case l: List[Tree] => mkString(l)
      case s: String     => s
      case x             => x.toString
    }

    private def mkString(list: List[Tree]) = {
      val it = list.iterator
      var s = ""
      while (it.hasNext) {
        s += evaluate(it.next)
        if (it.hasNext)
          s += N()
      }
      s
    }

    private def color(tree: Tree, color: String) =
      if (PrintInColor)
        p"$color${prettyPrint(tree)}$ColorReset"
      else
        prettyPrint(tree)

    private def colorKeywords(output: String): String = {
      if (!PrintInColor)
        return output
      KeywordsRegex.replaceAllIn(output, m => {
        Matcher.quoteReplacement(s"$KeywordColor${m.group(1)}$ColorReset")
      })
    }
  }

}
