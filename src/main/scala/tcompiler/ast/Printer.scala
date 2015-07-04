package tcompiler
package ast

import Trees._
import tcompiler.analyzer.Symbols._
import tcompiler.analyzer.Types.Typed
import org.apache.commons.lang3.StringEscapeUtils._


object Printer {

  var indent: Int   = 0
  var printIdNumber = false

  def l: String = {
    indent += 1
    "{" + n
  }

  def r: String = {
    indent -= 1
    n + "}"
  }

  def n: String = "\n" + " " * (2 * indent)

  def symbol[T <: Symbol](t: Symbolic[T]): String =
    if (printIdNumber)
      if (t.hasSymbol) "[" + t.getSymbol.getType + "]" else "??"
    else ""

  def apply(t: Tree, printIdNumber: Boolean = false): String = {
    this.printIdNumber = printIdNumber
    f(t)
  }

  private def f(t: Tree): String = {
    val s = t match {
      case Program(pack, imports, main, classes)              => optional(pack, f) + all(imports) + n + optional(main, f) + all(classes)
      case Package(identifiers)                               => "package " + identifiers.map(_.value).mkString(".") + ";" + n
      case RegularImport(identifiers)                         => "import " + identifiers.map(_.value).mkString(".") + ";" + n
      case WildCardImport(identifiers)                        => "import " + identifiers.map(_.value).mkString(".") + ".*;" + n
      case GenericImport(identifiers)                         => "import <" + identifiers.map(_.value).mkString(".") + ">;" + n
      case MainObject(id, stats)                              => "main " + f(id) + " = " + l + allStats(stats) + r
      case ClassDecl(id, parent, vars, methods)               => n + n + "class " + f(id) + optional(parent, t => " extends " + f(t.asInstanceOf[ClassIdentifier])) + " " + l + all(vars) + all(methods) + "" + r
      case VarDecl(tpe, id, expr)                             => "var " + f(id) + " : " + f(tpe) + optional(expr, t => " = " + f(t)) + ";" + n
      case MethodDecl(retType, id, args, vars, stats, access) => definition(access) + " " + f(id) + "(" + commaList(args) + "): " + f(retType) + " = " + l + all(vars) + allStats(stats) + r + n
      case ConstructorDecl(id, args, vars, stats, access)     => definition(access) + " " + f(id) + "(" + commaList(args) + ") = " + l + all(vars) + allStats(stats) + r + n
      case Formal(tpe, id)                                    => f(id) + ": " + f(tpe)
      // Types
      case ArrayType(tpe) => f(tpe) + "[]"
      case IntType()      => "Int"
      case LongType()     => "Long"
      case FloatType()    => "Float"
      case DoubleType()   => "Double"
      case BooleanType()  => "Bool"
      case CharType()     => "Char"
      case StringType()   => "String"
      case UnitType()     => "Unit"
      // Statements
      case Block(stats)                     => l + allStats(stats) + r
      case If(expr, thn, els)               => "if(" + f(expr) + ")" + f(thn) + optional(els, "else " + f(_)) + n
      case While(expr, stat)                => "while(" + f(expr) + ") " + f(stat) + n
      case For(init, condition, post, stat) => "for(" + forAssign(init) + " ; " + f(condition) + " ; " + commaList(post) + ") " + f(stat) + n
      case Print(expr)                      => "print(" + f(expr) + "); " + n
      case Println(expr)                    => "println(" + f(expr) + "); " + n
      case Assign(id, expr)                 => id.value + " = " + f(expr) + "; " + n
      case PlusAssign(id, expr)             => id.value + " += " + f(expr) + "; " + n
      case MinusAssign(id, expr)            => id.value + " -= " + f(expr) + "; " + n
      case MulAssign(id, expr)              => id.value + " *= " + f(expr) + "; " + n
      case DivAssign(id, expr)              => id.value + " /= " + f(expr) + "; " + n
      case ModAssign(id, expr)              => id.value + " %= " + f(expr) + "; " + n
      case AndAssign(id, expr)              => id.value + " &= " + f(expr) + "; " + n
      case OrAssign(id, expr)               => id.value + " |= " + f(expr) + "; " + n
      case XorAssign(id, expr)              => id.value + " ^= " + f(expr) + "; " + n
      case LeftShiftAssign(id, expr)        => id.value + " <<= " + f(expr) + "; " + n
      case RightShiftAssign(id, expr)       => id.value + " >>= " + f(expr) + "; " + n
      case ArrayAssign(id, index, expr)     => f(id) + "[" + f(index) + "] = " + f(expr) + ";" + n
      case Return(expr)                     => "return " + optional(expr, f) + ";" + n
      // Expressions
      case And(lhs, rhs)                   => "(" + f(lhs) + " && " + f(rhs) + ")"
      case Or(lhs, rhs)                    => "(" + f(lhs) + " || " + f(rhs) + ")"
      case Plus(lhs, rhs)                  => "(" + f(lhs) + " + " + f(rhs) + ")"
      case Minus(lhs, rhs)                 => "(" + f(lhs) + " - " + f(rhs) + ")"
      case LogicAnd(lhs, rhs)              => "(" + f(lhs) + " & " + f(rhs) + ")"
      case LogicOr(lhs, rhs)               => "(" + f(lhs) + " | " + f(rhs) + ")"
      case LogicXor(lhs, rhs)              => "(" + f(lhs) + " ^ " + f(rhs) + ")"
      case LeftShift(lhs, rhs)             => "(" + f(lhs) + " << " + f(rhs) + ")"
      case RightShift(lhs, rhs)            => "(" + f(lhs) + " >> " + f(rhs) + ")"
      case Times(lhs, rhs)                 => "(" + f(lhs) + " * " + f(rhs) + ")"
      case Div(lhs, rhs)                   => "(" + f(lhs) + " / " + f(rhs) + ")"
      case Modulo(lhs, rhs)                => "(" + f(lhs) + " % " + f(rhs) + ")"
      case LessThan(lhs, rhs)              => "(" + f(lhs) + " < " + f(rhs) + ")"
      case LessThanEquals(lhs, rhs)        => "(" + f(lhs) + " <= " + f(rhs) + ")"
      case GreaterThan(lhs, rhs)           => "(" + f(lhs) + " > " + f(rhs) + ")"
      case GreaterThanEquals(lhs, rhs)     => "(" + f(lhs) + " >= " + f(rhs) + ")"
      case Equals(lhs, rhs)                => "(" + f(lhs) + " == " + f(rhs) + ")"
      case NotEquals(lhs, rhs)             => "(" + f(lhs) + " != " + f(rhs) + ")"
      case Instance(expr, id)              => "(" + f(expr) + " inst " + f(id) + ")"
      case As(expr, tpe)                   => "(" + f(expr) + " as " + f(tpe) + ")"
      case Not(expr)                       => "!(" + f(expr) + ")"
      case Negation(expr)                  => "-(" + f(expr) + ")"
      case LogicNot(expr)                  => "~(" + f(expr) + ")"
      case ArrayRead(arr, index)           => f(arr) + "[" + f(index) + "]"
      case ArrayLength(arr)                => f(arr) + ".length"
      case MethodCall(obj, meth, args)     => f(obj) + "." + f(meth) + "(" + commaList(args) + ")"
      case IntLit(value)                   => value.toString
      case LongLit(value)                  => value.toString + "l"
      case FloatLit(value)                 => value.toString + "f"
      case DoubleLit(value)                => value.toString
      case CharLit(value)                  => "'" + value + "'"
      case StringLit(value)                => "\"" + escapeJava(value) + "\""
      case True()                          => "true"
      case False()                         => "false"
      case id@Identifier(value)            => value + symbol(id)
      case id@ClassIdentifier(value, list) => value + symbol(id) + (if (id.isTemplated) "<" + commaList(list) + ">" else "")
      case This()                          => "this"
      case NewArray(tpe, size)             => "new " + f(tpe) + "[" + f(size) + "]"
      case New(tpe, exprs)                 => "new " + f(tpe) + "(" + commaList(exprs) + ")"
      case PreIncrement(id)                => "++" + f(id)
      case PostIncrement(id)               => f(id) + "++"
      case PreDecrement(id)                => "--" + f(id)
      case PostDecrement(id)               => f(id) + "--"
      case Ternary(condition, thn, els)    => f(condition) + " ? " + f(thn) + " : " + f(els)
    }
    s
  }

  private def definition(a: Accessability) = a match {
    case Private   => "def"
    case Public    => "Def"
    case Protected => "def protected"
  }

  private def pos(t: Tree): String = "[" + t.line + ", " + t.col + "]"

  private def optional(t: Option[Tree], f: (Tree) => String) = if (t.isDefined) f(t.get) else ""

  private def forAssign(list: List[Assign]) = list.map(a => a.id.value + " = " + f(a.expr)).mkString(", ")

  private def commaList(list: List[Tree]): String = list.map(f).mkString(", ")

  private def allStats(list: List[StatTree]): String =
    list.map {
      case MethodCall(obj, meth, args) => f(obj) + "." + f(meth) + "(" + commaList(args) + ");" + n
      case x                           => f(x)
    }.mkString

  private def all(list: List[Tree], start: String = "") = list.foldLeft(start)(_ + f(_))

  private def typeOf(t: Tree): String = t match {
    case typed: Typed => "[" + typed.getType + "]"
    case _            => ""
  }

}
