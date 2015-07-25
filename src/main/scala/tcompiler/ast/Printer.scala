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
      case VarDecl(tpe, id, expr, access)                     => varDecl(access) + " " + f(id) + " : " + f(tpe) + optional(expr, t => " = " + f(t)) + ";" + n
      case MethodDecl(retType, id, args, vars, stats, access) => definition(access) + " " + f(id) + "(" + commaList(args) + "): " + f(retType) + " = " + l + all(vars) + allStats(stats) + r + n
      case ConstructorDecl(args, vars, stats, access)         => definition(access) + " new(" + commaList(args) + ") = " + l + all(vars) + allStats(stats) + r + n
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
      case If(expr, thn, els)               => "if(" + f(expr) + ") " + statement(thn) + optional(els, stat => "else " + statement(stat.asInstanceOf[StatTree]))
      case While(expr, stat)                => "while(" + f(expr) + ") " + statement(stat)
      case For(init, condition, post, stat) => "for(" + forAssign(init) + " ; " + f(condition) + " ; " + commaList(post) + ") " + statement(stat)
      case Print(expr)                      => "print(" + f(expr) + ")"
      case Println(expr)                    => "println(" + f(expr) + ")"
      case Assign(id, expr)                 => id.value + " = " + f(expr)
      case ArrayAssign(id, index, expr)     => f(id) + "[" + f(index) + "] = " + f(expr)
      case Return(expr)                     => "return " + optional(expr, f)
      // Expressions
      case And(lhs, rhs)                     => "(" + f(lhs) + " && " + f(rhs) + ")"
      case Or(lhs, rhs)                      => "(" + f(lhs) + " || " + f(rhs) + ")"
      case Plus(lhs, rhs)                    => "(" + f(lhs) + " + " + f(rhs) + ")"
      case Minus(lhs, rhs)                   => "(" + f(lhs) + " - " + f(rhs) + ")"
      case LogicAnd(lhs, rhs)                => "(" + f(lhs) + " & " + f(rhs) + ")"
      case LogicOr(lhs, rhs)                 => "(" + f(lhs) + " | " + f(rhs) + ")"
      case LogicXor(lhs, rhs)                => "(" + f(lhs) + " ^ " + f(rhs) + ")"
      case LeftShift(lhs, rhs)               => "(" + f(lhs) + " << " + f(rhs) + ")"
      case RightShift(lhs, rhs)              => "(" + f(lhs) + " >> " + f(rhs) + ")"
      case Times(lhs, rhs)                   => "(" + f(lhs) + " * " + f(rhs) + ")"
      case Div(lhs, rhs)                     => "(" + f(lhs) + " / " + f(rhs) + ")"
      case Modulo(lhs, rhs)                  => "(" + f(lhs) + " % " + f(rhs) + ")"
      case LessThan(lhs, rhs)                => "(" + f(lhs) + " < " + f(rhs) + ")"
      case LessThanEquals(lhs, rhs)          => "(" + f(lhs) + " <= " + f(rhs) + ")"
      case GreaterThan(lhs, rhs)             => "(" + f(lhs) + " > " + f(rhs) + ")"
      case GreaterThanEquals(lhs, rhs)       => "(" + f(lhs) + " >= " + f(rhs) + ")"
      case Equals(lhs, rhs)                  => "(" + f(lhs) + " == " + f(rhs) + ")"
      case NotEquals(lhs, rhs)               => "(" + f(lhs) + " != " + f(rhs) + ")"
      case Instance(expr, id)                => "(" + f(expr) + " inst " + f(id) + ")"
      case As(expr, tpe)                     => "(" + f(expr) + " as " + f(tpe) + ")"
      case Not(expr)                         => "!(" + f(expr) + ")"
      case Negation(expr)                    => "-(" + f(expr) + ")"
      case LogicNot(expr)                    => "~(" + f(expr) + ")"
      case ArrayRead(arr, index)             => f(arr) + "[" + f(index) + "]"
      case ArrayLength(arr)                  => f(arr) + ".length"
      case FieldRead(obj, id)                => f(obj) + "." + f(id)
      case FieldAssign(obj, id, expr)        => f(obj) + "." + f(id) + " = " + f(expr)
      case MethodCall(obj, meth, args)       => f(obj) + "." + f(meth) + "(" + commaList(args) + ")"
      case IntLit(value)                     => value.toString
      case LongLit(value)                    => value.toString + "L"
      case FloatLit(value)                   => value.toString + "F"
      case DoubleLit(value)                  => value.toString
      case CharLit(value)                    => "'" + escapeJava("" + value) + "'"
      case StringLit(value)                  => "\"" + escapeJava(value) + "\""
      case True()                            => "true"
      case False()                           => "false"
      case id @ Identifier(value)            => value + symbol(id)
      case id @ ClassIdentifier(value, list) => value + symbol(id) + (if (id.isTemplated) "<" + commaList(list) + ">" else "")
      case This()                            => "this"
      case NewArray(tpe, size)               => "new " + f(tpe) + "[" + f(size) + "]"
      case New(tpe, exprs)                   => "new " + f(tpe) + "(" + commaList(exprs) + ")"
      case PreIncrement(id)                  => "++" + f(id)
      case PostIncrement(id)                 => f(id) + "++"
      case PreDecrement(id)                  => "--" + f(id)
      case PostDecrement(id)                 => f(id) + "--"
      case Ternary(condition, thn, els)      => f(condition) + " ? " + f(thn) + " : " + f(els)
    }
    s
  }

  private def definition(a: Accessability) = a match {
    case Private   => "def"
    case Public    => "Def"
    case Protected => "def protected"
  }

  private def varDecl(a: Accessability) = a match {
    case Private   => "var"
    case Public    => "Var"
    case Protected => "var protected"
  }

  private def pos(t: Tree): String = "[" + t.line + ", " + t.col + "]"

  private def optional(t: Option[Tree], f: (Tree) => String) = if (t.isDefined) f(t.get) else ""

  private def forAssign(list: List[Assign]) = list.map(a => a.id.value + " = " + f(a.expr)).mkString(", ")

  private def commaList(list: List[Tree]): String = list.map(f).mkString(", ")

  private def statement(stat: StatTree): String = stat match {
    case _: Block => f(stat)
    case _: For   => f(stat) + n
    case _: While => f(stat) + n
    case _: If    => f(stat) + n
    case _        => f(stat) + ";" + n
  }

  private def allStats(list: List[StatTree]): String =
    list.map(statement).mkString

  private def all(list: List[Tree], start: String = "") = list.foldLeft(start)(_ + f(_))

  private def typeOf(t: Tree): String = t match {
    case typed: Typed => "[" + typed.getType + "]"
    case _            => ""
  }

}
