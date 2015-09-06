package tcompiler
package ast

import org.apache.commons.lang3.StringEscapeUtils._
import tcompiler.analyzer.Symbols._
import tcompiler.ast.Trees._


object Printer {

  val Indentation = 2

  private var indent: Int   = 0
  private var printIdNumber = false

  private def l: String = {
    indent += 1
    "{" + n
  }

  private def r: String = {
    indent -= 1
    n + "}"
  }

  private def n: String = "\n" + " " * (Indentation * indent)

  def apply(t: Tree, printIdNumber: Boolean = false): String = {
    this.printIdNumber = printIdNumber
    indent = 0
    f(t)
  }

  private def f(t: Tree): String = {
    val s = t match {
      case Program(pack, imports, main, classes)                         => optional(pack, f) + all(imports) + n + all(classes)
      case Package(identifiers)                                          => "package " + identifiers.map(_.value).mkString(".") + n
      case RegularImport(identifiers)                                    => "import " + identifiers.map(_.value).mkString(".") + n
      case WildCardImport(identifiers)                                   => "import " + identifiers.map(_.value).mkString(".") + ".*" + n
      case GenericImport(identifiers)                                    => "import <" + identifiers.map(_.value).mkString(".") + ">" + n
      case ClassDecl(id, parent, vars, methods)                          => n + n + "class " + f(id) + optional(parent, t => " extends " + f(t.asInstanceOf[ClassIdentifier])) + " " + l + all(vars) + n + all(methods) + r
      case VarDecl(tpe, id, expr, modifiers)                             => varDecl(modifiers) + " " + f(id) + optional(tpe, t => " : " + f(t)) + optional(expr, t => " = " + f(t)) + n
      case MethodDecl(retType, id, args, stat, modifiers)                => definition(modifiers) + " " + f(id) + "(" + commaList(args) + ")" + optional(retType, t => ": " + f(t)) + " = " + f(stat)
      case ConstructorDecl(_, id, args, stat, modifiers)                 => definition(modifiers) + " new(" + commaList(args) + ") = " + f(stat) + n
      case OperatorDecl(operatorType, retType, args, stat, modifiers, _) => definition(modifiers) + " " + Trees.operatorString(operatorType) + "(" + commaList(args) + ")" + optional(retType, t => ": " + f(t)) + " = " + f(stat) + n
      case Formal(tpe, id)                                               => f(id) + ": " + f(tpe)
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
      case If(expr, thn, els)               => "if(" + f(expr) + ") " + statement(thn, true) + optional(els, stat => "else " + statement(stat.asInstanceOf[StatTree], true))
      case While(expr, stat)                => "while(" + f(expr) + ") " + statement(stat, true)
      case For(init, condition, post, stat) => "for(" + forAssign(init) + " ; " + f(condition) + " ; " + commaList(post) + ") " + statement(stat, true)
      case Print(expr)                      => "print(" + f(expr) + ")"
      case Println(expr)                    => "println(" + f(expr) + ")"
      case Assign(id, expr)                 => id.value + " = " + f(expr)
      case ArrayAssign(id, index, expr)     => f(id) + "[" + f(index) + "] = " + f(expr)
      case Return(expr)                     => "return " + optional(expr, f)
      case Error(expr)                      => "error(" + f(expr) + ")"
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
      case Hash(expr)                        => "#(" + f(expr) + ")"
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
      case NewArray(tpe, sizes)              => "new " + f(tpe) + sizes.map("[" + f(_) + "]")
      case New(tpe, exprs)                   => "new " + f(tpe) + "(" + commaList(exprs) + ")"
      case PreIncrement(id)                  => "++" + f(id)
      case PostIncrement(id)                 => f(id) + "++"
      case PreDecrement(id)                  => "--" + f(id)
      case PostDecrement(id)                 => f(id) + "--"
      case Ternary(condition, thn, els)      => f(condition) + " ? " + f(thn) + " : " + f(els)
      case Empty()                           => ""
    }
    s
  }


  private def mainMethod(meth: Option[MethodDecl]): String = meth match {
    case Some(MethodDecl(_, id, _, stat, _)) => "main " + f(id) + " = " + f(stat) + n
    case _                                   => ""
  }

  private def symbol[T <: Symbol](t: Symbolic[T]): String =
    if (printIdNumber)
      if (t.hasSymbol) "[" + t.getSymbol.getType + "]" else "??"
    else ""

  private def definition(modifiers: Set[Modifier]) = {
    val decl = modifiers.find(_.isInstanceOf[Accessability]).get match {
      case Private   => "def"
      case Public    => "Def"
      case Protected => "def protected"
    }

    decl + staticModifier(modifiers)
  }

  private def varDecl(modifiers: Set[Modifier]) = {

    val decl = modifiers.find(_.isInstanceOf[Accessability]).get match {
      case Private   => "var"
      case Public    => "Var"
      case Protected => "var protected"
    }

    decl + staticModifier(modifiers)
  }

  private def staticModifier(modifiers: Set[Modifier]) = {
    modifiers.find(_ == Static) match {
      case Some(_) => " static"
      case _       => ""
    }
  }

  private def optional(t: Option[Tree], f: (Tree) => String) = if (t.isDefined) f(t.get) else ""

  private def forAssign(list: List[StatTree]) = list.map {
    case VarDecl(tpe, id, expr, modifiers) => varDecl(modifiers) + " " + f(id) + optional(tpe, t => " : " + f(t)) + optional(expr, t => " = " + f(t))
  }.mkString(", ")

  private def commaList(list: List[Tree]): String = list.map(f).mkString(", ")

  private def allStats(list: List[StatTree]): String = {
    list.zipWithIndex.map { case (stat, i) =>
      statement(stat, i == list.length - 1)
    }.mkString
  }


  private def statement(stat: StatTree, last: Boolean): String = {
    val s = stat match {
      case _: Block                          => f(stat)
      case _: For                            => f(stat)
      case _: While                          => f(stat)
      case _: If                             => f(stat)
      case VarDecl(tpe, id, expr, modifiers) => varDecl(modifiers) + " " + f(id) + optional(tpe, t => " : " + f(t)) + optional(expr, t => " = " + f(t))
      case _                                 => f(stat)
    }
    if (last) s else s + n
  }

  private def all(list: List[Tree], start: String = "") = list.foldLeft(start)(_ + f(_) + n)

}
