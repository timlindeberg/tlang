package tcompiler
package ast

import java.util.regex.Matcher

import org.apache.commons.lang3.StringEscapeUtils._
import tcompiler.lexer.Tokens
import tcompiler.utils.Colored


object Printer extends Colored {

  import Trees._

  var useColor = false

  val Indentation = 3

  val Keywords      = Tokens.Keywords.keys.toList.sortBy(-_.length) ::: List("Int", "Char", "Float", "Double", "Bool", "Unit")
  val KeywordsRegex = s"(${Keywords.mkString("|")})".r

  private var indent: Int = 0

  def apply(t: Tree, printInColor: Boolean = true): String = {
    useColor = printInColor
    indent = 0
    prettyPrint(t)
  }

  private def comment(cu: CompilationUnit) = {
    val fileName = if (cu.hasPosition)
      cu.file.getName
    else
      "No file"

    s"""|//-------------------------------------------------------------
        |//--- $fileName
        |//-------------------------------------------------------------
        |""".stripMargin
  }

  private def prettyPrint(t: Tree): String = t match {
    case cu@CompilationUnit(pack, classes, importMap)               => p"${comment(cu)}$pack${imports(importMap.imports)}$classes"
    case Package(address)                                           => p"${packDecl(address)}"
    case RegularImport(address)                                     => p"import ${address.mkString("::")}"
    case ExtensionImport(address, className)                        => p"import ${address.mkString("::")}::extension ${className.mkString("::")}"
    case WildCardImport(address)                                    => p"import ${address.mkString("::")}.*"
    case ClassDecl(id, parents, fields, methods)                    => p"$N${N}class ${restOfClassDecl(id, parents, fields, methods)}"
    case TraitDecl(id, parents, fields, methods)                    => p"$N${N}trait ${restOfClassDecl(id, parents, fields, methods)}"
    case ExtensionDecl(id, methods)                                 => p"$N${N}extension ${restOfClassDecl(id, Nil, Nil, methods)}"
    case VarDecl(tpe, id, expr, modifiers)                          => p"${varDecl(modifiers)} $id${optional(tpe)(t => p": $t")}${optional(expr)(t => p" = $t")}"
    case MethodDecl(retType, id, args, stat, modifiers)             => p"${definition(modifiers)} $id(${Separated(args, ", ")})${optional(retType)(t => p": $t")}${optional(stat)(s => p" = $s")}$N"
    case ConstructorDecl(_, id, args, stat, modifiers)              => p"${definition(modifiers)} new(${Separated(args, ", ")}) = $stat$N"
    case OperatorDecl(operatorType, retType, args, stat, modifiers) => p"${definition(modifiers)} ${operatorType.op}(${Separated(args, ", ")})${optional(retType)(t => p": $t")} = $stat$N"
    case Formal(tpe, id)                                            => p"$id: $tpe"
    case Private()                                                  => p"private"
    case Public()                                                   => p"public"
    case Protected()                                                => p"protected"
    case Final()                                                    => p"final"
    // Types
    case ArrayType(tpe)    => p"$tpe[]"
    case IntType()         => p"Int"
    case LongType()        => p"Long"
    case FloatType()       => p"Float"
    case DoubleType()      => p"Double"
    case BooleanType()     => p"Bool"
    case CharType()        => p"Char"
    case UnitType()        => p"Unit"
    case NullableType(tpe) => p"$tpe?"
    // Statements
    case Block(stats)                      => if (stats.isEmpty) "{}" else p"$L$stats$R"
    case If(condition, thn, els)           => p"if($condition) ${Stat(thn)}${optional(els)(stat => p"${N}else ${Stat(stat)}")}"
    case While(condition, stat)            => p"while($condition) ${Stat(stat)}"
    case For(init, condition, post, stat)  => p"for(${Separated(init, ", ")} ; $condition ; ${Separated(post, ", ")}) ${Stat(stat)}"
    case Foreach(varDecl, container, stat) => p"for($varDecl in $container) ${Stat(stat)}"
    case Print(expr)                       => p"print($expr)"
    case Println(expr)                     => p"println($expr)"
    case Error(expr)                       => p"error($expr)"
    case Assign(id, expr)                  => p"$id = $expr"
    case ArrayAssign(id, index, expr)      => p"$id[$index] = $expr"
    case Return(expr)                      => p"return $expr"
    // Expressions
    case And(lhs, rhs)                     => p"($lhs && $rhs)"
    case Or(lhs, rhs)                      => p"($lhs || $rhs)"
    case Plus(lhs, rhs)                    => p"($lhs + $rhs)"
    case Minus(lhs, rhs)                   => p"($lhs - $rhs)"
    case LogicAnd(lhs, rhs)                => p"($lhs & $rhs)"
    case LogicOr(lhs, rhs)                 => p"($lhs | $rhs)"
    case LogicXor(lhs, rhs)                => p"($lhs ^ $rhs)"
    case LeftShift(lhs, rhs)               => p"($lhs << $rhs)"
    case RightShift(lhs, rhs)              => p"($lhs >> $rhs)"
    case Times(lhs, rhs)                   => p"($lhs * $rhs)"
    case Div(lhs, rhs)                     => p"($lhs / $rhs)"
    case Modulo(lhs, rhs)                  => p"($lhs % $rhs)"
    case LessThan(lhs, rhs)                => p"($lhs < $rhs)"
    case LessThanEquals(lhs, rhs)          => p"($lhs <= $rhs)"
    case GreaterThan(lhs, rhs)             => p"($lhs > $rhs)"
    case GreaterThanEquals(lhs, rhs)       => p"($lhs >= $rhs)"
    case Equals(lhs, rhs)                  => p"($lhs == $rhs)"
    case NotEquals(lhs, rhs)               => p"($lhs != $rhs)"
    case Is(expr, id)                      => p"($expr is $id)"
    case As(expr, tpe)                     => p"($expr as $tpe)"
    case Not(expr)                         => p"!($expr)"
    case Negation(expr)                    => p"-($expr)"
    case LogicNot(expr)                    => p"~($expr)"
    case Hash(expr)                        => p"#($expr)"
    case ArrayRead(arr, index)             => p"$arr[$index]"
    case ArraySlice(arr, start, end, step) => p"$arr[$start:$end:$step]"
    case NormalAccess(obj, application)    => p"$obj.$application"
    case SafeAccess(obj, application)      => p"$obj?.$application"
    case MethodCall(meth, args)            => p"$meth(${Separated(args, ", ")})"
    case IntLit(value)                     => p"$value"
    case LongLit(value)                    => p"${value}L"
    case FloatLit(value)                   => p"${value}F"
    case DoubleLit(value)                  => p"$value"
    case CharLit(value)                    => p"'${escapeJava(p"$value")}'"
    case StringLit(value)                  => "\"" + p"${escapeJava(p"$value")}" + "\""
    case ArrayLit(expressions)             => p"{ ${Separated(expressions, ", ")} }"
    case TrueLit()                         => p"true"
    case FalseLit()                        => p"false"
    case NullLit()                         => p"null"
    case id@ClassID(value, list)           => p"$value${templateList(id)}"
    case Identifier(value)                 => p"$value"
    case This()                            => p"this"
    case Super(spec)                       => p"super${optional(spec)(s => p"<$s>")}"
    case NewArray(tpe, sizes)              => p"new ${newArray(tpe, sizes)}"
    case New(tpe, exprs)                   => p"new $tpe(${Separated(exprs, ", ")})"
    case PreIncrement(id)                  => p"++$id"
    case PostIncrement(id)                 => p"$id++"
    case PreDecrement(id)                  => p"--$id"
    case PostDecrement(id)                 => p"$id--"
    case Ternary(condition, thn, els)      => p"($condition ? $thn : $els)"
    case Elvis(nullableValue, ifNull)      => p"($nullableValue ?: $ifNull)"
    case ExtractNullable(expr)             => p"$expr!!"
    case Break()                           => p"break"
    case Continue()                        => p"continue"
    case Empty()                           => p"<EMPTY>"
    case GeneratedExpr(stats)              => p"${genExpr(stats)}"
    case PutValue(expr)                    => s"<PutValue(${p"$expr"})>"
  }

  private def restOfClassDecl(id: ClassID, parents: List[ClassID], fields: List[VarDecl], methods: List[MethodDeclTree]): String = {
    val start = p"$id${parentList(parents)}"
    if (fields.isEmpty && methods.isEmpty)
      return s"$start { }"

    if (fields.isEmpty)
      return p"$start $L$N$methods$R"

    if (methods.isEmpty)
      return p"$start $L$N$fields$R$R"

    p"$start $L$N$fields$N$N$methods$R"
  }

  private def imports(imps: List[Import]) = {
    if (imps.isEmpty) ""
    else
      p"${Separated(imps, "\n")}$N"
  }

  private def genExpr(stats: List[StatTree]) = {
    if (stats.size == 1) p"<${stats.head}>"
    else p"<$L$stats$R>"
  }

  private def packDecl(address: List[String]) = {
    if (address.isEmpty) ""
    else
      p"package ${address.mkString("::")}$N"
  }

  private def parentList(parents: List[ClassID]) = {
    if (parents.isEmpty) ""
    else
      p": ${Separated(parents, ", ")}"
  }

  private def newArray(tpe: TypeTree, sizes: List[ExprTree]) = {
    def str(tpe: TypeTree, sizes: List[ExprTree]): String =
      tpe match {
        case NullableType(t) => p"${str(t, sizes)}?"
        case ArrayType(t)    => p"${str(t, sizes.tail)}[${sizes.head}]"
        case t               => p"$t"
      }

    str(tpe, sizes.reverse)
  }

  private def templateList(id: ClassID) = if (id.isTemplated) p"<${Separated(id.templateTypes, ", ")}>" else ""

  private def definition(modifiers: Set[Modifier]) = {
    val decl = modifiers.find(_.isInstanceOf[Accessability]).get match {
      case Private()   => p"def"
      case Public()    => p"Def"
      case Protected() => p"def protected"
    }

    decl + mods(modifiers)
  }

  private def varDecl(modifiers: Set[Modifier]) = {
    val isFinal = modifiers.contains(Final())
    val decl = modifiers.find(_.isInstanceOf[Accessability]).get match {
      case Private() if isFinal   => p"val"
      case Private()              => p"var"
      case Public() if isFinal    => p"Val"
      case Public()               => p"Var"
      case Protected() if isFinal => p"val protected"
      case Protected()            => p"var protected"
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
          case _: VariableID => VarColor(prettyPrint(t))
          case _: MethodID   => MethodColor(prettyPrint(t))
          case _: ClassID    => ClassColor(prettyPrint(t))
          case _: StringLit |
               _: CharLit    => StringColor(prettyPrint(t))
          case _: IntLit |
               _: LongLit |
               _: FloatLit |
               _: DoubleLit  => NumColor(prettyPrint(t))
          case _             => prettyPrint(t)
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

    private def colorKeywords(output: String): String = {
      if (!useColor)
        return output
      KeywordsRegex.replaceAllIn(output, m => {
        Matcher.quoteReplacement(KeywordColor(m.group(1)))
      })
    }
  }

}
