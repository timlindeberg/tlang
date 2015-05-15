package koolc.ast

import koolc.utils.Pipeline
import koolc.utils.Context
import koolc.ast.Trees._
import koolc.utils.Positioned

object Templates extends Pipeline[Program, Program] {
  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._

    val templateClasses = prog.classes.filter(_.id.isTemplated)
    var set: Set[String] = Set()
    val classList: List[ClassDecl] = List()

    object Generator {
      def generate(option: Option[Tree], classList: List[ClassDecl]): List[ClassDecl] = if (option.isDefined) generate(option.get, classList) else List()
      def generate(list: List[Tree], classList: List[ClassDecl]): List[ClassDecl] = list.map(generate(_, classList)).flatten
      def generate(t: Tree, classList: List[ClassDecl]): List[ClassDecl] = t match {
        case Program(main, classes) => generate(main, classList) ::: generate(classes, classList)
        case MainObject(id, stats) => generate(stats, classList)
        case ClassDecl(id, parent, vars, methods) =>
          if (!id.isTemplated) generate(parent, classList) ::: generate(vars,classList) ::: generate(methods, classList)
          else List()
        case VarDecl(tpe, id) => tpe match {
            case id @ TypeIdentifier(value, templateTypes) =>
              val templateName = id.templatedClassName(templateTypes)
              if (id.isTemplated && !set.contains(templateName)) {
                set += templateName
                List(generateClass(value, templateTypes, tpe))
              }else{
                List()
              }
            case _ => List()
          }
        case MethodDecl(retType, id, args, vars, stats, retExpr) => List()
        case Formal(tpe, id) => List()
        case Block(stats) => List()
        case If(expr, thn, els) => List()
        case While(expr, stat) => List()
        case Println(expr) => List()
        case Assign(id, expr) => List()
        case ArrayAssign(id, index, expr) => List()
        case And(lhs, rhs) => List()
        case Or(lhs, rhs) => List()
        case Plus(lhs, rhs) => List()
        case Minus(lhs, rhs) => List()
        case Times(lhs, rhs) => List()
        case Div(lhs, rhs) => List()
        case LessThan(lhs, rhs) => List()
        case Equals(lhs, rhs) => List()
        case ArrayRead(arr, index) => List()
        case ArrayLength(arr) => List()
        case MethodCall(obj, meth, args) => List()
        case IntLit(value) => List()
        case StringLit(value) => List()
        case Identifier(value) => List()
        case TypeIdentifier(value, templateTypes) => List()
        case NewIntArray(size) => List()
        case New(tpe) => List()
        case Not(expr) => List()
        case _ => List()
      }

      def generateClass(name: String, templateTypes: List[TypeTree], pos: Positioned): ClassDecl = {
        templateClasses.find(_.id.value == name) match {
          case Some(template) =>
            if (template.id.templateTypes.size != templateTypes.size) {
              error("Wrong number of generic parameters, expected "
                + template.id.templateTypes.size + " but found " + templateTypes.size, pos)
            }

            val typeMap: Map[TypeTree, TypeTree] = template.id.templateTypes.zip(templateTypes).toMap

            val newClass = template.copy(
              id = new TypeIdentifier(template.id.templatedClassName(templateTypes), List()),
              vars = template.vars.map(varDecl =>
                varDecl.copy(tpe = typeMap.getOrElse(varDecl.tpe, varDecl.tpe))),
              methods = template.methods.map(methDecl =>
                methDecl.copy(
                  retType = typeMap.getOrElse(methDecl.retType, methDecl.retType),
                  args = methDecl.args.map(a => a.copy(tpe = typeMap.getOrElse(a.tpe, a.tpe))),
                  vars = methDecl.vars.map(v => v.copy(tpe = typeMap.getOrElse(v.tpe, v.tpe))),
                  stats = methDecl.stats.map(stat => replaceNewDeclarationTypes(stat, typeMap)))))
            newClass
          case None => ???
        }

      }

      def replaceNewDeclarationTypes(t: StatTree, typeMap: Map[TypeTree, TypeTree]): StatTree = {
        object Replacer {
          def f(option: Option[StatTree]): Option[StatTree] = if (option.isDefined) Some(f(option.get)) else None
          def statList(list: List[StatTree]): List[StatTree] = list.map(f)
          def f(t: StatTree): StatTree = t match {
            case Block(stats) => new Block(statList(stats))
            case If(expr, thn, els) => new If(f(expr), f(thn), f(els))
            case While(expr, stat) => new While(f(expr), f(stat))
            case Println(expr) => new Println(f(expr))
            case Assign(id, expr) => new Assign(id, f(expr))
            case ArrayAssign(id, index, expr) => new ArrayAssign(id, f(index), f(expr))
          }

          def exprList(list: List[ExprTree]): List[ExprTree] = list.map(f)
          def f(t: ExprTree): ExprTree = t match {
            case And(lhs, rhs) => new And(f(lhs), f(rhs))
            case Or(lhs, rhs) => new Or(f(lhs), f(rhs))
            case Plus(lhs, rhs) => new Plus(f(lhs), f(rhs))
            case Minus(lhs, rhs) => new Minus(f(lhs), f(rhs))
            case Times(lhs, rhs) => new Times(f(lhs), f(rhs))
            case Div(lhs, rhs) => new Div(f(lhs), f(rhs))
            case LessThan(lhs, rhs) => new LessThan(f(lhs), f(rhs))
            case Equals(lhs, rhs) => new Equals(f(lhs), f(rhs))
            case ArrayRead(arr, index) => new ArrayRead(f(arr), f(index))
            case ArrayLength(arr) => new ArrayLength(f(arr))
            case MethodCall(obj, meth, args) => new MethodCall(f(obj), meth, exprList(args))
            case Not(expr) => new Not(f(expr))
            case n @ New(tpe) => n.copy(tpe = typeMap.getOrElse(tpe, tpe).asInstanceOf[TypeIdentifier])
          }
        }
        Replacer.f(t)
      }
    }

    prog.copy(classes = prog.classes ++ Generator.generate(prog, List()))
  }
}
