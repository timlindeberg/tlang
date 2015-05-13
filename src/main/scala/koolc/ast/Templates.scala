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

    object Generator {
      def generate(option: Option[Tree]): Unit = if (option.isDefined) generate(option.get)
      def generate(list: List[Tree]): Unit = list.foreach(generate)
      def generate(t: Tree): Unit = t match {
        case Program(main, classes) =>
          generate(main)
          generate(classes)
        case MainObject(id, stats) => generate(stats)
        case ClassDecl(id, parent, vars, methods) =>
          if (!id.isTemplated) {
            generate(parent)
            generate(vars)
            generate(methods)
          }
        case VarDecl(tpe, id) =>
          tpe match {
            case id @ TypeIdentifier(value, templateTypes) =>
              val templateName = id.templatedClassName
              if(id.isTemplated && !set.contains(templateName)){
                set += templateName
                val lol = generateClass(value, templateTypes, tpe)
                  
                prog.classes ++= List(lol)
                println(Printer(lol))
              }
            case _ =>
          }

        case MethodDecl(retType, id, args, vars, stats, retExpr) => 
        case Formal(tpe, id) => 
        case Block(stats) => 
        case If(expr, thn, els) => 
        case While(expr, stat) => 
        case Println(expr) => 
        case Assign(id, expr) => 
        case ArrayAssign(id, index, expr) => 
        case And(lhs, rhs) => 
        case Or(lhs, rhs) => 
        case Plus(lhs, rhs) => 
        case Minus(lhs, rhs) => 
        case Times(lhs, rhs) => 
        case Div(lhs, rhs) => 
        case LessThan(lhs, rhs) => 
        case Equals(lhs, rhs) => 
        case ArrayRead(arr, index) => 
        case ArrayLength(arr) => 
        case MethodCall(obj, meth, args) => 
        case IntLit(value) => 
        case StringLit(value) => 
        case Identifier(value) => 
        case TypeIdentifier(value, templateTypes) => 
        case NewIntArray(size) => 
        case New(tpe) => 
        case Not(expr) => 
        case _ =>
      }

      def generateClass(name: String, templateTypes: List[TypeTree], pos: Positioned): ClassDecl = {
        templateClasses.find(_.id.value == name) match {
          case Some(template) =>
            if (template.id.templateTypes.size != templateTypes.size) {
              error("Wrong number of generic parameters, expected "
                + template.id.templateTypes.size + " but found " + templateTypes.size, pos)
            }

            val typeMap: Map[TypeTree, TypeTree] = template.id.templateTypes.zip(templateTypes).toMap

            val newClass = template.copy()
            newClass.vars.foreach(varDecl =>
              if (typeMap.contains(varDecl.tpe)) varDecl.tpe = typeMap(varDecl.tpe)
            )
             
            newClass.methods.foreach(methDecl => {
              if (typeMap.contains(methDecl.retType)) methDecl.retType = typeMap(methDecl.retType)
              methDecl.args.foreach(arg => {
                if (typeMap.contains(arg.tpe)) arg.tpe = typeMap(arg.tpe)
              })
              methDecl.vars.foreach(varDecl =>
                if (typeMap.contains(varDecl.tpe)) varDecl.tpe = typeMap(varDecl.tpe))
              methDecl.stats.foreach(stat => {
                replaceNewDeclarationTypes(stat, typeMap)
              })

            })
            newClass.id.value = newClass.id.templatedClassName
            newClass.id.templateTypes = List()
            newClass

          case None => ???
        }

      }

      def replaceNewDeclarationTypes(t: Tree, typeMap: Map[TypeTree, TypeTree]): Unit = {

        object Replacer {
          def f(option: Option[Tree]): Unit = if (option.isDefined) f(option.get)
          def f(list: List[Tree]): Unit = list.foreach(f)
          def f(t: Tree): Unit = t match {
            case Block(stats) => f(stats)
            case If(expr, thn, els) =>
              f(expr); f(thn); f(els)
            case While(expr, stat) =>
              f(expr); f(stat)
            case Println(expr) => f(expr)
            case Assign(id, expr) =>
              f(id); f(expr)
            case ArrayAssign(id, index, expr) =>
              f(id); f(index); f(expr)
            case And(lhs, rhs) =>
              f(lhs); f(rhs)
            case Or(lhs, rhs) =>
              f(lhs); f(rhs)
            case Plus(lhs, rhs) =>
              f(lhs); f(rhs)
            case Minus(lhs, rhs) =>
              f(lhs); f(rhs)
            case Times(lhs, rhs) =>
              f(lhs); f(rhs)
            case Div(lhs, rhs) =>
              f(lhs); f(rhs)
            case LessThan(lhs, rhs) =>
              f(lhs); f(rhs)
            case Equals(lhs, rhs) =>
              f(lhs); f(rhs)
            case ArrayRead(arr, index) =>
              f(arr); f(index)
            case ArrayLength(arr) => f(arr)
            case MethodCall(obj, meth, args) =>
              f(obj); f(meth); f(args)
            case Not(expr) => f(expr)
            case n @ New(tpe) =>
              if (typeMap.contains(tpe)) n.tpe = typeMap(tpe).asInstanceOf[TypeIdentifier]
            case _ =>
          }
        }
        Replacer.f(t)
      }
    }

    Generator.generate(prog)
    prog
  }
}
