package koolc.ast

import koolc.utils.Pipeline
import koolc.utils.Context
import koolc.ast.Trees._
import koolc.utils.Positioned

object Templates extends Pipeline[Program, Program] {
  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._

    val templateClasses = prog.classes.filter(_.id.isTemplated)

    def generateClass(typeId: TypeIdentifier): ClassDecl = {
      val name = typeId.value
      val templateTypes = typeId.templateTypes
      templateClasses.find(_.id.value == name) match {
        case Some(template) =>
          if (template.id.templateTypes.size != templateTypes.size) {
            error("Wrong number of generic parameters, expected "
              + template.id.templateTypes.size + " but found " + templateTypes.size, typeId)
          }
          val typeMap: Map[TypeTree, TypeTree] = template.id.templateTypes.zip(templateTypes).toMap

          val newClass = template.copy(
            id = new TypeIdentifier(template.id.templatedClassName(templateTypes), List()),
            vars = template.vars.map(varDecl =>
              varDecl.copy(tpe = typeMap.getOrElse(varDecl.tpe, varDecl.tpe), id = varDecl.id.copy())),
            methods = template.methods.map(methDecl =>
              methDecl.copy(
                retType = typeMap.getOrElse(methDecl.retType, methDecl.retType),
                args = methDecl.args.map(a => a.copy(tpe = typeMap.getOrElse(a.tpe, a.tpe), id = a.id.copy())),
                vars = methDecl.vars.map(v => v.copy(tpe = typeMap.getOrElse(v.tpe, v.tpe), id = v.id.copy())),
                stats = methDecl.stats.map(stat => replaceNewDeclarationTypesInStatement(stat, typeMap)),
                retExpr = replaceNewDeclarationTypesInExpr(methDecl.retExpr, typeMap))))
          newClass
        case None =>
          error("No template class named \'" + name + "\'.", typeId)
          new ClassDecl(new TypeIdentifier("ERROR", List()), None, List(), List())
      }

    }

    def replaceNewDeclarationTypesInStatement(t: StatTree, typeMap: Map[TypeTree, TypeTree]): StatTree = {
      new Replacer(typeMap).f(t)
    }

    def replaceNewDeclarationTypesInExpr(t: ExprTree, typeMap: Map[TypeTree, TypeTree]): ExprTree = {
      new Replacer(typeMap).f(t)
    }

    class Replacer(typeMap: Map[TypeTree, TypeTree]) {
      def f(option: Option[StatTree]): Option[StatTree] = if (option.isDefined) Some(f(option.get)) else None
      def statList(list: List[StatTree]): List[StatTree] = list.map(f)

      def f(t: StatTree): StatTree = t match {
        case Block(stats)                 => new Block(statList(stats))
        case If(expr, thn, els)           => new If(f(expr), f(thn), f(els))
        case While(expr, stat)            => new While(f(expr), f(stat))
        case Println(expr)                => new Println(f(expr))
        case Assign(id, expr)             => new Assign(id, f(expr))
        case ArrayAssign(id, index, expr) => new ArrayAssign(id, f(index), f(expr))
      }

      def exprList(list: List[ExprTree]): List[ExprTree] = list.map(f)
      def f(t: ExprTree): ExprTree = t match {
        case And(lhs, rhs)               => new And(f(lhs), f(rhs))
        case Or(lhs, rhs)                => new Or(f(lhs), f(rhs))
        case Plus(lhs, rhs)              => new Plus(f(lhs), f(rhs))
        case Minus(lhs, rhs)             => new Minus(f(lhs), f(rhs))
        case Times(lhs, rhs)             => new Times(f(lhs), f(rhs))
        case Div(lhs, rhs)               => new Div(f(lhs), f(rhs))
        case LessThan(lhs, rhs)          => new LessThan(f(lhs), f(rhs))
        case Equals(lhs, rhs)            => new Equals(f(lhs), f(rhs))
        case ArrayRead(arr, index)       => new ArrayRead(f(arr), f(index))
        case ArrayLength(arr)            => new ArrayLength(f(arr))
        case MethodCall(obj, meth, args) => new MethodCall(f(obj), meth, exprList(args))
        case Not(expr)                   => new Not(f(expr))
        case NewIntArray(size)           => new NewIntArray(f(size))
        case n @ New(tpe) =>
          val t = typeMap.get(tpe)
          if (t.isDefined && !t.get.isInstanceOf[TypeIdentifier]) {
            error("Cannot create new instance of primitive type \'" + t.get.name + "\'.", n)
            n.copy(tpe = new TypeIdentifier("ERROR", List()))
          } else {
            n.copy(tpe = typeMap.getOrElse(tpe, tpe).asInstanceOf[TypeIdentifier])
          }
        case Identifier(value)    => new Identifier(value)
        case x                    => x
      }
    }

    def replaceTypeNames(t: Tree): Tree = {
      object TypeReplacer {
        def f(option: Option[StatTree]): Option[StatTree] = if (option.isDefined) Some(f(option.get)) else None
        def statList(list: List[StatTree]): List[StatTree] = list.map(f)

        def f(t: StatTree): StatTree = t match {
          case Block(stats)                 => new Block(statList(stats))
          case If(expr, thn, els)           => new If(f(expr), f(thn), f(els))
          case While(expr, stat)            => new While(f(expr), f(stat))
          case Println(expr)                => new Println(f(expr))
          case Assign(id, expr)             => new Assign(id, f(expr))
          case ArrayAssign(id, index, expr) => new ArrayAssign(id, f(index), f(expr))
        }

        def exprList(list: List[ExprTree]): List[ExprTree] = list.map(f)
        def f(t: ExprTree): ExprTree = t match {
          case And(lhs, rhs)               => new And(f(lhs), f(rhs))
          case Or(lhs, rhs)                => new Or(f(lhs), f(rhs))
          case Plus(lhs, rhs)              => new Plus(f(lhs), f(rhs))
          case Minus(lhs, rhs)             => new Minus(f(lhs), f(rhs))
          case Times(lhs, rhs)             => new Times(f(lhs), f(rhs))
          case Div(lhs, rhs)               => new Div(f(lhs), f(rhs))
          case LessThan(lhs, rhs)          => new LessThan(f(lhs), f(rhs))
          case Equals(lhs, rhs)            => new Equals(f(lhs), f(rhs))
          case ArrayRead(arr, index)       => new ArrayRead(f(arr), f(index))
          case ArrayLength(arr)            => new ArrayLength(f(arr))
          case MethodCall(obj, meth, args) => new MethodCall(f(obj), meth, exprList(args))
          case Not(expr)                   => new Not(f(expr))
          case n @ New(tpe) =>
            if (tpe.isTemplated) {
              n.copy(tpe = new TypeIdentifier(tpe.templatedClassName, List()))
            } else {
              new New(tpe)
            }
          case x => x
        }
      }
      t match {
        case e: ExprTree => TypeReplacer.f(e)
        case e: StatTree => TypeReplacer.f(e)
        case e => e
      }
    }

    var classesToGenerate: Set[TypeIdentifier] = Set()
    def addTemplatedType(tpe: TypeTree): Unit = tpe match {
      case x: TypeIdentifier => if (x.isTemplated) classesToGenerate += x
      case _                 =>
    }

    traverse(prog, _ match {
      case varDecl @ VarDecl(tpe, id) => addTemplatedType(tpe)
      case formal @ Formal(tpe, id)   => addTemplatedType(tpe)
      case n @ New(tpe)               => addTemplatedType(tpe)
      case x                          =>
    })

    val generatedClasses = classesToGenerate.map(generateClass).toList
    val modifiedClasses = (generatedClasses ++ prog.classes.filter(!_.id.isTemplated)).map(classDecl =>
      classDecl.copy(
        parent = classDecl.parent.map(x => replaceType(x)),
        vars = classDecl.vars.map(varDecl =>
          varDecl.copy(tpe = replaceType(varDecl.tpe))),
        methods = classDecl.methods.map(methDecl =>
          methDecl.copy(
            retType = replaceType(methDecl.retType),
            args = methDecl.args.map(a => a.copy(tpe = replaceType(a.tpe))),
            vars = methDecl.vars.map(v => v.copy(tpe = replaceType(v.tpe))),
            stats = methDecl.stats.map(stat => replaceTypeNames(stat).asInstanceOf[StatTree]),
            retExpr = replaceTypeNames(methDecl.retExpr).asInstanceOf[ExprTree]))))

    prog.copy(classes = modifiedClasses)
  }

  def replaceType(tpe: TypeTree): TypeTree = tpe match {
    case x @ TypeIdentifier(value, templateTypes) =>
      replaceType(x)
    case x => x
  }

  def replaceType(tpe: TypeIdentifier): TypeIdentifier =
    if (tpe.isTemplated) {
      new TypeIdentifier(tpe.templatedClassName, List())
    } else {
      tpe
    }

  def traverse(t: Product, f: Product => Unit): Unit = {
    t.productIterator.foreach(_ match {
      case x: List[_] =>
        x.foreach(_ match {
          case x: Product =>
            traverse(x, f)
          case _ =>
        })
      case x: Option[Any] =>
        if (x.isDefined) {
          x match {
            case x: Product =>
              traverse(x, f)
            case _ =>
          }
        }
      case x: Product =>
        traverse(x, f)
      case _ =>
    })
    f(t)
  }

}
