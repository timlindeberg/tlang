package koolc.ast

import koolc.utils.Pipeline
import koolc.utils.Context
import koolc.ast.Trees._
import koolc.utils.Positioned
import com.rits.cloning._

object Templates extends Pipeline[Program, Program] {
  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._

    val cloner = new Cloner
    val templateClasses = prog.classes.filter(_.id.isTemplated)

    object ClassGenerator {
      def apply(typeId: TypeIdentifier): ClassDecl = generateClass(typeId)

      private def generateClass(typeId: TypeIdentifier): ClassDecl = {
        val name = typeId.value
        templateClasses.find(_.id.value == name) match {
          case Some(template) =>
            newTemplateClass(template, typeId.templateTypes)
          case None =>
            error("No template class named \'" + name + "\'.", typeId)
            new ClassDecl(new TypeIdentifier("ERROR"), None, List(), List())
        }
      }

      def constructTemplateMapping(template: ClassDecl, templateTypes: List[TypeTree]): Map[TypeTree, TypeTree] =
        if (template.id.templateTypes.size != templateTypes.size) {
          error("Wrong number of generic parameters, expected "
            + template.id.templateTypes.size + " but found " + templateTypes.size, templateTypes.head)
          Map()
        } else {
          template.id.templateTypes.zip(templateTypes).toMap
        }

      def newTemplateClass(template: ClassDecl, templateTypes: List[TypeTree]): ClassDecl = {
        val typeMap = constructTemplateMapping(template, templateTypes)

        def selectType(t: TypeTree): TypeTree = typeMap.getOrElse(t, t)
        def templateName(id: TypeIdentifier) =
          id.copy(value = template.id.templatedClassName(templateTypes), templateTypes = List())
        def selectTypeOfNewExpr(newExpr: New) = typeMap.get(newExpr.tpe) match {
          case Some(t) if !t.isInstanceOf[TypeIdentifier] =>
            error("Cannot create new instance of primitive type \'" + t.name + "\'.", newExpr.tpe)
            new TypeIdentifier("ERROR", List())
          case x => selectType(newExpr.tpe).asInstanceOf[TypeIdentifier]
        }

        val newClass = cloner.deepClone(template)
        Trees.traverse(newClass, _ match {
          case c @ ClassDecl(id, _, _, _)             => c.id = templateName(id)
          case v @ VarDecl(tpe, _)                    => v.tpe = selectType(tpe)
          case f @ Formal(tpe, _)                     => f.tpe = selectType(tpe)
          case m @ MethodDecl(retType, _, _, _, _, _) => m.retType = selectType(retType)
          case n @ New(tpe)                           => n.tpe = selectTypeOfNewExpr(n)
          case _                                      =>
        })
        newClass
      }
    }

    def findClassesToGenerate: Set[TypeIdentifier] = {
      var classesToGenerate: Set[TypeIdentifier] = Set()
      def addTemplatedType(tpe: TypeTree): Unit = tpe match {
        case x: TypeIdentifier if x.isTemplated => classesToGenerate += x
        case _                                  =>
      }

      Trees.traverse(prog, _ match {
        case c @ ClassDecl(id, parent, _, _) => if(parent.isDefined) addTemplatedType(parent.get)
        case v @ VarDecl(tpe, _)             => addTemplatedType(tpe)
        case f @ Formal(tpe, _)              => addTemplatedType(tpe)
        case n @ New(tpe)                    => addTemplatedType(tpe)
        case _                               =>
      })
      classesToGenerate
    }

    def replaceType(tpe: TypeTree): TypeTree = tpe match {
      case x @ TypeIdentifier(_, _) => replaceTypeId(x)
      case x                        => x
    }

    def replaceTypeId(tpe: TypeIdentifier): TypeIdentifier =
      if (tpe.isTemplated) new TypeIdentifier(tpe.templatedClassName)
      else tpe

    val classesToGenerate = findClassesToGenerate
    val generatedClasses = classesToGenerate.map(ClassGenerator(_)).toList

    val modifiedClasses = (generatedClasses ++ prog.classes.filter(!_.id.isTemplated)).map(x => cloner.deepClone(x))

    modifiedClasses.foreach(classDecl =>
      Trees.traverse(classDecl, _ match {
        case c @ ClassDecl(_, parent, _, _)         => c.parent = parent.map(replaceTypeId)
        case m @ MethodDecl(retType, _, _, _, _, _) => m.retType = replaceType(retType)
        case v @ VarDecl(tpe, _)                    => v.tpe = replaceType(tpe)
        case f @ Formal(tpe, _)                     => f.tpe = replaceType(tpe)
        case n @ New(tpe)                           => if (tpe.isTemplated) n.tpe = new TypeIdentifier(tpe.templatedClassName)
        case _                                      =>
      }))

    val modifiedMain = cloner.deepClone(prog.main)
    Trees.traverse(modifiedMain, _ match {
      case n @ New(tpe) => if (tpe.isTemplated) n.tpe = new TypeIdentifier(tpe.templatedClassName)
      case _            =>
    })

    val p = prog.copy(classes = modifiedClasses, main = modifiedMain)
    println(Printer(p))
    p
  }
}
