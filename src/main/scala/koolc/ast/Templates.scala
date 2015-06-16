package koolc.ast

import koolc.utils.Pipeline
import koolc.utils.Context
import koolc.ast.Trees._
import koolc.utils.Positioned
import com.rits.cloning._
import scala.collection.mutable.ArrayBuffer

object Templates extends Pipeline[Program, Program] {
  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._

    val templateClasses = prog.classes.filter(_.id.isTemplated)

    /* Error messages and predefined
     * error types to return in case of errors. */

    val ERROR_MAP = Map[TypeTree, TypeTree]()
    val ERROR_TYPE = new TypeIdentifier("ERROR")
    val ERROR_CLASS = new ClassDecl(ERROR_TYPE, None, List(), List())

    def ERROR_WRONG_NUM_GENERICS(expected: Int, found: Int, pos: Positioned) = {
      error("Wrong number of generic parameters, expected " + expected + " but found " + found, pos)
      ERROR_MAP
    }

    def ERROR_NEW_PRIMITIVE(name: String, pos: Positioned) = {
      error("Cannot create a new instance of primitive type \'" + name + "\'.", pos)
      ERROR_TYPE
    }

    def ERROR_DOES_NOT_EXIST(name: String, pos: Positioned) = {
      error("No template class named \'" + name + "\'.", pos)
      ERROR_CLASS
    }

    def ERROR_SAME_NAME(name: String, pos: Positioned) = {
      error("Generic identifiers with the same name: \'" + name + "\'", pos)
    }

    object ClassGenerator {

      val cloner = new Cloner
      cloner.registerConstant(Nil)
      cloner.registerConstant(None)
      cloner.registerConstant(Private)
      cloner.registerConstant(Public)
      cloner.registerConstant(Protected)

      var generated: Set[String] = Set()
      var generatedClasses: ArrayBuffer[ClassDecl] = ArrayBuffer()

      def generate(prog: Program): List[ClassDecl] = {

        def generateIfTemplated(tpe: TypeTree): Unit = Some(tpe) collect {
          case x: TypeIdentifier if x.isTemplated =>
            x.templateTypes.foreach(generateIfTemplated)
            generateClass(x)
        }

        def collect(f: Product, p: Product) = Some(p) collect {
          case c: ClassDecl => c.parent.foreach(generateIfTemplated)
          case v: VarDecl   => generateIfTemplated(v.tpe)
          case f: Formal    => generateIfTemplated(f.tpe)
          case n: New       => generateIfTemplated(n.tpe)
        }

        Trees.traverse(prog.classes.filter(!_.id.isTemplated), collect)
        Trees.traverse(prog.main, collect)
        generatedClasses.toList
      }

      private def generateClass(typeId: TypeIdentifier): Unit = {
        if (generated(typeId.templatedClassName))
          return

        generated += typeId.templatedClassName
        templateClasses.find(_.id.value == typeId.value) match {
          case Some(template) => generatedClasses += newTemplateClass(template, typeId.templateTypes)
          case None           => ERROR_DOES_NOT_EXIST(typeId.value, typeId)
        }
      }

      private def newTemplateClass(template: ClassDecl, templateTypes: List[TypeTree]): ClassDecl = {
        val templateMap = constructTemplateMapping(template.id.templateTypes, templateTypes)

        /* Helper functions to perform transformation */
        def updateType(t: TypeTree): TypeTree = {
          Some(t) collect {
            case t @ TypeIdentifier(_, templateTypes) if t.isTemplated =>
              t.templateTypes = templateTypes.map(updateType)
              generateClass(t)
          }
          templateMap.getOrElse(t, t)
        }

        def updateTypeOfNewExpr(newExpr: New) = updateType(newExpr.tpe) match {
          case t: TypeIdentifier => t
          case t                 => ERROR_NEW_PRIMITIVE(t.name, newExpr.tpe)
        }

        def templateName(id: TypeIdentifier) =
          id.copy(value = template.id.templatedClassName(templateTypes), templateTypes = List())

        val newClass = cloner.deepClone(template)
        Trees.traverse(newClass, (_, current) => Some(current) collect {
          case c: ClassDecl       => c.id = templateName(c.id)
          case v: VarDecl         => v.tpe = updateType(v.tpe)
          case f: Formal          => f.tpe = updateType(f.tpe)
          case m: MethodDecl      => m.retType = updateType(m.retType)
          case c: ConstructorDecl => c.id = Identifier(template.id.templatedClassName(templateTypes))
          case n: New             => n.tpe = updateTypeOfNewExpr(n)
        })
        newClass
      }

      private def constructTemplateMapping(templateList: List[TypeTree], templateTypes: List[TypeTree]): Map[TypeTree, TypeTree] = {
        val diff = templateTypes.size - templateList.size
        if (diff != 0) {
          val index = if (diff > 0) templateList.size else templateTypes.size - 1
          ERROR_WRONG_NUM_GENERICS(templateList.size, templateTypes.size, templateTypes(index))
        } else {
          templateList.zip(templateTypes).toMap
        }
      }
    }

    def checkTemplateClassDefs(templateClasses: List[ClassDecl]) =
      templateClasses.foreach { x =>
        var set = Set[TypeTree]()
        var reportedFor = Set[TypeTree]()
        x.id.templateTypes.foreach { x =>
          if (set(x) && !reportedFor(x)) {
            ERROR_SAME_NAME(x.name, x)
            reportedFor += x
          }
          set += x
        }
      }

    def replaceTypes(prog: Program): Program = {
      def replaceType(tpe: TypeTree) = tpe match {
        case x: TypeIdentifier => replaceTypeId(x)
        case x                 => x
      }

      def replaceTypeId(tpe: TypeIdentifier) =
        if (tpe.isTemplated) new TypeIdentifier(tpe.templatedClassName).setPos(tpe)
        else tpe

      Trees.traverse(prog, (_,curr) => Some(curr) collect {
        case c: ClassDecl  => c.parent = c.parent.map(replaceTypeId)
        case m: MethodDecl => m.retType = replaceType(m.retType)
        case v: VarDecl    => v.tpe = replaceType(v.tpe)
        case f: Formal     => f.tpe = replaceType(f.tpe)
        case n: New        => n.tpe = replaceTypeId(n.tpe)
      })
      prog
    }

    checkTemplateClassDefs(templateClasses)
    val newClasses = ClassGenerator.generate(prog)
    val oldClasses = prog.classes.filter(!_.id.isTemplated)
    val newProg = prog.copy(classes = oldClasses ++ newClasses)

    replaceTypes(newProg)
    newProg
  }
}
