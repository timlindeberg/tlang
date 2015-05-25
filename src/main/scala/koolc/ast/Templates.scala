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
    val cloner = new Cloner
    var generated: Set[String] = Set()
    var generatedClasses: List[ClassDecl] = List()

    /* Error messages and predefined
     * error types to return in case of errors. */

    val ERROR_MAP = Map[TypeTree, TypeTree]()
    val ERROR_TYPE = new TypeIdentifier("ERROR")
    val ERROR_CLASS = new ClassDecl(ERROR_TYPE, None, List(), List())
    val ERROR_METH = new MethodDecl(ERROR_TYPE, new Identifier("ERROR"), List(), List(), List(), new New(ERROR_TYPE))

    def ERROR_WRONG_NUM_GENERICS(expected: Int, found: Int, pos: Positioned) = {
      fatal("Wrong number of generic parameters, expected " + expected + " but found " + found, pos)
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
      def apply(typeId: TypeIdentifier): Unit = generateClass(typeId)

      private def generateClass(typeId: TypeIdentifier): Unit = {
        if (generated.contains(typeId.templatedClassName)) {
          return ;
        }
        generated += typeId.templatedClassName
        templateClasses.find(_.id.value == typeId.value) match {
          case Some(template) => generatedClasses = newTemplateClass(template, typeId.templateTypes) :: generatedClasses
          case None           => ERROR_DOES_NOT_EXIST(typeId.value, typeId)
        }
      }

      private def newTemplateClass(template: ClassDecl, templateTypes: List[TypeTree]): ClassDecl = {
        val typeMap = constructTemplateMapping(template.id.templateTypes, templateTypes)
        /* Helper functions to perform transformation */
        def updateType(t: TypeTree): TypeTree = {
          t match {
            case t: TypeIdentifier =>
              if (t.isTemplated) {
                var newList: ArrayBuffer[TypeTree] = new ArrayBuffer()
                t.templateTypes.foreach(x => newList += updateType(x))
                t.templateTypes = newList.toList
                generateClass(t)
              }
            case _ =>
          }
          typeMap.getOrElse(t, t)
        }

        def templateName(id: TypeIdentifier) =
          id.copy(value = template.id.templatedClassName(templateTypes), templateTypes = List())

        def updateTypeOfNewExpr(newExpr: New) = {
          newExpr.tpe match {
            case t: TypeIdentifier =>
              if (t.isTemplated) {
                var newList: ArrayBuffer[TypeTree] = new ArrayBuffer()
                t.templateTypes.foreach(x => newList += updateType(x))
                t.templateTypes = newList.toList
                generateClass(t)
              }
            case _ =>
          }
          typeMap.get(newExpr.tpe) match {
            case Some(t: TypeIdentifier) => updateType(t).asInstanceOf[TypeIdentifier]
            case Some(t)                 => ERROR_NEW_PRIMITIVE(t.name, newExpr.tpe)
            case None                    => newExpr.tpe
          }
        }

        val newClass = cloner.deepClone(template)
        Trees.traverse(newClass, Some(_) collect {
          case c: ClassDecl  => c.id = templateName(c.id)
          case v: VarDecl    => v.tpe = updateType(v.tpe)
          case f: Formal     => f.tpe = updateType(f.tpe)
          case m: MethodDecl => m.retType = updateType(m.retType)
          case n: New        => n.tpe = updateTypeOfNewExpr(n)
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

    def findClassesToGenerate: Set[TypeIdentifier] = {
      var classesToGenerate: Set[TypeIdentifier] = Set()
      def addTemplatedType(tpe: TypeTree): Unit = Some(tpe) collect {
        case x: TypeIdentifier if x.isTemplated =>
          x.templateTypes.foreach(addTemplatedType(_))
          classesToGenerate += x
      }
      def collect(p: Product) = Some(p) collect {
        case c: ClassDecl => c.parent.foreach(addTemplatedType)
        case v: VarDecl   => addTemplatedType(v.tpe)
        case f: Formal    => addTemplatedType(f.tpe)
        case n: New       => addTemplatedType(n.tpe)
      }
      Trees.traverse(prog.classes.filter(!_.id.isTemplated), collect)
      Trees.traverse(prog.main, collect)
      classesToGenerate
    }

    def replaceType(tpe: TypeTree) = tpe match {
      case x: TypeIdentifier => replaceTypeId(x)
      case x                 => x
    }

    def replaceTypeId(tpe: TypeIdentifier) =
      if (tpe.isTemplated) new TypeIdentifier(tpe.templatedClassName).setPos(tpe)
      else tpe

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

    /* Generate templates.
     * Step 1: Check wether template classes are legal.
     * Step 2: Find which classes need to be generated.
     * Step 3: Generate the new classes and remove the templates.
     * Step 3: Replace references to classes by the templated
     * name eg. Foo[Int, String] becomes var -Foo$Int$String-
     */

    val classesToGenerate = findClassesToGenerate
    checkTemplateClassDefs(templateClasses)
    classesToGenerate.foreach(ClassGenerator(_))
    val modifiedClasses = generatedClasses.toList ++ prog.classes.filter(!_.id.isTemplated)
    val newProg = prog.copy(classes = modifiedClasses)

    Trees.traverse(newProg, Some(_) collect {
      case c: ClassDecl  => c.parent = c.parent.map(replaceTypeId)
      case m: MethodDecl => m.retType = replaceType(m.retType)
      case v: VarDecl    => v.tpe = replaceType(v.tpe)
      case f: Formal     => f.tpe = replaceType(f.tpe)
      case n: New        => n.tpe = replaceTypeId(n.tpe)
    })

    newProg
  }
}
