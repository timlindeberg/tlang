package tcompiler
package modification

import tcompiler.ast.Trees._
import tcompiler.imports.TemplateImporter
import tcompiler.utils.{Context, Pipeline}

import scala.collection.mutable

object Templates extends Pipeline[List[Program], List[Program]] {

  val StartEnd  = "-"
  val Seperator = "$"

  def run(ctx: Context)(progs: List[Program]): List[Program] = {

    val templateClassGenerator = new TemplateModifier(ctx)
    templateClassGenerator.generateTemplatePrograms(progs)
  }
}

class TemplateModifier(override var ctx: Context) extends TemplateErrors {

  private val templatePrograms    = mutable.Map[String, Program]()
  private var generatedClassNames = mutable.Set[String]()

  def generateTemplatePrograms(progs: List[Program]): List[Program] = {
    //  Add all original template classes
    progs foreach { prog =>
      prog.classes.filter(_.id.isTemplated) foreach { clazz =>
        checkDuplicateTemplateNames(clazz)
        templatePrograms(clazz.id.name) = prog
      }
    }

    // Generate all needed classes
    progs foreach { prog =>
      val templateClassGenerator = new TemplateClassGenerator(prog)
      templateClassGenerator.generateNeededTemplates()
    }

    // all needed classes are generated,
    // construct final program list and filter old classes
    val allPrograms = mutable.Set[Program]()

    // TODO: This is probably pretty expensive
    allPrograms ++= progs
    allPrograms ++= templatePrograms.values

    // Remove all template classes and replace types in rest of the classes
    allPrograms foreach { prog =>
      prog.classes = prog.classes.filter(!_.id.isTemplated)
      replaceTypes(prog)
    }


    allPrograms.toList
  }

  private def replaceTypes(prog: Program): Program = {

    def replaceMaybeType(tpe: Option[TypeTree]) = tpe match {
      case Some(x) => Some(replaceType(x))
      case None    => None
    }

    def replaceType(tpe: TypeTree): TypeTree = tpe match {
      case x: ClassID          => replaceTypeId(x)
      case x@ArrayType(arrTpe) =>
        x.tpe = replaceType(arrTpe)
        x
      case x                   => x
    }

    def replaceTypeId(tpe: ClassID) =
      if (tpe.isTemplated) {
        val newName = tpe.templatedClassName
        val shortName = tpe.name.split("::").last
        if (prog.importMap.contains(shortName)) {
          val fullName = prog.importMap(tpe.name)
          val prefix = fullName.split("\\.").dropRight(1).mkString(".")
          val importEntry = newName -> s"$prefix.$newName"
          prog.importMap += importEntry
        }

        new ClassID(tpe.templatedClassName).setPos(tpe)
      } else {
        tpe
      }



    prog foreach {
      case t: ClassDecl    => t.parents = t.parents map replaceTypeId
      case t: MethodDecl   => t.retType = replaceMaybeType(t.retType)
      case t: OperatorDecl => t.retType = replaceMaybeType(t.retType)
      case t: VarDecl      => t.tpe = replaceMaybeType(t.tpe)
      case t: Formal       => t.tpe = replaceType(t.tpe)
      case t: NewArray     => t.tpe = replaceType(t.tpe)
      case t: New          => t.tpe = replaceType(t.tpe)
      case _               =>
    }
    prog
  }

  private def checkDuplicateTemplateNames(templateClass: ClassDecl) = {
    var seen = Set[TypeTree]()
    var reportedFor = Set[TypeTree]()
    val templateTypes = templateClass.id.templateTypes
    templateTypes foreach { tType =>
      if (seen(tType) && !reportedFor(tType)) {
        ErrorSameName(tType.name, tType)
        reportedFor += tType
      }
      seen += tType
    }
  }

  class TemplateClassGenerator(prog: Program) {

    /**
      * Has the side effect of filling the programs in the template program map with
      * generated template classes.
      */
    def generateNeededTemplates(): Unit = {
      prog foreach {
        case c: ClassDecl => c.parents foreach generateIfTemplated
        case v: VarDecl   => v.tpe collect { case t => generateIfTemplated(t) }
        case f: Formal    => generateIfTemplated(f.tpe)
        case n: New       => generateIfTemplated(n.tpe)
        case _            =>
      }
    }

    private def generateIfTemplated(tpe: TypeTree): Unit =
      tpe match {
        case x: ClassID if x.isTemplated =>
          x.templateTypes foreach generateIfTemplated
          generateClass(x)
        case _                           =>
      }

    private def generateClass(typeId: ClassID): Unit = {
      val shortName = typeId.templatedClassName.split("::").last

      if (generatedClassNames(shortName))
        return

      generatedClassNames += shortName

      // Update import map to include the newly generated class
      updateImportMap(prog, typeId)

      findTemplateProgram(typeId) match {
        case Some(templateProgram) =>
          val shortName = typeId.name.split("::").last
          val templateClass = templateProgram.classes.find(_.id.name == shortName).get
          val generatedClass = newTemplateClass(templateClass, typeId)
          templateProgram.classes ::= generatedClass

        case None => ErrorDoesNotExist(typeId.name, typeId)
      }
    }

    private def updateImportMap(prog: Program, typeId: ClassID) =
      if (prog.importMap.contains(typeId.name)) {
        val className = typeId.templatedClassName
        val fullName = prog.importMap(typeId.name)
        val prefix = fullName.split("\\.").dropRight(1).mkString(".")
        prog.importMap(className) = s"$prefix.$className"
      }

    private def findTemplateProgram(typeId: ClassID): Option[Program] = {
      val className = typeId.name.split("::").last
      if (templatePrograms.contains(className))
        return Some(templatePrograms(className))

      val templateImporter = new TemplateImporter(ctx)
      val importName = prog.importName(typeId)
      val importedPrograms = templateImporter.importPrograms(importName)

      importedPrograms foreach { prog =>
        prog.classes foreach { clazz =>
          templatePrograms(clazz.id.name) = prog
        }
      }


      templatePrograms.get(className)
    }

    private def newTemplateClass(template: ClassDecl, typeId: ClassID): ClassDecl = {
      checkDuplicateTemplateNames(template)

      val templateTypes = typeId.templateTypes
      val templateMap = constructTemplateMapping(typeId, template.id.templateTypes, templateTypes)

      /* Helper functions to perform transformation */
      def updateType(t: TypeTree): TypeTree = {
        t match {
          case t@ClassID(_, templateTypes) if t.isTemplated =>
            t.templateTypes = templateTypes.map(updateType)
            generateClass(t)
          case a@ArrayType(tpe)                             =>
            a.tpe = updateType(tpe)
          case _                                            =>
        }
        templateMap.getOrElse(t, t)
      }

      val newClass = template.copyTree()
      newClass foreach {
        case t: ClassDecl    =>
          val templateName = template.id.templatedClassName(templateTypes)
          t.id = t.id.copy(name = templateName, templateTypes = List())
          t.parents = t.parents.map(p => updateType(p).asInstanceOf[ClassID])
        case v: VarDecl      => v.tpe collect { case t => v.tpe = Some(updateType(t)) }
        case f: Formal       => f.tpe = updateType(f.tpe)
        case m: MethodDecl   => m.retType collect { case t => m.retType = Some(updateType(t)) }
        case o: OperatorDecl => o.retType collect { case t => o.retType = Some(updateType(t)) }
        case n: New          => n.tpe = updateType(n.tpe)
        case n: NewArray     => n.tpe = updateType(n.tpe)
        case a: As           => a.tpe = updateType(a.tpe)
        case _               =>
      }
      newClass
    }



    private def constructTemplateMapping(typedId: ClassID, templateList: List[TypeTree], templateTypes: List[TypeTree]): Map[TypeTree, TypeTree] = {
      if (templateTypes.size != templateList.size) {
        ErrorWrongNumGenerics(templateList.size, templateTypes.size, typedId)
        Map()
      } else {
        templateList.zip(templateTypes).toMap
      }
    }
  }

}

