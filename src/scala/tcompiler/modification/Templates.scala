package tcompiler
package modification

import tcompiler.ast.{TreeCopier, TreeTransformer}
import tcompiler.ast.Trees._
import tcompiler.imports.{ImportMap, TemplateImporter}
import tcompiler.utils.{Context, Pipeline}

import scala.collection.mutable

object Templates extends Pipeline[List[CompilationUnit], List[CompilationUnit]] {

  val StartEnd  = "-"
  val Seperator = "$"

  def run(ctx: Context)(cus: List[CompilationUnit]): List[CompilationUnit] = {

    val templateClassGenerator = new TemplateModifier(ctx)
    templateClassGenerator.generateTemplatePrograms(cus)
  }
}

class TemplateModifier(override var ctx: Context) extends TemplateErrors {

  override var importMap: ImportMap = new ImportMap(ctx)
  private  val templateCus          = mutable.Map[String, CompilationUnit]()
  private  var generatedClassNames  = mutable.Set[String]()

  def generateTemplatePrograms(cus: List[CompilationUnit]): List[CompilationUnit] = {
    //  Add all original template classes
    cus foreach { cu =>
      importMap = cu.importMap
      cu.classes.filter(_.id.isTemplated) foreach { clazz =>
        checkDuplicateTemplateNames(clazz)
        templateCus(clazz.id.name) = cu
      }
    }

    // Generate all needed classes
    cus foreach { cu =>
      importMap = cu.importMap
      val templateClassGenerator = new TemplateClassGenerator(cu)
      templateClassGenerator.generateNeededTemplates()
    }

    // all needed classes are generated,
    // construct final program list and filter old classes
    val allCus = mutable.Set[CompilationUnit]()

    // TODO: This is probably pretty expensive
    allCus ++= cus
    allCus ++= templateCus.values

    // Remove all template classes and replace types in rest of the classes
    val replaced = allCus map { cu =>
      cu.classes = cu.classes.filter(!_.id.isTemplated)
      replaceTypes(cu)
    }

    replaced.toList
  }

  private def replaceTypes(cu: CompilationUnit): CompilationUnit = {
    // Replace types with their templated class names, eg.
    // replace Map<Int, String> with -Map$Int$String-.
    val replacer = new TreeTransformer {
      override def transform(t: Tree): Tree = t match {
        case tpe: ClassID if tpe.isTemplated =>
          val newName = tpe.templatedClassName
          val shortName = tpe.name.split("::").last
          if (cu.importMap.contains(shortName)) {
            val fullName = cu.importMap.getFullName(tpe.name)
            val prefix = fullName.split("\\.").dropRight(1).mkString(".")
            cu.importMap.addImport(newName, s"$prefix.$newName")
          }

          treeCopy.ClassID(tpe, newName)
        case _                               => super.transform(t)
      }

    }

    replacer.transform(cu).asInstanceOf[CompilationUnit]
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

  class TemplateClassGenerator(cu: CompilationUnit) {

    /**
      * Has the side effect of filling the programs in the template program map with
      * generated template classes.
      */
    def generateNeededTemplates(): Unit = {
      cu foreach {
        case c: ClassID if c.isTemplated => generateClass(c)
        case _                           =>
      }
    }

    private def generateClass(typeId: ClassID): Unit = {
      val shortName = typeId.templatedClassName.split("::").last

      if (generatedClassNames(shortName))
        return

      generatedClassNames += shortName

      // Update import map to include the newly generated class
      updateImportMap(cu.importMap, typeId)

      findTemplateCU(typeId) match {
        case Some(templateCU) =>
          val shortName = typeId.name.split("::").last
          val templateClass = templateCU.classes.find(_.id.name == shortName).get
          val generatedClass = newTemplateClass(templateClass, typeId)
          templateCU.classes ::= generatedClass

        case None => ErrorDoesNotExist(typeId.name, typeId)
      }
    }

    private def updateImportMap(importMap: ImportMap, typeId: ClassID) =
      if (importMap.contains(typeId.name)) {
        val className = typeId.templatedClassName
        val fullName = importMap.getFullName(typeId.name)
        val prefix = fullName.split("\\.").dropRight(1).mkString(".")
        importMap.addImport(className, s"$prefix.$className")
      }

    private def findTemplateCU(typeId: ClassID): Option[CompilationUnit] = {
      val className = typeId.name.split("::").last
      if (templateCus.contains(className))
        return Some(templateCus(className))

      val templateImporter = new TemplateImporter(ctx)
      val importName = cu.importMap.importName(typeId)
      val importedCus = templateImporter.importCus(importName)

      importedCus foreach { cu =>
        cu.classes foreach { clazz =>
          templateCus(clazz.id.name) = cu
        }
      }


      templateCus.get(className)
    }

    private def newTemplateClass(template: ClassDecl, typeId: ClassID): ClassDecl = {
      checkDuplicateTemplateNames(template)

      val templateTypes = typeId.templateTypes
      val templateMap = constructTemplateMapping(typeId, template.id.templateTypes, templateTypes)

      val templateTransformer = new TreeTransformer {
        // uses a strict copier so we recieve an actual copy of the tree
        // TODO: this might not actually be needed if immutability is enforced
        override val treeCopy = new TreeCopier

        override def transform(t: Tree) = t match {
          case c@ClassDecl(id, parents, fields, methods, isAbstract) =>
            // Update the name of the templated class
            val templateName = template.id.templatedClassName(templateTypes)
            val newId = treeCopy.ClassID(id, templateName, Nil)
            treeCopy.ClassDecl(c, newId, tr(parents), tr(fields), tr(methods), isAbstract)
          case c@ClassID(name, tTypes)                               =>
            val newId = treeCopy.ClassID(c, name, tr(tTypes))
            if (c.isTemplated)
              generateClass(newId)
            templateMap.getOrElse(newId, newId)
          case _                                                     => super.transform(t)
        }
      }
      templateTransformer.transform(template).asInstanceOf[ClassDecl]
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

