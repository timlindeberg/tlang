package tlang.compiler
package modification

import tlang.compiler.ast.Trees
import tlang.compiler.ast.Trees._
import tlang.compiler.imports.{ImportMap, TemplateImporter}
import tlang.utils.Extensions._

import scala.collection.mutable

object Templates extends Pipeline[CompilationUnit, CompilationUnit] {

  val StartEnd  = "-"
  val Seperator = "$"

  def run(ctx: Context)(cus: List[CompilationUnit]): List[CompilationUnit] = {
    val templateClassGenerator = TemplateModifier(ctx)
    templateClassGenerator.generateTemplates(cus)
  }
}

case class TemplateModifier(ctx: Context) {

  private val templateCus         = mutable.Map[String, CompilationUnit]()
  private var generatedClassNames = mutable.Set[String]()

  def generateTemplates(cus: List[CompilationUnit]): List[CompilationUnit] = {
    //  Add all original template classes
    cus foreach { cu =>
      cu.classes.filterInstance[IDClassDeclTree].filter(_.id.isTemplated) foreach { clazz =>
        templateCus(clazz.id.name) = cu
      }
    }

    // Generate all needed classes
    cus foreach { cu =>
      val templateClassGenerator = TemplateClassGenerator(ctx, cu)
      templateClassGenerator()
    }

    // all needed classes are generated,
    // construct final program list and filter old classes
    val allCus = mutable.Set[CompilationUnit]()

    allCus ++= cus
    allCus ++= templateCus.values

    // Remove all template classes and replace types in rest of the classes
    val replaced = allCus map { cu =>
      cu.classes = cu.classes.filterInstance[IDClassDeclTree].filter(!_.id.isTemplated) ++ cu.classes.filterInstance[ExtensionDecl]
      replaceTypes(cu)
    }

    replaced.toList
  }

  private def replaceTypes(cu: CompilationUnit): CompilationUnit = {
    // Replace types with their templated class names, eg.
    // replace Map<Int, String> with -Map$Int$String-.
    val replace = new Trees.Transformer {
      override def _transform(t: Tree): Tree = t match {
        case tpe: ClassID if tpe.isTemplated =>
          val shortName = tpe.name.split("::").last
          if (cu.importMap.contains(shortName)) {
            val entry = getImportEntry(cu.importMap, tpe)
            cu.importMap.addImport(entry)
          }
          treeCopy.ClassID(tpe, tpe.templatedClassName)
        case _                               => super._transform(t)
      }

    }

    replace(cu)
  }


  private def getImportEntry(importMap: ImportMap, classId: ClassID) = {
    val templateName = classId.templatedClassName
    val fullName = importMap.getFullName(classId.name)
    val prefix = fullName.split("::").dropRight(1).mkString("::")
    templateName -> s"$prefix::$templateName"
  }

  case class TemplateClassGenerator(override val ctx: Context, cu: CompilationUnit) extends TemplateErrors {

    override val importMap: ImportMap = cu.importMap

    /**
      * Has the side effect of filling the programs in the template program map with
      * generated template classes.
      */
    def apply(): Unit = {
      cu.classes.filterInstance[IDClassDeclTree].filter(_.id.isTemplated) foreach checkDuplicateTemplateNames
      
      val traverser = new Trees.Traverser {
        override def _traverse(t: Tree): Unit = t match {
          case ClassDeclTree(_, parents, fields, methods) =>
            // Ignore the id of classdecls since these can declare templated types
            // which should not be generated
            _traverse(parents)
            _traverse(fields)
            _traverse(methods)
          case c: ClassID if c.isTemplated                => generateClass(c)
          case _                                          => super._traverse(t)
        }
      }
      traverser.traverse(cu)
    }

    private def generateClass(typeId: ClassID): Unit = {
      val shortName = typeId.templatedClassName.split("::").last

      if (generatedClassNames(shortName))
        return

      generatedClassNames += shortName

      // Update import map to include the newly generated class
      if (cu.importMap.contains(typeId.name)) {
        val entry = getImportEntry(cu.importMap, typeId)
        cu.importMap.addImport(entry)
      }

      findTemplateCU(typeId) match {
        case Some(templateCU) =>
          typeId.templateTypes.foreach {
            case classId: ClassID =>
              // Update the import map with the instantiated types:
              // e.g. Iterator<Vector<Int>> updates the
              // Iterator's import map with Vector<Int> -> T.std.Vector<Int>
              updateImportMap(templateCU.importMap, classId)
            case _                =>
          }
          val generatedClass = newTemplateClass(templateCU, typeId)
          templateCU.classes ::= generatedClass
        case None             => report(ClassDoesNotExist(typeId.name, typeId))
      }
    }

    private def checkDuplicateTemplateNames(templateClass: IDClassDeclTree) = {
      var seen = Set[TypeTree]()
      var reportedFor = Set[TypeTree]()
      val templateTypes = templateClass.id.templateTypes
      templateTypes foreach { tType =>
        if (seen(tType) && !reportedFor(tType)) {
          report(SameName(tType.name, tType))
          reportedFor += tType
        }
        seen += tType
      }
    }

    private def updateImportMap(importMap: ImportMap, classId: ClassID) = {
      // Add the classId to the import map
      val templateName = classId.templatedClassName
      if (cu.importMap.contains(templateName)) {
        val fullName = cu.importMap.getFullName(templateName)
        importMap.addImport(templateName, fullName)
      } else if (cu.importMap.contains(classId.name)) {
        val entry = getImportEntry(cu.importMap, classId)
        importMap.addImport(entry)
      }
    }


    private def findTemplateCU(typeId: ClassID): Option[CompilationUnit] = {
      val className = typeId.name.split("::").last
      if (templateCus.contains(className))
        return Some(templateCus(className))

      val templateImporter = new TemplateImporter(ctx)
      val importName = cu.importMap.getFullName(typeId.name)
      val importedCus = templateImporter.importCus(importName)

      importedCus foreach { cu =>
        cu.classes.filterInstance[IDClassDeclTree] foreach { clazz =>
          templateCus(clazz.id.name) = cu
        }
      }


      templateCus.get(className)
    }

    private def newTemplateClass(templateCU: CompilationUnit, typeId: ClassID): ClassDeclTree = {
      val shortName = typeId.name.split("::").last
      val template = templateCU.classes.filterInstance[IDClassDeclTree].find(_.id.name == shortName).get

      checkDuplicateTemplateNames(template)

      val templateTypes = typeId.templateTypes
      val templateMap = constructTemplateMapping(typeId, template.id.templateTypes, templateTypes)

      val transformTemplate = new Trees.Transformer {
        // uses a strict copier so we recieve an actual copy of the tree
        // TODO: this might not actually be needed if immutability is enforced
        override val treeCopy = new Trees.Copier

        override def _transform(t: Tree): Tree = t match {
          case c@ClassDeclTree(id, parents, fields, methods) =>
            // Update the name of the templated class
            val templateName = template.id.templatedClassName(templateTypes)
            val newId = treeCopy.ClassID(id, templateName, Nil)
            val cons: (List[ClassID], List[VarDecl], List[MethodDeclTree]) => ClassDeclTree = c match {
              case _: ClassDecl => treeCopy.ClassDecl(c, newId, _, _, _)
              case _: TraitDecl => treeCopy.TraitDecl(c, newId, _, _, _)
              case _            => ???
            }
            cons(transform(parents), transform(fields), transform(methods))
          case classId@ClassID(name, tTypes)                 =>
            val newId = treeCopy.ClassID(classId, name, transform(tTypes))
            if (classId.isTemplated)
              TemplateClassGenerator(ctx, templateCU).generateClass(newId)

            templateMap.get(newId) match {
              case Some(replacement) => replacement.copyAttributes(classId)
              case None              => newId
            }
          case _                                             => super._transform(t)
        }
      }
      transformTemplate(template)
    }


    private def constructTemplateMapping(typedId: ClassID, templateList: List[TypeTree], templateTypes: List[TypeTree]): Map[TypeTree, TypeTree] = {
      if (templateTypes.size != templateList.size) {
        report(WrongNumGenerics(templateList.size, templateTypes.size, typedId))
        Map()
      } else {
        templateList.zip(templateTypes).toMap
      }
    }
  }

}

