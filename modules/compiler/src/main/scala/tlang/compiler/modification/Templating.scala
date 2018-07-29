package tlang
package compiler
package modification

import tlang.compiler.ast.Trees
import tlang.compiler.ast.Trees._
import tlang.compiler.imports.{ClassSymbolLocator, Imports, TemplateImporter}
import tlang.compiler.messages.Reporter
import tlang.compiler.output.Output
import tlang.compiler.output.debug.ASTOutput
import tlang.formatting.{ErrorStringContext, Formatter}
import tlang.utils.Logging

import scala.collection.mutable

object Templating extends CompilerPhase[CompilationUnit, CompilationUnit] with Logging {

  val StartEnd  = "-"
  val Seperator = "$"

  def run(ctx: Context)(cus: List[CompilationUnit]): List[CompilationUnit] = {
    info"Generating templates"
    val templateClassGenerator = TemplateModifier(ctx)
    templateClassGenerator.generateTemplates(cus)
  }

  override def description(implicit formatter: Formatter): String =
    "Imports template classes and instantiates templates from generic classes."

  override def debugOutput(output: List[CompilationUnit])(implicit formatter: Formatter): Output = ASTOutput(phaseName, output)


}

case class TemplateModifier(ctx: Context) extends Logging {

  import ctx.formatter

  private val templateCus         = mutable.Map[String, CompilationUnit]()
  private var generatedClassNames = mutable.Set[String]()

  def generateTemplates(cus: List[CompilationUnit]): List[CompilationUnit] = {

    templateCus ++= cus.flatMap { cu =>
      val classes = cu.classes.filterInstance[IDClassDeclTree]
      classes.filter(_.id.isTemplated).map { clazz => (clazz.id.name, cu) }
    }

    val classSymbolLocator = ClassSymbolLocator(ctx.classPath)
    // Generate all needed classes
    // This can't be ran in parallell since there are dependencies between CUs
    cus foreach { cu =>
      val transforms = List[String => String](cu.imports.replaceNames)
      val errorStringContext = ErrorStringContext(transforms = transforms)
      val templateClassGenerator = TemplateClassGenerator(ctx.reporter, errorStringContext, cu, classSymbolLocator)
      templateClassGenerator()
    }

    // all needed classes are generated,
    // construct final program list and filter old classes
    val allCus = mutable.Set[CompilationUnit]()

    allCus ++= cus
    allCus ++= templateCus.values

    // Remove all template classes and replace types in rest of the classes
    ctx.executor.map(allCus.toList) { cu =>
      cu.classes = cu.classes.filterInstance[IDClassDeclTree].filter(!_.id.isTemplated) ++ cu.classes.filterInstance[ExtensionDecl]
      replaceTypes(cu)
    }
  }

  private def replaceTypes(cu: CompilationUnit): CompilationUnit = {
    debug"Replacing template types in ${ cu.sourceDescription }"
    // Replace types with their templated class names, eg.
    // replace Map<Int, String> with -Map$Int$String-.
    val replace = new Trees.Transformer {
      def transformation: TreeTransformation = {
        case tpe: ClassID if tpe.isTemplated =>
          val shortName = tpe.name.split("::").last
          if (cu.imports.contains(shortName)) {
            val entry = getImportEntry(cu.imports, tpe)
            cu.imports += entry
          }
          copier.ClassID(tpe, tpe.templatedClassName)
      }
    }

    replace(cu)
  }


  private def getImportEntry(imports: Imports, classId: ClassID) = {
    val templateName = classId.templatedClassName
    val fullName = imports.getFullName(classId.name)
    val prefix = fullName.split("::").dropRight(1).mkString("::")
    templateName -> s"$prefix::$templateName"
  }

  case class TemplateClassGenerator(
    override val reporter: Reporter,
    override val errorStringContext: ErrorStringContext,
    cu: CompilationUnit,
    classSymbolLocator: ClassSymbolLocator) extends TemplatingErrors with Logging {

    /**
      * Has the side effect of filling the programs in the template program map with
      * generated template classes.
      */
    def apply(): Unit = {
      cu.classes.filterInstance[IDClassDeclTree].filter(_.id.isTemplated) foreach checkDuplicateTemplateNames

      val traverser = new Trees.Traverser {
        def traversal: TreeTraversal = {
          case ClassDeclTree(_, parents, fields, methods) =>
            // Ignore the id of classdecls since these can declare templated types
            // which should not be generated
            traverse(parents)
            traverse(fields)
            traverse(methods)
          case c: ClassID if c.isTemplated                => generateClass(c)
        }
      }
      traverser.traverse(cu)
    }

    private def generateClass(typeId: ClassID): Unit = {
      val shortName = typeId.templatedClassName.split("::").last

      if (generatedClassNames(shortName))
        return

      info"Generating template for $shortName"

      generatedClassNames += shortName

      // Update import map to include the newly generated class
      if (cu.imports.contains(typeId.name)) {
        val entry = getImportEntry(cu.imports, typeId)
        cu.imports.+=(entry)
      }

      findTemplateCU(typeId) match {
        case Some(templateCU) =>
          typeId.templateTypes.foreach {
            case classId: ClassID =>
              // Update the import map with the instantiated types:
              // e.g. Iterator<Vector<Int>> updates the
              // Iterator's import map with Vector<Int> -> T.std.Vector<Int>
              updateImports(templateCU.imports, classId)
            case _                =>
          }
          val generatedClass = newTemplateClass(templateCU, typeId)
          templateCU.classes ::= generatedClass
        case None             => report(ClassDoesNotExist(typeId.name, typeId))
      }
    }

    private def checkDuplicateTemplateNames(templateClass: IDClassDeclTree): Unit = {
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

    private def updateImports(imports: Imports, classId: ClassID) = {
      // Add the classId to the import map
      val templateName = classId.templatedClassName
      if (cu.imports.contains(templateName)) {
        val fullName = cu.imports.getFullName(templateName)
        imports.+=(templateName, fullName)
      } else if (cu.imports.contains(classId.name)) {
        val entry = getImportEntry(cu.imports, classId)
        imports.+=(entry)
      }
    }


    private def findTemplateCU(typeId: ClassID): Option[CompilationUnit] = {
      val className = typeId.name.split("::").last
      if (templateCus.contains(className))
        return Some(templateCus(className))

      val templateImporter = new TemplateImporter(ctx)
      val importName = cu.imports.getFullName(typeId.name)
      val importedCus = templateImporter.importCUs(importName)

      importedCus foreach { cu =>
        cu.classes
          .filterInstance[IDClassDeclTree]
          .filter { clazz => !templateCus.contains(clazz.id.name) }
          .foreach { clazz => templateCus(clazz.id.name) = cu }
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

        // Uses a strict copier so we receive a deep copy of the tree
        override val copier = new Trees.Copier

        def transformation: TreeTransformation = {
          case c@ClassDeclTree(id, parents, fields, methods) =>
            // Update the name of the templated class
            val templateName = template.id.templatedClassName(templateTypes)
            val newId = copier.ClassID(id, templateName, Nil)
            val cons: (List[ClassID], List[VarDecl], List[MethodDeclTree]) => ClassDeclTree = c match {
              case _: ClassDecl => copier.ClassDecl(c, newId, _, _, _)
              case _: TraitDecl => copier.TraitDecl(c, newId, _, _, _)
              case _            => ???
            }
            cons(transform(parents), transform(fields), transform(methods))
          case classId@ClassID(name, tTypes)                 =>
            val newId = copier.ClassID(classId, name, transform(tTypes))
            if (classId.isTemplated) {
              val transforms = List[String => String](templateCU.imports.replaceNames)
              val e = ErrorStringContext(transforms = transforms)(errorStringContext.formatter)
              TemplateClassGenerator(reporter, e, templateCU, classSymbolLocator).generateClass(newId)
            }

            templateMap.get(newId) match {
              case Some(replacement) => replacement.copyAttributes(classId)
              case None              => newId
            }
        }
      }
      transformTemplate(template)
    }


    private def constructTemplateMapping(typedId: ClassID, templateList: List[TypeTree], templateTypes: List[TypeTree]): Map[TypeTree, TypeTree] = {
      if (templateTypes.lengthCompare(templateList.size) != 0) {
        report(WrongNumGenerics(templateList.size, templateTypes.size, typedId))
        Map()
      } else {
        templateList.zip(templateTypes).toMap
      }
    }
  }

}

