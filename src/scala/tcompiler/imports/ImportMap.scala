package tcompiler.imports

import tcompiler.Main
import tcompiler.ast.Trees.{ClassID, Import, RegularImport}
import tcompiler.utils.Context
import tcompiler.utils.Extensions._

import scala.collection.mutable

/**
  * Created by Tim Lindeberg on 7/5/2016.
  */

class ImportMap(override var ctx: Context) extends ImportErrors {

  override var importMap = this
  private val shortToFull = mutable.Map[String, String]()
  private val fullToShort = mutable.Map[String, String]()

  private val DefaultImports = List[String](
    Main.TLangObject,
    Main.TLangString
  )

  def this(imports: List[Import], ctx: Context) {
    this(ctx)
    val regImports = imports.filterType(classOf[RegularImport])

    // TODO: Support wild card imports. Need to be able to search the full classpath
    //val wcImports = imports.filterType(classOf[WildCardImport])

    for (imp <- DefaultImports) {
      val s = imp.split("/")
      addImport(s.last, imp)
    }

    for (imp <- regImports) {
      val fullName = imp.name
      val shortName = imp.shortName
      val templateImporter = new TemplateImporter(ctx)

      if (contains(shortName))
        ErrorConflictingImport(imp.writtenName, getFullName(shortName), imp)
      else if (!(templateImporter.classExists(fullName) || ClassSymbolLocator.classExists(fullName)))
        ErrorCantResolveImport(imp.writtenName, imp)
      else
        addImport(shortName, fullName)
    }
  }


  def addImport(short: String, full: String) = {
    val f = full.replaceAll("::", ".")
    shortToFull += short -> f
    fullToShort += f -> short
  }

  def importName(imp: Import): String = getFullName(imp.name)

  def importName(typeId: ClassID): String = {
    val name = typeId.name.replaceAll("::", ".")
    getFullName(name)
  }

  def getFullName(shortName: String) = shortToFull.getOrElse(shortName, shortName).replaceAll("::", ".")
  def getShortName(fullName: String) = fullToShort.getOrElse(fullName.replaceAll("::", "."), fullName)

  def getErrorName(name: String) = {
    var s = name
    for(e <- fullToShort)
      s = s.replaceAll(e._1, e._2)
    s.replaceAll("/", "::")
  }

  def contains(shortName: String) = shortToFull.contains(shortName)

  def entries = fullToShort.iterator

}