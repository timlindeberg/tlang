package tcompiler.imports

import tcompiler.Main
import tcompiler.ast.Trees._
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

  var imports: List[Import] = null
  lazy val extensionImports: List[ExtensionImport] = imports.filterType[ExtensionImport]

  private val DefaultImports = List[String](
    Main.TLangObject,
    Main.TLangString
  )

  def this() = this(null)
  def this(imports: List[Import], pack: Package, classes: List[ClassDeclTree], ctx: Context) {
    this(ctx)
    this.imports = imports

    // TODO: Support wild card imports. Need to be able to search the full classpath
    // val wcImports = imports.filterType(classOf[WildCardImport])

    for (imp <- DefaultImports) {
      val s = imp.split("/")
      addImport(s.last, imp)
    }

    val regImports = imports.filterType[RegularImport]
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

    if(pack.name != ""){
      classes.filterNotType[ExtensionDecl] foreach { c =>
        val className = c.id.name
        addImport(className, s"${pack.name}.$className")
      }
    }



  }

  def addImport(tup: (String, String)): Unit  = addImport(tup._1, tup._2)
  def addImport(short: String, full: String): Unit = {
    val f = full.replaceAll("::", ".").replaceAll("/", ".")
    shortToFull += short -> f
    fullToShort += f -> short
  }

  def importNames = imports map importName

  def importName(imp: Import): String = getFullName(imp.name)

  def importName(typeId: ClassID): String = {
    val name = typeId.name.replaceAll("::", ".")
    getFullName(name)
  }

  def importEntries = shortToFull.values.toList

  def getFullName(shortName: String) = shortToFull.getOrElse(shortName, shortName).replaceAll("::", ".")
  def getShortName(fullName: String) = fullToShort.getOrElse(fullName.replaceAll("::", "."), fullName)

  def getErrorName(name: String) = {
    var s = name
    for(e <- fullToShort)
      s = s.replaceAll(e._1, e._2)
    s.replaceAll("/", "::")
  }

  def contains(shortName: String) = shortToFull.contains(shortName)

  def entries = shortToFull.iterator

  override def toString = {
    shortToFull.map { case (short, full) => s"$short -> $full"}.mkString("\n")
  }

}