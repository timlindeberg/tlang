package tcompiler.imports

import tcompiler.analyzer.Symbols.ExtensionClassSymbol
import tcompiler.ast.Trees._
import tcompiler.utils.Extensions._
import tcompiler.utils.{Context, NoPosition}

import scala.collection.mutable

/**
  * Created by Tim Lindeberg on 7/5/2016.
  */

class ImportMap(
  var ctx: Context,
  val imports: List[Import] = Nil,
  val pack: Package = Package(Nil),
  val classes: List[ClassDeclTree] = Nil
) extends ImportErrors {

  override var importMap: ImportMap = this
  private  val shortToFull          = mutable.Map[String, String]()
  private  val fullToShort          = mutable.Map[String, String]()

  var extensionSymbols: List[ExtensionClassSymbol] = Nil

  private val javaObject = List("java", "lang", "Object")
  private val javaString = List("java", "lang", "String")
  private val koolInt    = List("kool", "lang", "Int")
  private val koolLong   = List("kool", "lang", "Long")
  private val koolFloat  = List("kool", "lang", "Float")
  private val koolDouble = List("kool", "lang", "Double")
  private val koolChar   = List("kool", "lang", "Char")
  private val koolBool   = List("kool", "lang", "Bool")
  private val koolLang   = List("kool", "lang")

  private val DefaultImports = List[Import](
    RegularImport(javaObject),
    RegularImport(javaString),
    RegularImport(koolInt),
    RegularImport(koolLong),
    RegularImport(koolFloat),
    RegularImport(koolDouble),
    RegularImport(koolChar),
    RegularImport(koolBool),
    ExtensionImport(koolLang, javaObject),
    ExtensionImport(koolLang, javaString),
    ExtensionImport(koolLang, koolInt),
    ExtensionImport(koolLang, koolLong),
    ExtensionImport(koolLang, koolFloat),
    ExtensionImport(koolLang, koolDouble),
    ExtensionImport(koolLang, koolChar)
  )

  init()

  def init(): Unit = {
    val ignoredImports = ctx.ignoredImports

    val defaultImportNames = DefaultImports.map(_.writtenName)
    ignoredImports
      .filter(!defaultImportNames.contains(_))
      .foreach(ErrorDefaultImportDoesntExist(_, NoPosition))

    val defaultImports = DefaultImports.filter(imp => !ctx.ignoredImports.contains(imp.writtenName))
    defaultImports ++ imports foreach addImport

    val packName = pack.name
    if (packName.nonEmpty) {
      classes.filterInstance[IDClassDeclTree] foreach { clazz =>
        val className = clazz.id.name
        addImport(className, s"$packName::$className")
      }
    }
  }


  private def addImport(imp: Import): Unit = imp match {
    case regImp: RegularImport            =>
      val fullName = regImp.name
      val shortName = regImp.shortName
      val templateImporter = new TemplateImporter(ctx)

      if (contains(shortName))
        ErrorConflictingImport(regImp.writtenName, getFullName(shortName), regImp)
      else if (!(templateImporter.classExists(fullName) || ClassSymbolLocator.classExists(fullName)))
        ErrorCantResolveImport(regImp.writtenName, regImp)
      else
        addImport(shortName, fullName)
    case extensionImport: ExtensionImport =>
      ClassSymbolLocator.findExtensionSymbol(extensionImport.name) match {
        case Some(e) => addExtensionClass(e)
        case None    => ErrorCantResolveExtensionsImport(extensionImport, extensionImport)
      }
    case _: WildCardImport                => // TODO: Support wild card imports.
  }

  def getExtensionClasses(className: String): List[ExtensionClassSymbol] =
    extensionSymbols.filter { extSym =>
      val name = ExtensionDecl.stripExtension(extSym.name)
      name == className
    }

  def addExtensionClass(extensionClassSymbol: ExtensionClassSymbol): Unit = extensionSymbols ::= extensionClassSymbol

  def addImport(tup: (String, String)): Unit = addImport(tup._1, tup._2)
  def addImport(short: String, full: String): Unit = {
    shortToFull += short -> full
    fullToShort += full -> short
  }

  def getFullName(shortName: String): String = shortToFull.getOrElse(shortName, shortName)
  def getShortName(fullName: String): String = fullToShort.getOrElse(fullName, fullName)

  def replaceNames(str: String): String =
    fullToShort.foldLeft(str) { case (s, (full, short)) => s.replaceAll(full, short) }

  def contains(shortName: String): Boolean = shortToFull.contains(shortName)

  def entries: Iterator[(String, String)] = shortToFull.iterator

  override def toString: String = {
    shortToFull.map { case (short, full) => s"$short -> $full" }.mkString("\n")
  }

}