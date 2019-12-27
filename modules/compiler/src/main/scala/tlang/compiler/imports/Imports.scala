package tlang
package compiler
package imports

import java.util.regex.Matcher

import tlang.compiler.analyzer.Symbols.ExtensionClassSymbol
import tlang.compiler.ast.Trees._
import tlang.compiler.messages.Reporter
import tlang.formatting.ErrorStringContext
import tlang.utils.Logging

import scala.collection.mutable

object Imports {

  val javaLang = List("java", "lang")
  val tLang = List("T", "lang")

  val JavaObject: List[String] = javaLang :+ "Object"
  val JavaString: List[String] = javaLang :+ "String"
  val TInt: List[String] = tLang :+ "Int"
  val TLong: List[String] = tLang :+ "Long"
  val TFloat: List[String] = tLang :+ "Float"
  val TDouble: List[String] = tLang :+ "Double"
  val TChar: List[String] = tLang :+ "Char"
  val TBool: List[String] = tLang :+ "Bool"
  val TObjectExtension: List[String] = tLang :+ "ObjectExtension"
  val TStringExtension: List[String] = tLang :+ "StringExtension"
  val TBoolExtension: List[String] = tLang :+ "BoolExtension"
  val TIntExtension: List[String] = tLang :+ "IntExtension"
  val TLongExtension: List[String] = tLang :+ "LongExtension"
  val TFloatExtension: List[String] = tLang :+ "FloatExtension"
  val TDoubleExtension: List[String] = tLang :+ "DoubleExtension"
  val TCharExtension: List[String] = tLang :+ "CharExtension"

  val DefaultImports: List[Import] = List(
    RegularImport(JavaObject),
    RegularImport(JavaString),
    RegularImport(TInt),
    RegularImport(TLong),
    RegularImport(TFloat),
    RegularImport(TDouble),
    RegularImport(TChar),
    RegularImport(TBool),
    RegularImport(TObjectExtension),
    RegularImport(TStringExtension),
    RegularImport(TBoolExtension),
    RegularImport(TIntExtension),
    RegularImport(TLongExtension),
    RegularImport(TFloatExtension),
    RegularImport(TDoubleExtension),
    RegularImport(TCharExtension)
  )

  val DefaultImportNames: List[String] = DefaultImports.map(_.writtenName)
}

case class Imports(ctx: Context, override val errorStringContext: ErrorStringContext, imports: List[Import] = Nil)
  extends ImportErrors with Logging {

  import Imports._

  override val reporter: Reporter = ctx.reporter

  private var extensionSymbols: List[ExtensionClassSymbol] = Nil

  private val shortToFull = mutable.Map[String, String]()
  private val fullToShort = mutable.Map[String, String]()
  private val addedImports = mutable.ListBuffer[Import]()
  private val classPath = ctx.classPath
  private val classSymbolLocator = ClassSymbolLocator(classPath)
  private val templateImporter = new TemplateImporter(ctx)
  private val defaultImports = DefaultImports.filter { imp => imp.writtenName notIn ctx.ignoredImports }

  // Initialize
  {
    defaultImports foreach add
    imports foreach { this += _ }
  }

  def getExtensionClasses(className: String): List[ExtensionClassSymbol] =
    extensionSymbols.filter { extSym => extSym.getExtendedType.name == className }

  def addExtensionClass(extensionClassSymbol: ExtensionClassSymbol): Unit = extensionSymbols ::= extensionClassSymbol

  def +=(tup: (String, String)): this.type = this += (tup._1, tup._2)
  def +=(short: String, full: String): this.type = {
    shortToFull += short -> full
    fullToShort += full -> short
    this
  }

  def +=(imp: Import): this.type = {
    defaultImports.find { _ == imp } ifDefined { _ =>
      report(AlreadyImportedByDefault(imp))
      return this
    }
    addedImports.find { _ == imp } ifDefined { existing =>
      report(AlreadyImported(imp, existing, imp))
      return this
    }
    add(imp)
    addedImports += imp
    this
  }

  private def add(imp: Import): Unit = {
    debug"Adding import ${ imp.writtenName }"

    imp match {
      case imp: RegularImport  => addImport(imp)
      case imp: WildCardImport => addWildCardImport(imp)
    }
  }

  private def addImport(imp: RegularImport): Unit = {
    val shortName = imp.shortName

    if (contains(shortName)) {
      report(ConflictingImport(imp.writtenName, getFullName(shortName), imp))
      return
    }

    val name = imp.name

    if (classSymbolLocator.classExists(name)) {
      this += (shortName, name)
      return
    }

    classSymbolLocator.findExtensionSymbol(name) ifDefined { e =>
      addExtensionClass(e)
      return
    }

    if (templateImporter.classExists(name)) {
      this += (shortName, name)
      return
    }

    report(CantResolveImport(imp.writtenName, imp))
  }

  private def addWildCardImport(wildCardImport: WildCardImport): Unit = {
    val classes = classPath.getClassesInPackage(wildCardImport.name)
    if (classes.isEmpty)
      report(CantResolveImport(wildCardImport.writtenName, wildCardImport))

    classes foreach { name =>
      val imp = RegularImport(name).setPos(wildCardImport)
      addedImports.find { _ == imp } match {
        case Some(existing) => report(AlreadyImported(imp, existing, wildCardImport))
        case None           => addImport(imp)
      }
    }
  }

  def ++=(imps: Imports): this.type = { imps.imports foreach { this += _ }; this }

  def getFullName(shortName: String): String = shortToFull.getOrElse(shortName, shortName)
  def getShortName(fullName: String): String = fullToShort.getOrElse(fullName, fullName)

  override def replaceNames(str: String): String =
    fullToShort.foldLeft(str) { case (s, (full, short)) =>
      s.replaceAll(s"\\Q$full\\E", Matcher.quoteReplacement(short))
    }

  def contains(shortName: String): Boolean = shortToFull.contains(shortName)

  def entries: Iterator[(String, String)] = shortToFull.iterator

  override def toString: String = {
    shortToFull.map { case (short, full) => s"$short -> $full" }.mkString(NL)
  }
}
