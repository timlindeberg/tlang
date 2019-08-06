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

  private val javaLang = List("java", "lang")
  private val tLang    = List("T", "lang")

  private val javaObject       = javaLang :+ "Object"
  private val javaString       = javaLang :+ "String"
  private val TInt             = tLang :+ "Int"
  private val TLong            = tLang :+ "Long"
  private val TFloat           = tLang :+ "Float"
  private val TDouble          = tLang :+ "Double"
  private val TChar            = tLang :+ "Char"
  private val TBool            = tLang :+ "Bool"
  private val TObjectExtension = tLang :+ "ObjectExtension"
  private val TStringExtension = tLang :+ "StringExtension"
  private val TIntExtension    = tLang :+ "IntExtension"
  private val TLongExtension   = tLang :+ "LongExtension"
  private val TFloatExtension  = tLang :+ "FloatExtension"
  private val TDoubleExtension = tLang :+ "DoubleExtension"
  private val TCharExtension   = tLang :+ "CharExtension"

  val DefaultImports: List[Import] = List(
    RegularImport(javaObject),
    RegularImport(javaString),
    RegularImport(TInt),
    RegularImport(TLong),
    RegularImport(TFloat),
    RegularImport(TDouble),
    RegularImport(TChar),
    RegularImport(TBool),
    RegularImport(TObjectExtension),
    RegularImport(TStringExtension),
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

  var extensionSymbols: List[ExtensionClassSymbol] = Nil

  private val shortToFull        = mutable.Map[String, String]()
  private val fullToShort        = mutable.Map[String, String]()
  private val classPath          = ctx.classPath
  private val classSymbolLocator = ClassSymbolLocator(classPath)
  private val templateImporter   = new TemplateImporter(ctx)


  // Initialize
  {
    val defaultImports = DefaultImports.filter { imp => imp.writtenName notIn ctx.ignoredImports }
    (defaultImports ++ imports) foreach { this += _ }
  }

  def getExtensionClasses(className: String): List[ExtensionClassSymbol] =
    extensionSymbols.filter { extSym =>
      val name = ExtensionDecl.stripPrefix(extSym.name)
      name == className
    }

  def addExtensionClass(extensionClassSymbol: ExtensionClassSymbol): Unit = extensionSymbols ::= extensionClassSymbol

  def +=(tup: (String, String)): this.type = this += (tup._1, tup._2)
  def +=(short: String, full: String): this.type = {
    shortToFull += short -> full
    fullToShort += full -> short
    this
  }

  def +=(imp: Import): this.type = {
    debug"Adding import ${ imp.writtenName }"
    imp match {
      case imp: RegularImport  => addImport(imp)
      case imp: WildCardImport => addWildCardImport(imp)
    }
    this
  }

  private def addImport(imp: RegularImport): Unit = {
    val shortName = imp.shortName

    if (contains(shortName)) {
      report(ConflictingImport(imp.writtenName, getFullName(shortName), imp))
      return
    }

    if (classExists(imp.name)) {
      this += (shortName, imp.name)
      return
    }

    val extensionClassName = ExtensionDecl.prefix + imp.name
    if (classExists(extensionClassName)) {
      classSymbolLocator.findExtensionSymbol(extensionClassName) match {
        case Some(e) => addExtensionClass(e)
        case None    => report(CantResolveExtensionsImport(imp.writtenName, imp))
      }
      return
    }
    report(CantResolveImport(imp.writtenName, imp))
  }

  private def addWildCardImport(imp: WildCardImport): Unit = {
    classPath.getClassesInPackage(imp.name) foreach { name => this += RegularImport(name) }
  }

  private def classExists(className: String): Boolean = {
    templateImporter.classExists(className) || classSymbolLocator.classExists(className)
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
