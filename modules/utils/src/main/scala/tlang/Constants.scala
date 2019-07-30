package tlang

import java.io.FileNotFoundException
import java.nio.file.{Path, Paths}

import better.files.{File, _}

import scala.io.Source
import scala.util.Try

object Constants {

  val FileEnding                    = ".t"
  val THome                         = "T_HOME"
  val CompilerCommandName           = "tcompile"
  val ReplCommandName               = "trepl"
  val JavaObject                    = "java::lang::Object"
  val JavaString                    = "java::lang::String"
  val TLangPackage                  = "T::lang"
  val TInt                          = s"$TLangPackage::Int"
  val TIntRef                       = s"$TLangPackage::IntRef"
  val TLong                         = s"$TLangPackage::Long"
  val TLongRef                      = s"$TLangPackage::LongRef"
  val TFloat                        = s"$TLangPackage::Float"
  val TFloatRef                     = s"$TLangPackage::FloatRef"
  val TDouble                       = s"$TLangPackage::Double"
  val TDoubleRef                    = s"$TLangPackage::DoubleRef"
  val TChar                         = s"$TLangPackage::Char"
  val TCharRef                      = s"$TLangPackage::CharRef"
  val TBool                         = s"$TLangPackage::Bool"
  val TBoolRef                      = s"$TLangPackage::BoolRef"
  val TAnnotation                   = s"$TLangPackage::Annotation"
  val ExtensionAnnotation           = s"$TLangPackage::$$ExtensionMethod"
  val ImplicitConstructorAnnotation = s"$TLangPackage::$$ImplicitConstructor"

  val Primitives = List(TInt, TLong, TFloat, TDouble, TBool, TChar)

  lazy val Version          : String = readVersion()
  lazy val THomeDirectory   : String = sys.env.getOrElse(THome, FatalCantFindTHome)
  lazy val TStdLibDirectory : String = tStdLibDirectory
  lazy val SettingsDirectory: File   = System.getProperty("user.home") / ".tlang"
  lazy val Pwd              : Path   = Paths.get("").toAbsolutePath

  private val VersionFile = "version.txt"
  private val StdLibDir   = "stdlib"

  private def FatalCantFindTHome: Nothing = {
    System.err.println(s"$THome environment variable is not set. It needs to point to the directory of the T standard library.")
    sys.exit(1)
  }

  private def readVersion(): String = {
    Try(Source.fromResource(VersionFile).mkString.trim)
      .getOrElse(throw new FileNotFoundException(VersionFile))
  }

  private def tStdLibDirectory: String = {
    val dir = Paths.get(THomeDirectory, StdLibDir).toFile
    if (!dir.exists()) {
      System.err.println(s"$THome folder ($THomeDirectory) does not contain a $StdLibDir directory.")
      sys.exit(1)
    }
    dir.getAbsolutePath
  }

}
