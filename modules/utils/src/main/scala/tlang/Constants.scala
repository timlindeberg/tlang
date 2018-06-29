package tlang

import java.nio.file.{Path, Paths}

import better.files.{File, _}

object Constants {

  val FileEnding                    = ".t"
  val VersionNumber                 = "0.0.1"
  val THome                         = "T_HOME"
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
  val ExtensionAnnotation           = s"$TLangPackage::$$ExtensionMethod"
  val ImplicitConstructorAnnotation = s"$TLangPackage::$$ImplicitConstructor"

  val Primitives = List(TInt, TLong, TFloat, TDouble, TBool, TChar)

  lazy val TDirectory       : String = sys.env.getOrElse(THome, FatalCantFindTHome)
  lazy val SettingsDirectory: File   = System.getProperty("user.home") / ".tlang"
  lazy val Pwd              : Path   = Paths.get("").toAbsolutePath

  private def FatalCantFindTHome: Nothing = {
    System.err.println(s"$THome environment variable is not set. It needs to point to the directory of the T standard library.")
    sys.exit(1)
  }

}
