package tlang

import tlang.compiler.MainErrors

object Constants extends MainErrors {

  val FileEnding                    = ".t"
  val VersionNumber                 = "0.0.1"
  val THome                         = "T_HOME"
  val JavaObject                    = "java::lang::Object"
  val JavaString                    = "java::lang::String"
  val TInt                          = "T::lang::Int"
  val TIntRef                       = "T::lang::IntRef"
  val TLong                         = "T::lang::Long"
  val TLongRef                      = "T::lang::LongRef"
  val TFloat                        = "T::lang::Float"
  val TFloatRef                     = "T::lang::FloatRef"
  val TDouble                       = "T::lang::Double"
  val TDoubleRef                    = "T::lang::DoubleRef"
  val TChar                         = "T::lang::Char"
  val TCharRef                      = "T::lang::CharRef"
  val TBool                         = "T::lang::Bool"
  val TBoolRef                      = "T::lang::BoolRef"
  val ExtensionAnnotation           = "T::lang::$ExtensionMethod"
  val ImplicitConstructorAnnotation = "T::lang::$ImplicitConstructor"

  val Primitives = List(TInt, TLong, TFloat, TDouble, TBool, TChar)

  lazy val TDirectory: String = {
    if (!sys.env.contains(THome))
      FatalCantFindTHome(THome)
    sys.env(THome)
  }

  protected def FatalCantFindTHome(tHome: String): Nothing = {
    println(s"$tHome environment variable is not set. It needs to point to the directory of the T standard library.")
    sys.exit(1)
  }

}
