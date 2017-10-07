package tlang.utils

import java.io.PrintWriter

import scala.annotation.StaticAnnotation
import scala.collection.immutable.Seq
import scala.meta._

case class AST(name: Term.Name, params: Seq[Term.Param]) {

  val args      : Seq[Term.Name]    = params.map(p => Term.Name(p.name.value))
  val patTerms  : Seq[Pat.Var.Term] = args.map(a => Pat.Var.Term(a))
  val commonness: Int               = -FillTreeHelpers.TreeStatistics.getOrElse(name.syntax, 0)

}


class FillTreeHelpers extends StaticAnnotation {

  inline def apply(defn: Any): Any = meta {
    import FillTreeHelpers._

    defn match {
      case q"object Trees { ..$stats }" =>
        val asts = getASTs(stats)

        val filledTrees = stats.map {
          case q"class Copier"                              =>
            q"class Copier { ..${ fillCopier(asts) } }"
          case q"class LazyCopier extends $_"               =>
            q"class LazyCopier extends Copier { ..${ fillLazyCopier(asts) } }"
          case q"trait Transformer { ..$transformerStats }" =>
            q"trait Transformer { ..${ fillTransformer(transformerStats, asts) } }"
          case q"trait Traverser { ..$traverserStats }"     =>
            q"trait Traverser { ..${ fillTraverser(traverserStats, asts) } }"
          case s                                            => s
        }

        //val file = Paths.get("C:\\Users\\Tim Lindeberg\\IdeaProjects\\T-Compiler\\tree.txt")
        //Files.write(file, filledTrees.map(_.syntax).toList.asJava, Charset.forName("UTF-8"))

        q"object Trees { ..$filledTrees }"
      case _                            =>
        abort("@GenerateTreeHelpers must annotate Trees object.")
    }
  }
}

object FillTreeHelpers {

  // Statistics taken from the test suite 2017-09-31
  // Calculated by writing the name of the class to a file
  // every time that class is selected in the traverser or transformer.
  // @formatter:off

  val TreeStatistics: Map[String, Int] = Map(
    "VariableID"        -> 20933,
    "MethodCall"        -> 11923,
    "NormalAccess"      -> 11542,
    "MethodID"          -> 8910,
    "ClassID"           -> 8230,
    "IntLit"            -> 4858,
    "VarDecl"           -> 3534,
    "Println"           -> 3153,
    "Block"             -> 3052,
    "Assign"            -> 2848,
    "Private"           -> 2677,
    "Public"            -> 2538,
    "MethodDecl"        -> 2529,
    "Formal"            -> 2452,
    "ArrayRead"         -> 2096,
    "Return"            -> 2051,
    "Plus"              -> 1969,
    "StringLit"         -> 1629,
    "New"               -> 1498,
    "Empty"             -> 1158,
    "Final"             -> 1073,
    "Static"            -> 950,
    "If"                -> 932,
    "ArrayType"         -> 830,
    "Equals"            -> 784,
    "ClassDecl"         -> 733,
    "LessThan"          -> 652,
    "UnitType"          -> 590,
    "NotEquals"         -> 528,
    "CompilationUnit"   -> 525,
    "This"              -> 517,
    "Not"               -> 495,
    "ArrayLit"          -> 425,
    "NullableType"      -> 409,
    "Minus"             -> 407,
    "And"               -> 400,
    "Package"           -> 378,
    "Times"             -> 357,
    "For"               -> 335,
    "NullLit"           -> 329,
    "PutValue"          -> 311,
    "OperatorDecl"      -> 293,
    "GeneratedExpr"     -> 285,
    "Div"               -> 283,
    "PostIncrement"     -> 273,
    "NewArray"          -> 260,
    "Error"             -> 254,
    "ConstructorDecl"   -> 236,
    "While"             -> 223,
    "Foreach"           -> 222,
    "Ternary"           -> 194,
    "Hash"              -> 187,
    "GreaterThan"       -> 184,
    "GreaterThanEquals" -> 184,
    "TrueLit"           -> 180,
    "Print"             -> 173,
    "FalseLit"          -> 172,
    "CharLit"           -> 160,
    "As"                -> 159,
    "LogicXor"          -> 157,
    "ArraySlice"        -> 149,
    "TraitDecl"         -> 142,
    "LessThanEquals"    -> 124,
    "Is"                -> 123,
    "RightShift"        -> 117,
    "LongLit"           -> 107,
    "Or"                -> 106,
    "ExtractNullable"   -> 102,
    "Modulo"            -> 97,
    "Elvis"             -> 83,
    "LeftShift"         -> 80,
    "SafeAccess"        -> 70,
    "FloatLit"          -> 70,
    "Super"             -> 69,
    "LogicAnd"          -> 69,
    "LogicOr"           -> 59,
    "DoubleLit"         -> 54,
    "PostDecrement"     -> 50,
    "Negation"          -> 49,
    "PreIncrement"      -> 39,
    "Continue"          -> 37,
    "Implicit"          -> 37,
    "PreDecrement"      -> 30,
    "Break"             -> 27,
    "Protected"         -> 20,
    "LogicNot"          -> 17,
    "ExtensionDecl"     -> 9
  )
  // @formatter:on

  private val Primitives   = List("Int", "Long", "Float", "Double", "Char")
  private val IgnoredTypes = Primitives ::: List("String", "List[String]", "Imports")

  def getASTs(stats: Seq[Stat]): Seq[AST] = stats.collect {
    case q"case class $clazz[..$_] ..$_ (...$paramss) extends $_" =>
      val params = paramss.flatten.map(_.copy(mods = Seq[Mod]()))
      val name = Term.Name(clazz.value)
      AST(name, params)
  }

  def fillCopier(asts: Seq[AST]): Seq[Defn.Def] =
    asts map { case ast@AST(name, params) =>
      val ctor = Ctor.Ref.Name(name.value)
      q"def $name(t: Tree, ..$params) = new $ctor(..${ ast.args }).copyAttributes(t)"
    }

  def fillLazyCopier(asts: Seq[AST]): Seq[Defn.Def] =
    asts map { case ast@AST(name, params) =>
      val args = ast.args
      val patTerms = args.map(a => Pat.Var.Term(a.copy(a.value + "0")))
      val equals = equality(params.toList)

      val pat = p"t @ $name(..$patTerms)"
      val eqCase = equals match {
        case Some(eq) => p"case $pat if $eq => t"
        case None     => p"case $pat => t"
      }

      q"""
       override def $name(tree: Tree, ..$params) = tree match {
         case $eqCase
         case _ => super.$name(tree, ..$args)
       }
      """
    }

  def fillTransformer(transformerStats: Seq[Stat], asts: Seq[AST]): Seq[Stat] = transformerStats map {
    case q"final def transformChildren(t: Tree): Tree = ???" =>
      val cases = asts
        .sortBy(_.commonness)
        .map { case ast@AST(name, params) =>
          val transforms = params.map { param =>
            val tpe = param.decltpe.map(_.syntax).getOrElse("Any")
            val name = Term.Name(param.name.value)
            if (IgnoredTypes.contains(tpe)) name else q"transform($name)"
          }
          p"case $name(..${ ast.patTerms }) => copier.$name(t, ..$transforms)"
        }

      q"""
         final def transformChildren(t: Tree): Tree = t match {
            ..case $cases
         }
       """
    case s                                                   => s
  }

  def fillTraverser(traverserStats: Seq[Stat], asts: Seq[AST]): Seq[Stat] = traverserStats map {
    case q"final def traverseChildren(t: Tree): Unit = ???" =>
      val cases = asts
        .sortBy(_.commonness)
        .map { case ast@AST(_, params) =>
          val traverses = params
            .filter { param => !IgnoredTypes.contains(param.decltpe.map(_.syntax).getOrElse("")) }
            .map { param => q"traverse(${ Term.Name(param.name.value) })" }
          (ast, traverses)
        }
        .filter { case (_, traverses) => traverses.nonEmpty }
        .map { case (ast@AST(name, _), traverses) => p"case $name(..${ ast.patTerms }) => { ..$traverses }" }

      q"""
          final def traverseChildren(t: Tree): Unit = t match {
             case _: Leaf =>
           ..case $cases
          }
       """
    case s                                                  => s
  }

  // Used to log trees to file during compilation
  def logTree(t: Tree): Unit = {
    val path = "C:\\Users\\Tim Lindeberg\\IdeaProjects\\log.txt"
    new PrintWriter(path) {write(t.syntax); close() }
  }

  private def equality(params: List[Term.Param]): Option[Term] = {

    def _equality(params: List[Term.Param]): Term = {
      params match {
        case param :: Nil  => compare(param)
        case param :: rest => q"${ compare(param) } && (${ _equality(rest) })"
        case _             => ???
      }
    }

    def compare(param: Term.Param) = {
      val name = param.name.value
      val a = Term.Name(name)
      val a0 = Term.Name(name + "0")
      val tpe = param.decltpe.map(_.syntax).getOrElse("")
      if (Primitives.contains(tpe)) q"($a == $a0)" else q"($a eq $a0)"
    }

    if (params.isEmpty) None else Some(_equality(params))
  }
}