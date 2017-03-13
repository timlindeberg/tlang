package tlang.utils

import java.io.PrintWriter

import scala.annotation.StaticAnnotation
import scala.collection.immutable.Seq
import scala.meta._

case class AST(name: Term.Name, params: Seq[Term.Param]) {
  def args: Seq[Term.Name] = params.map(p => Term.Name(p.name.value))
  def patTerms: Seq[Pat.Var.Term] = args.map(a => Pat.Var.Term(a))
}

class GenerateTreeHelpers extends StaticAnnotation {

  inline def apply(defn: Any): Any = meta {
    import GenerateTreeHelpers._

    defn match {
      case q"object Trees { ..$stats }" =>
        val asts = getASTs(stats)
        val newStats = stats :+
          createCopier(asts) :+
          createLazyCopier(asts) :+
          createTransformer(asts) :+
          createTraverser(asts)
        q"object Trees { ..$newStats }"
      case _                            =>
        abort("@GenerateTreeHelpers must annotate Trees object.")
    }
  }
}

object GenerateTreeHelpers {

  private val Primitives   = List("Int", "Long", "Float", "Double", "Char")
  private val IgnoredTypes = Primitives ::: List("String", "List[String]", "ImportMap")

  def getASTs(stats: Seq[Stat]): Seq[AST] = stats.collect {
    case q"case class $clazz[..$_] ..$_ (...$paramss) extends $_" =>
      val params = paramss.flatten.map(_.copy(mods = Seq[Mod]()))
      val name = Term.Name(clazz.value)
      AST(name, params)
  }

  def createCopier(asts: Seq[AST]): Defn.Class = {
    val copyFunctions = asts map { case ast@AST(name, params) =>
      val ctor = Ctor.Ref.Name(name.value)
      q"def $name(t: Tree, ..$params) = new $ctor(..${ast.args}).copyAttributes(t)"
    }
    q"class Copier { ..$copyFunctions }"
  }

  def createLazyCopier(asts: Seq[AST]): Defn.Class = {
    val copyFunctions = asts map { case ast@AST(name, params) =>
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
    q"class LazyCopier extends Copier { ..$copyFunctions }"
  }

  def createTransformer(asts: Seq[AST]): Defn.Class = {
    val cases = asts map { case ast@AST(name, params) =>
      val transforms = params.map { param =>
        val tpe = param.decltpe.map(_.syntax).getOrElse("Any")
        val name = Term.Name(param.name.value)
        if (IgnoredTypes.contains(tpe)) name else q"_transform($name).asInstanceOf[${Type.Name(tpe)}]"
      }

      p"case $name(..${ast.patTerms}) => treeCopy.$name(t, ..$transforms)"
    }

    q"""
      class Transformer {
         val treeCopy: Copier = new LazyCopier()

         final def apply[T <: Tree](t: T): T = _transform(t).asInstanceOf[T]

         final def transform[T <: Tree](t: T): T                  = _transform(t).asInstanceOf[T]
         final def transform[T <: Tree](list: List[T]): List[T]   = _transform(list).asInstanceOf[List[T]]
         final def transform[T <: Tree](set: Set[T]): Set[T]      = _transform(set).asInstanceOf[Set[T]]
         final def transform[T <: Tree](op: Option[T]): Option[T] = _transform(op).asInstanceOf[Option[T]]

         protected def _transform(t: Tree): Tree = t match {
            ..case $cases
         }

         final protected def _transform[T <: Tree](list: List[T]): List[Tree]   = list map _transform
         final protected def _transform[T <: Tree](set: Set[T]): Set[Tree]      = set map _transform
         final protected def _transform[T <: Tree](op: Option[T]): Option[Tree] = op map _transform
      }
    """
  }

  def createTraverser(asts: Seq[AST]): Defn.Class = {
    val cases = asts
      .map { case ast@AST(_, params) =>
        val traverses = params
          .filter { p =>
            val tpe = p.decltpe.map(_.syntax).getOrElse("")
            !IgnoredTypes.contains(tpe)
          }
          .map { p => q"_traverse(${Term.Name(p.name.value)})" }
        (ast, traverses)
      }
      .filter { case (_, traverses) => traverses.nonEmpty }
      .map { case (ast@AST(name, _), traverses) => p"case $name(..${ast.patTerms}) => { ..$traverses }" }

    q"""
      class Traverser {
         final def traverse(t: Tree): Unit                  = _traverse(t)
         final def traverse(trees: Traversable[Tree]): Unit = _traverse(trees)

         protected def _traverse(t: Tree): Unit = t match {
            case _: Leaf =>
            ..case $cases
         }

         final protected def _traverse(op: Option[Tree]): Unit         = op foreach _traverse
         final protected def _traverse(trees: Traversable[Tree]): Unit = trees foreach _traverse
      }
     """
  }

  // Used to log trees to file during compilation
  def logTree(t: Tree): Unit = {
    val path = "C:\\Users\\Tim Lindeberg\\IdeaProjects\\log.txt"
    new PrintWriter(path) {write(t.syntax); close()}
  }

  private def equality(params: List[Term.Param]): Option[Term] = {

    def _equality(params: List[Term.Param]): Term = {
      params match {
        case param :: Nil  => compare(param)
        case param :: rest => q"${compare(param)} && (${_equality(rest)})"
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