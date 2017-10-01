package tlang.utils

import java.io.PrintWriter
import java.nio.charset.Charset
import java.nio.file.{Files, Paths}

import scala.annotation.StaticAnnotation
import scala.collection.JavaConverters._
import scala.collection.immutable.Seq
import scala.meta._

case class AST(name: Term.Name, params: Seq[Term.Param]) {
  def args: Seq[Term.Name] = params.map(p => Term.Name(p.name.value))
  def patTerms: Seq[Pat.Var.Term] = args.map(a => Pat.Var.Term(a))
}

class FillTreeHelpers extends StaticAnnotation {

  inline def apply(defn: Any): Any = meta {
    import FillTreeHelpers._

    defn match {
      case q"object Trees { ..$stats }" =>
        val asts = getASTs(stats)
        val file = Paths.get("C:\\Users\\Tim Lindeberg\\IdeaProjects\\T-Compiler\\tree.txt")

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

        Files.write(file, filledTrees.map(_.syntax).toList.asJava, Charset.forName("UTF-8"))

        q"object Trees { ..$filledTrees }"
      case _                            =>
        abort("@GenerateTreeHelpers must annotate Trees object.")
    }
  }
}

object FillTreeHelpers {

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
      val cases = asts map { case ast@AST(name, params) =>
        val transforms = params.map { param =>
          val tpe = param.decltpe.map(_.syntax).getOrElse("Any")
          val name = Term.Name(param.name.value)
          if (IgnoredTypes.contains(tpe)) name else q"_transform($name).asInstanceOf[${ Type.Name(tpe) }]"
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
    case q"protected def _traverse(t: Tree): Unit = ???" =>
      val cases = asts
        .map { case ast@AST(_, params) =>
          val traverses = params
            .filter { p =>
              val tpe = p.decltpe.map(_.syntax).getOrElse("")
              !IgnoredTypes.contains(tpe)
            }
            .map { p => q"_traverse(${ Term.Name(p.name.value) })" }
          (ast, traverses)
        }
        .filter { case (_, traverses) => traverses.nonEmpty }
        .map { case (ast@AST(name, _), traverses) => p"case $name(..${ ast.patTerms }) => { ..$traverses }" }
      q"""
          protected def _traverse(t: Tree): Unit = t match {
             case _: Leaf =>
           ..case $cases
          }
       """
    case s                                               => s
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