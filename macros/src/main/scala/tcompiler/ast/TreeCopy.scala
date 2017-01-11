package tcompiler.ast

import scala.annotation.StaticAnnotation
import scala.meta._

object GenHelpers {

}

class GenTreeCopier extends StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    defn match {
      case q"object $name { ..$stats }" =>
        val (copyFunctions, lazyCopyFunctions) = stats.collect {
          case q"case class $clazz[..$tparams] ..$ctorMods (...$paramss) extends $template" =>

            // remove mods such as var
            val p = paramss.flatten.map(_.copy(mods = scala.collection.immutable.Seq[Mod]()))
            val a = p.map(param => Term.Name(param.name.value))
            val methName = Term.Name(clazz.value)

            val ctor = Ctor.Ref.Name(clazz.value)
            val cpFunc = q"def $methName(t: Tree, ..$p) = new $ctor(..$a).copyAttributes(t)"
            val a0 = a.map(name => Term.Name(name.value + "0"))

            // hack
            val expr = a.length match {
              case 0 => q"true"
              case 1 => q"(${a(0)} eq ${a0(0)})"
              case 2 => q"(${a(0)} eq ${a0(0)}) && (${a(1)} eq ${a0(1)})"
              case 3 => q"(${a(0)} eq ${a0(0)}) && (${a(1)} eq ${a0(1)}) && (${a(2)} eq ${a0(2)})"
              case 4 => q"(${a(0)} eq ${a0(0)}) && (${a(1)} eq ${a0(1)}) && (${a(2)} eq ${a0(2)}) && (${a(3)} eq ${a0(3)})"
              case 5 => q"(${a(0)} eq ${a0(0)}) && (${a(1)} eq ${a0(1)}) && (${a(2)} eq ${a0(2)}) && (${a(3)} eq ${a0(3)}) && (${a(4)} eq ${a0(4)})"
              case 6 => q"(${a(0)} eq ${a0(0)}) && (${a(1)} eq ${a0(1)}) && (${a(2)} eq ${a0(2)}) && (${a(3)} eq ${a0(3)}) && (${a(4)} eq ${a0(4)}) && (${a(5)} eq ${a0(5)})"
              case 7 => q"(${a(0)} eq ${a0(0)}) && (${a(1)} eq ${a0(1)}) && (${a(2)} eq ${a0(2)}) && (${a(3)} eq ${a0(3)}) && (${a(4)} eq ${a0(4)}) && (${a(5)} eq ${a0(5)}) && (${a(6)} eq ${a0(6)})"
              case 8 => q"(${a(0)} eq ${a0(0)}) && (${a(1)} eq ${a0(1)}) && (${a(2)} eq ${a0(2)}) && (${a(3)} eq ${a0(3)}) && (${a(4)} eq ${a0(4)}) && (${a(5)} eq ${a0(5)}) && (${a(6)} eq ${a0(6)}) && (${a(7)} eq ${a0(7)})"
              case 9 => q"(${a(0)} eq ${a0(0)}) && (${a(1)} eq ${a0(1)}) && (${a(2)} eq ${a0(2)}) && (${a(3)} eq ${a0(3)}) && (${a(4)} eq ${a0(4)}) && (${a(5)} eq ${a0(5)}) && (${a(6)} eq ${a0(6)}) && (${a(7)} eq ${a0(7)}) && (${a(8)} eq ${a0(8)})"
              case _ => abort("Cannot generate lazy copier with more than 9 arguments")
            }

            val patArgs = a0.map(arg => parg"$arg")
            val pat = p"$methName(..$patArgs)"
            val lazyCopy =
              q"""
                 override def $methName(tree: Tree, ..$p) = tree match {
                   case t @ $pat if $expr => t
                   case _ => super.$methName(tree, ..$a)
                 }
                """
            (cpFunc, lazyCopy)
        }.unzip

        val treeCopier = q"class TreeCopier { ..$copyFunctions }"
        val lazyTreeCopier = q"class LazyTreeCopier extends TreeCopier { ..$lazyCopyFunctions }"
        q"object $name { ..${ treeCopier +: lazyTreeCopier +: stats}  }"
      case _ =>
        abort("@GenTreeCopier must annotate object.")
    }
  }
}