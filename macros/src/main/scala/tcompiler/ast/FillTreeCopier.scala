package tcompiler.ast

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.language.experimental.macros
import scala.reflect.macros.whitebox


/**
  * Created by Tim Lindeberg on 1/6/2017.
  */
object GenTreeCopier {

  def fillTreeCopier(c: whitebox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    val res = annottees.head match {
      // Match a class, and expand.
      case objectDef @ q"object Trees { ..$stats }" =>

        val copyMethods = for(q"case class $treeName(..$params) extends $parent with ..$traits" <- stats){
          val args = params map { case q"$id: $tpe" => q"id" }
          q"def ${treeName.asInstanceOf[TermName]}(t: Tree, $params) = new $treeName(..$args).copyAttrs(t)"
        }

        q""" object Trees {
           ..$stats

           class TreeCopier {
            ..$copyMethods
            }

           }
       """
      case _ => c.abort(c.enclosingPosition, "Invalid annotation target: Not object Trees")
    }

    c.Expr[Any](res)
  }


  def fillLazyTreeCopier(c: whitebox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    //
    //    val result = annottees map (_.tree) match {
    //      // Match a class, and expand.
    //      case (classDef @ q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }") :: _ =>
    //
    //        val aliasedDefs = for {
    //          q"@alias(..$aliases) def $tname[..$tparams](...$paramss): $tpt = $expr" <- stats
    //          Literal(Constant(alias)) <- aliases
    //          ident = TermName(alias.toString)
    //        } yield {
    //          val args = paramss map { paramList =>
    //            paramList.map { case q"$_ val $param: $_ = $_" => q"$param" }
    //          }
    //
    //          q"def $ident[..$tparams](...$paramss): $tpt = $tname(...$args)"
    //        }
    //
    //        if(aliasedDefs.nonEmpty) {
    //          q"""
    //            $mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self =>
    //              ..$stats
    //              ..$aliasedDefs
    //            }
    //          """
    //        } else classDef
    //      // Not a class.
    //      case _ => c.abort(c.enclosingPosition, "Invalid annotation target: not a class")
    //    }
    //
    c.Expr[Any](q"")
  }
}

@compileTimeOnly("enable macro paradise to expand macro annotations")
class genTreeCopier extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro GenTreeCopier.fillLazyTreeCopier
}

@compileTimeOnly("enable macro paradise to expand macro annotations")
class genLazyCopier extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro GenTreeCopier.fillLazyTreeCopier
}