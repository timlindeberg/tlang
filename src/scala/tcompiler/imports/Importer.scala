package tcompiler.imports

import tcompiler.ast.Trees._
import tcompiler.utils.Context

/**
  * Created by Tim Lindeberg on 5/14/2016.
  */

object Importer {

  def packageName(imp: Import): String = packageName(imp.identifiers)
  def packageName(packageIds: List[Identifier]): String = packageIds.map(_.value).mkString(".")

}

class Importer(override var ctx: Context, prog: Program) extends ImportErrors {

}