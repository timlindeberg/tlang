package tlang
package compiler
package lowering

import tlang.compiler.analyzer.Types.Bool
import tlang.compiler.ast.Trees._

import scala.collection.mutable.ListBuffer

/*
 * Transforms safe access calls
 *
 * Examples:
 * --------------------------------------------------------------------------------
 * A?.GetB()
 *
 * becomes:
 *
 * A != null ? A.GetB() : null
 * --------------------------------------------------------------------------------
 * A?.GetB()?.GetC()?.GetD()?.E
 *
 * becomes:
 *
 * if(A != null) {
 *   val tmp$1 = A.GetB()
 *   if(tmp$1 != null) {
 *     val tmp$2 = tmp$1.GetC()
 *     if(tmp$2 != null) {
 *       val tmp$3 = tmp$2.GetD()
 *       tmp$3 != null ? tmp$3.E : null
 *     } else {
 *       null
 *   } else {
 *     null
 * } else {
 *   null
 * }
 * --------------------------------------------------------------------------------
 */
case class SafeAccessLowerer() extends TreeLowerer {

  private val treeBuilder = new TreeBuilder

  import treeBuilder._

  override def lower: PartialFunction[Tree, Tree] = {
    case rootSafeAccess: SafeAccess =>
      val applications = new ListBuffer[ExprTree]()

      var safeAccess = rootSafeAccess.asInstanceOf[ExprTree]
      while (safeAccess.isInstanceOf[SafeAccess]) {
        val s = safeAccess.asInstanceOf[SafeAccess]
        applications += s.application
        safeAccess = s.obj
      }

      val applicationsInOrder = applications.reverse.toList
      val obj = safeAccess

      put(lowerSafeAccessRecursively(1, obj, applicationsInOrder))
      setPos(rootSafeAccess)
      getCode.setType(rootSafeAccess)
  }

  private def lowerSafeAccessRecursively(depth: Int, obj: ExprTree, applications: List[ExprTree]): StatTree = {
    val condition = NotEquals(obj, NullLit()).setType(Bool)
    val app = applications.head
    val access = NormalAccess(obj, app).setType(app.getType.getNonNullable)

    if (applications.lengthCompare(1) == 0) {
      val ternary = Ternary(condition, access, NullLit()).setType(app.getType.getNullable)
      return PutOnStack(ternary)
    }
    val ifNull = PutOnStack(NullLit())
    val varDecl = createVarDecl(s"tmp$depth", access)
    val thn = List(varDecl, lowerSafeAccessRecursively(depth + 1, varDecl.id, applications.tail))
    If(condition, Block(thn), Some(Block(List(ifNull))))
  }
}
