package tlang
package compiler
package lowering

import tlang.compiler.analyzer.Symbols._
import tlang.compiler.analyzer.Types.{Bool, Int, TArray, TObject}
import tlang.compiler.ast.Trees._
import tlang.compiler.imports.Imports

/*
 * Lowers for each loops, either array based or an iterator based.
 *
 * Examples:
 * --------------------------------------------------------------------------------
 * for(<varDecl> in <array>)
 *   <code>
 *
 * becomes:
 *
 * val $container = <array>
 * for(var $i = 0; $i < $container.Size(); i++)
 *   <varDecl> = $container[$i]
 *   <code>
 * --------------------------------------------------------------------------------
 * for(<varDecl> in <container>)
 *   <code>
 *
 * becomes:
 *
 * val $it = <container>.Iterator()
 * while($it.HasNext())
 *   <varDecl> = $it.Iterator()
 *   <code>
 *
 * --------------------------------------------------------------------------------
 */
case class ForeachLowerer(imports: Imports) extends TreeLowerer {

  val treeBuilder = new TreeBuilder

  import treeBuilder._

  override def lower: PartialFunction[Tree, Tree] = {
    case Foreach(varDecl, container, stat) => container.getType match {
      case _: TArray            => lowerArrayForeach(varDecl, container, stat)
      case TObject(classSymbol) => lowerIteratorForeach(classSymbol, varDecl, container, stat)
      case _                    => ???
    }
  }

  private def lowerArrayForeach(varDecl: VarDecl, container: ExprTree, stat: StatTree) = {
    val indexDecl = createVarDecl("i", IntLit(0))
    val index = indexDecl.id

    val containerId = putVarDecl("container", container)

    val sizeCall = createMethodCall(containerId, "Size", Int)

    val comparison = LessThan(index, sizeCall).setType(Bool).setPos(varDecl)
    val post = Assign(index, Plus(index, IntLit(1)).setType(Int)).setType(Int).setPos(varDecl)

    val arrReadType = containerId.getType.asInstanceOf[TArray].tpe
    val init = Some(ArrayRead(containerId, index).setType(arrReadType).setPos(varDecl))
    val valInit = varDecl.copy(initiation = init).setPos(stat)
    valInit.setSymbol(varDecl.getSymbol).setPos(varDecl)
    val stats = createBlock(stat, valInit)

    put(For(List(indexDecl), comparison, List(post), stats).setPos(stat))
    getCode
  }

  private def lowerIteratorForeach(classSymbol: ClassSymbol, varDecl: VarDecl, container: ExprTree, stat: StatTree) = {
    val iteratorCall = createMethodCall(container, classSymbol, "Iterator", imports, List())
    val iterator = putVarDecl("it", iteratorCall)

    val iteratorClass = iteratorCall.getType.asInstanceOf[TObject].classSymbol

    val comparisonCall = createMethodCall(iterator, iteratorClass, "HasNext", imports, List())
    val nextMethodCall = createMethodCall(iterator, iteratorClass, "Next", imports, List())

    val valInit = VarDecl(varDecl.id, varDecl.tpe, Some(nextMethodCall), varDecl.modifiers).setPos(stat)
    valInit.setSymbol(varDecl.getSymbol)
    val stats = createBlock(stat, valInit)
    put(While(comparisonCall, stats).setPos(stat))
    getCode
  }

  private def createBlock(stat: StatTree, valInit: VarDecl) = stat match {
    case Block(s) => Block(valInit :: s)
    case _        => Block(List(valInit, stat))
  }
}
