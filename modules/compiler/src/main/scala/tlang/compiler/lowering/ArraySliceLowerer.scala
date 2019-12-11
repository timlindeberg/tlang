package tlang
package compiler
package lowering

import tlang.compiler.analyzer.Types.{Bool, Int, TArray}
import tlang.compiler.ast.Trees._

/*
 * Transforms an array slice. Will not be needed once we have generic extension
 * methods.
 *
 * Example:
 * --------------------------------------------------------------------------------
 * val a = <arr>[<start>:<end>:<step>]
 *
 * becomes:
 *
 * var $container = <arr>
 * var $start     = <start> | 0
 * var $end       = <end>   | container.Size())
 * var $step      = <step>  | 1
 * var $slice = new <arrTpe>[(($start - $end) + $step - 1) / $step)]
 * for(var $i = $start; i < $end; i += $step)
 *   $slice[$start - $i] = $container[$i]
 * $slice
 * --------------------------------------------------------------------------------
 */
case class ArraySliceLowerer() extends TreeLowerer {

  private val treeBuilder = new TreeBuilder

  import treeBuilder._

  override def lower: PartialFunction[Tree, Tree] = {
    case arraySlice: ArraySlice => lowerArraySlice(arraySlice)
  }

  private def lowerArraySlice(arraySlice: ArraySlice): Tree = {
    val arr = arraySlice.arr
    val objectType = arr.getType

    assert(objectType.isInstanceOf[TArray])
    val arrType = objectType.asInstanceOf[TArray].tpe

    val container = putVarDecl("container", arr)

    // TODO: This needs a check and an error message if the class does not have a Size method
    val sizeCall = createMethodCall(container, "Size", Int)

    val start = putVarDecl("start", arraySlice.start.getOrElse(IntLit(0)))
    val end = putVarDecl("end", arraySlice.end.getOrElse(sizeCall).setType(Int))
    val step = putVarDecl("step", arraySlice.step.getOrElse(IntLit(1)))

    var size: ExprTree = Minus(end, start).setType(Int)
    if (arraySlice.step.isDefined)
      size = Div(Plus(size, Minus(step, IntLit(1)).setType(Int)).setType(Int), step).setType(Int)

    val typeTree = getTypeTree(objectType)
    val newArray = NewArray(typeTree, List(size)).setType(arr)
    val slice = putVarDecl("slice", newArray)

    val indexDecl = createVarDecl("i", start)
    val indexId = indexDecl.id
    val comparison = LessThan(indexId, end).setType(Bool)
    val post = Assign(indexId, Plus(indexId, step).setType(Int)).setType(Int)

    val index = Div(Minus(indexId, start).setType(Int), step).setType(Int)
    val toSlice = ArrayRead(slice, index).setType(arraySlice)
    val fromArr = ArrayRead(container, indexId).setType(arrType)
    val copyValue = Assign(toSlice, fromArr).setType(arrType)

    put(For(List(indexDecl), comparison, List(post), copyValue))
    put(PutOnStack(slice))

    setPos(arraySlice)

    getCode
  }
}
