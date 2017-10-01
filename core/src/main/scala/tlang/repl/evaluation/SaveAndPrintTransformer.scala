package tlang.repl.evaluation

import tlang.compiler.analyzer.Types.TUnit
import tlang.compiler.ast.Trees
import tlang.compiler.ast.Trees._
import tlang.compiler.code.TreeBuilder

/**
  * Adds a variable declaration and print statement for each of the entered
  * expressions.
  *
  * Example:
  * Input:
  *
  * <PrintMarker>
  * 1 + 1
  * 2 + 2
  * <PrintMarker>
  *
  * Output:
  * <PrintMarker>
  * val res0: Int = 1 + 1
  * println("val res0: Int = " + res0)
  * val res1: Int = 2 + 2
  * println("val res1: Int = " + res1)
  * <PrintMarker>
  */
case class SaveAndPrintTransformer(treeBuilder: TreeBuilder, state: ReplState) {

  import Evaluator.PrintMarker

  private var resultCounter = 0

  def apply[T <: Tree](t: T) = transformer(t)

  // We use a variable instead of making the class extend from transformer
  // since it seems Mockito has trouble mocking classes which are expanded
  // using macros.
  private val transformer = new Trees.Transformer {
    override def transformation: TreeTransformation = {
      case block@Block(stats) if stats.nonEmpty =>
        // This could potentially transform other code as well
        stats.last match {
          case Block(newStats) if newStats.length > 2 &&
            newStats.head == PrintMarker &&
            newStats.last == PrintMarker =>
            val statementsWithSaveAndPrint = newStats flatMap addSaveAndPrint
            state.setNewStatements(statementsWithSaveAndPrint)
            Block(stats.dropRight(1) ++ statementsWithSaveAndPrint)
          case _                         => block
        }
    }
  }


  private def addSaveAndPrint(statTree: StatTree): List[StatTree] = statTree match {
    case Block(stats)                 => Block(stats flatMap addSaveAndPrint) :: Nil
    case acc@Access(_, _: MethodCall) => if (acc.getType == TUnit) acc :: Nil else saveAndPrint(acc)
    case UselessStatement(e)          => saveAndPrint(e)
    case _                            => statTree :: Nil
  }

  private def saveAndPrint(e: ExprTree) = {
    val varName = "res" + resultCounter
    resultCounter += 1

    val valDecl = treeBuilder.createValDecl(varName, e, prefix = "")
    val tpe = state.imports.replaceNames(e.getType.toString)

    val varDeclMessage = treeBuilder.stringConcat(StringLit(s"val $varName: $tpe = "), valDecl.id)
    valDecl :: Println(varDeclMessage) :: Nil
  }

}