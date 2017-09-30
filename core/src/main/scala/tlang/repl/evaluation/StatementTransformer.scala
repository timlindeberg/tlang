package tlang.repl.evaluation

import tlang.compiler.analyzer.Types.TUnit
import tlang.compiler.ast.Trees
import tlang.compiler.ast.Trees._
import tlang.compiler.code.TreeBuilder

// Adds a variable declaration and print statement for each of the entered
// expressions
case class StatementTransformer(treeBuilder: TreeBuilder, state: ReplState) extends Trees.Transformer {

  import Evaluator.PrintMarker

  private var resultCounter = 0

  // This could potentially transform other code as well
  override protected def _transform(t: Tree): Tree = t match {
    case block@Block(stats) if stats.nonEmpty =>
      stats.last match {
        case Block(newStats) if newStats.length > 2 &&
          newStats.head == PrintMarker &&
          newStats.last == PrintMarker =>
          val newStatements = newStats.flatMap(convert)
          state.setNewStatements(newStatements)
          Block(stats.dropRight(1) ++ newStatements)
        case _                         => block
      }
    case _                                    => super._transform(t)
  }

  private def convert(statTree: StatTree): List[StatTree] = statTree match {
    case Block(stats)                 => Block(stats flatMap convert) :: Nil
    case acc@Access(_, _: MethodCall) => if (acc.getType == TUnit) acc :: Nil else saveAndPrint(acc)
    case UselessStatement(e)          => saveAndPrint(e)
    case _                            => statTree :: Nil
  }

  private def saveAndPrint(e: ExprTree) = {
    val varName = "res" + resultCounter
    resultCounter += 1

    val varDecl = treeBuilder.createValDecl(varName, e, prefix = "")
    val tpe = state.imports.replaceNames(e.getType.toString)

    val varDeclMessage = treeBuilder.stringConcat(StringLit(s"val $varName: $tpe = "), varDecl.id)
    varDecl :: Println(varDeclMessage) :: Nil
  }

}