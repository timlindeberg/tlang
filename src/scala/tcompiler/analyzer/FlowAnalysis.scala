package tcompiler.analyzer

import tcompiler.analyzer.Symbols.VariableSymbol
import tcompiler.ast.Trees._
import tcompiler.imports.ImportMap
import tcompiler.utils.{Context, Pipeline}

/**
  * Created by timlindeberg on 22/07/16.
  */
object FlowAnalysis extends Pipeline[List[CompilationUnit], List[CompilationUnit]] {
  override def run(ctx: Context)(cus: List[CompilationUnit]): List[CompilationUnit] = {
    cus foreach { cu =>
      cu.classes.flatMap(_.methods).flatMap(_.stat) foreach { stat =>
        val flowAnalyser = new FlowAnalyser(ctx, cu.importMap, stat)
        flowAnalyser()
      }
    }
    cus
  }
}

class FlowAnalyser(override var ctx: Context, override var importMap: ImportMap, stat: StatTree) extends FlowAnalysisErrors {

  class Knowledge {
    val hasBeenInitialized = false
    val hasBeenUsed = false
    val hasBeenReassigned = false
    val canBeNull = true
  }

  def apply() = analyze(stat, Map())

  def analyze(tree: Tree, knowledge: Map[VariableID, Knowledge]): Map[VariableID, Knowledge] = tree match {
    case Block(stats)                                   =>
      stats.foldLeft(knowledge)((currentKnowledge, next) => analyze(next, currentKnowledge))
      knowledge
/*    case varDecl@VarDecl(_, id, _, _) =>
      knowledge + (id -> new Knowledge)
    case For(init, condition, post, stat)               =>
      val newKnowledge = init.foldLeft(knowledge)((currentKnowledge, next) => analyze(next, currentKnowledge))
      analyze(condition, newKnowledge)
      analyze(stat, newKnowledge)

      analyze(post, newKnowledge)
      knowledge
    case Foreach(varDecl, container, stat)              =>
      analyze(container, knowledge)
      val newKnowledge = analyze(varDecl, knowledge)
      analyze(stat, newKnowledge)
      knowledge
    case If(expr, thn, els)                             =>
      analyze(expr, localVars, scopeLevel)
      bind(thn, localVars, scopeLevel, canBreakContinue)
      els collect { case e => bind(e, localVars, scopeLevel, canBreakContinue) }
      localVars
    case While(expr, stat)                              =>
      bind(expr, localVars, scopeLevel)
      bind(stat, localVars, scopeLevel, canBreakContinue = true)
      localVars
    case PrintStatTree(expr)                            =>
      bind(expr, localVars, scopeLevel)
      localVars
    case Error(expr)                                    =>
      bind(expr, localVars, scopeLevel)
      localVars
    case Return(expr)                                   =>
      expr collect { case e => bind(e, localVars, scopeLevel) }
      localVars
    case _: Break | _: Continue                         =>
      if (!canBreakContinue)
        ErrorBreakContinueOutsideLoop(statement, statement)
      localVars
    case expr: ExprTree                                 =>
      bindExpr(expr, localVars, scopeLevel)
      localVars*/
  }


}