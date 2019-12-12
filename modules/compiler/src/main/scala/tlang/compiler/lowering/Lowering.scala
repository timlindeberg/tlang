package tlang
package compiler
package lowering

import tlang.compiler.analyzer.Symbols._
import tlang.compiler.analyzer.Types._
import tlang.compiler.ast.Trees
import tlang.compiler.ast.Trees._
import tlang.compiler.imports.Imports
import tlang.compiler.output.Output
import tlang.compiler.output.debug.ASTOutput
import tlang.formatting.Formatter
import tlang.utils.Logging

object Lowering extends CompilerPhase[CompilationUnit, CompilationUnit] with Logging {

  override def run(ctx: Context)(cus: List[CompilationUnit]): List[CompilationUnit] = {
    val lowerers = cus.map { cu => new Lowerer(cu.imports) }
    val loweredDecls = ctx.executor.map(lowerers.zip(cus)) { case (lowerer, cu) =>
      lowerer.lowerDeclarations(cu)
    }
    ctx.executor.map(lowerers.zip(loweredDecls)) { case (lowerer, cu) => lowerer.lowerCode(cu) }
  }

  override def description(implicit formatter: Formatter): String =
    "Lowers the tree to simpler components. Performs desugaring."

  override def debugOutput(output: List[CompilationUnit])(implicit formatter: Formatter): Output = ASTOutput(phaseName, output)
}

class Lowerer(imports: Imports) extends Logging {

  private val extensionDeclarationLowerer = ExtensionDeclarationLowerer()
  private val extensionCallLowerer = ExtensionCallLowerer()
  private val operatorCallLowerer = OperatorCallLowerer(imports)
  private val operatorDeclLowerer = OperatorDeclarationLowerer()
  private val incDecLowerer = IncrementDecrementLowerer()
  private val foreachLowerer = ForeachLowerer(imports)
  private val arraySliceLowerer = ArraySliceLowerer()
  private val safeAccessLowerer = SafeAccessLowerer()
  private val elvisLowerer = ElvisOperatorLowerer()

  def lowerDeclarations(cu: CompilationUnit): CompilationUnit = {
    debug"Lowering declarations in ${ cu.simpleSourceDescription }"
    val transformer = new Trees.Transformer {

      def transformation: TreeTransformation = {
        case opDecl: OperatorDecl         => operatorDeclLowerer(opDecl)
        case extensionDecl: ExtensionDecl => extensionDeclarationLowerer(transformChildren(extensionDecl))
        case methodDecl: MethodDeclTree   => methodDecl // stop here for now, no need to recurse in to stats etc.
      }
    }
    transformer(cu)
  }

  def lowerCode(cu: CompilationUnit): CompilationUnit = {
    debug"Lowering code in ${ cu.simpleSourceDescription }"
    val transformer = new Trees.Transformer {

      def transformation: TreeTransformation = {
        case slice: ArraySlice if slice.arr.getType.isInstanceOf[TArray]    =>
          transformChildren(arraySliceLowerer(transformChildren(slice)))
        case incDec: IncrementDecrementTree if incDec.getType in Primitives =>
          transformChildren(incDecLowerer(incDec))
        case safeAccess: SafeAccess                                         =>
          // Recurse again to replace all calls in the lowered version
          transformChildren(safeAccessLowerer(safeAccess))
        case acc@NormalAccess(_, MethodCall(meth, _))                       =>
          val newAcc = transformChildren(acc)
          val classSymbol = meth.getSymbol.classSymbol
          classSymbol match {
            case _: ExtensionClassSymbol => extensionCallLowerer(newAcc)
            case _                       => newAcc
          }
        case assign: Assign                                                 =>
          val to = assign.to
          to match {
            case ArrayRead(arr, _) if arr.getType.isInstanceOf[TObject] =>
              val expr = apply(assign.from)
              val newAssign = copier.Assign(assign, to, expr)
              // Transform again to replace external method calls etc.
              apply(operatorCallLowerer(newAssign))
            case _                                                      => transformChildren(assign)
          }
        case op: OperatorTree                                               =>
          operatorCallLowerer(transformChildren(op)) match {
            case op: OperatorTree => op // Finished transform
            case tree             => apply(tree) // Transform again to replace external method calls etc.
          }
        case foreach: Foreach                                               => apply(foreachLowerer(foreach))
        case elvis: Elvis                                                   => apply(elvisLowerer(elvis))
      }
    }
    transformer(cu)
  }
}


