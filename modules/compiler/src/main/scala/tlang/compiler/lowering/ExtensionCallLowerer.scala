package tlang
package compiler
package lowering

import tlang.compiler.analyzer.Symbols._
import tlang.compiler.ast.Trees
import tlang.compiler.ast.Trees._

/*
 * Replaces a call to an extension method with a static call.
 *
 * Examples:
 *
 * --------------------------------------------------------------------------------
 * a.ExtensionMethod(1, 2, 3)
 *
 * Becomes:
 *
 * ext$AExtension::A.ExtensionMethod(a, 1, 2, 3)
 * --------------------------------------------------------------------------------
 */
case class ExtensionCallLowerer() extends TreeLowerer {

  private val treeCopy = new Trees.LazyCopier

  override def lower: PartialFunction[Tree, Tree] = {
    case acc@Access(obj, method@MethodCall(meth, args)) =>
      val methSym = meth.getSymbol
      val extSymbol = meth.getSymbol.classSymbol.asInstanceOf[ExtensionClassSymbol]
      val className = ExtensionDecl.nameToExtensionName(extSymbol.name)
      val classSym = new ClassSymbol(className)
      val classId = ClassID(className).setSymbol(classSym)

      if (methSym.isStatic) {
        treeCopy.NormalAccess(acc, classId, method)
      } else {
        val modifiers = methSym.modifiers + Static()
        val newMethSymbol = new MethodSymbol(methSym.name, classSym, None, modifiers).setType(methSym)
        val originalClass = extSymbol.getExtendedType
        val newArg = new VariableSymbol(TreeBuilder.ThisName).setType(originalClass)
        newMethSymbol.argList = newArg :: methSym.argList
        val methId = MethodID(meth.name).setSymbol(newMethSymbol)
        val newMethCall = treeCopy.MethodCall(method, methId, obj :: args)
        treeCopy.NormalAccess(acc, classId, newMethCall)
      }
  }
}
