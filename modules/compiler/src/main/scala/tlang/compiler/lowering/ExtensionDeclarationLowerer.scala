package tlang
package compiler
package lowering

import tlang.compiler.analyzer.Symbols._
import tlang.compiler.analyzer.Types
import tlang.compiler.ast.Trees
import tlang.compiler.ast.Trees._

/*
 * Replaces an extensions class with a class with all static methods.
 *
 * Example:
 *
 * --------------------------------------------------------------------------------
 * extension Extension : A =
 *
 *   Def Method(arg1: A, arg2: Int) =
 *     OtherMethod() + arg1.i + i + arg2.M()
 *
 *   Def static StaticMethod(arg: Int) = arg + 1
 *
 * becomes:
 *
 * class ext$Extension =
 *
 *   Def static Method($this: A, arg1: A, arg2: Int) =
 *     $this.OtherMethod() + arg1.i + $this.i + arg2.M()
 *
 *   Def static StaticMethod(arg: Int) = arg + 1
 * --------------------------------------------------------------------------------
 */
case class ExtensionDeclarationLowerer() extends TreeLowerer {

  override def lower: PartialFunction[Tree, Tree] = {
    case extensionDecl: ExtensionDecl =>
      val extensionClassSymbol = extensionDecl.getSymbol.asInstanceOf[ExtensionClassSymbol]
      val exName = ExtensionDecl.nameToExtensionName(extensionClassSymbol.name)

      val classSymbol = new ClassSymbol(exName)
      val extendedClass = (Constants.TExtendedClassName, StringAnnotationValue(extensionClassSymbol.getExtendedType.name))
      val extensionClassAnnotation = AnnotationSymbol(Constants.TExtensionClassAnnotation, List(extendedClass))
        .setType(Types.ExtensionClassAnnotation)
      classSymbol.addAnnotation(extensionClassAnnotation)

      val newMethods = extensionDecl.methods.map { meth =>
        replaceMethod(extensionDecl, classSymbol, extensionClassSymbol, meth)
      }

      val classId = ClassID(exName).setSymbol(classSymbol)
      ClassDecl(classId, Nil, Nil, newMethods).setSymbol(classSymbol).setPos(extensionDecl)
  }

  private def replaceMethod(
    extensionDecl: ExtensionDecl,
    classSymbol: ClassSymbol,
    extensionClassSymbol: ExtensionClassSymbol,
    meth: MethodDeclTree): MethodDeclTree = {
    if (meth.isStatic) {
      // No need to transform static methods
      return meth
    }
    val methSym = meth.getSymbol
    val modifiers = meth.modifiers + Static()
    val extendedType = extensionClassSymbol.getExtendedType
    val thisSym = new VariableSymbol(TreeBuilder.ThisName).setType(extendedType)
    val thisId = VariableID(TreeBuilder.ThisName).setSymbol(thisSym)
    val newMethSym = new MethodSymbol(methSym.name, classSymbol, None, modifiers).setType(methSym)
    newMethSym.argList = thisSym :: methSym.argList
    newMethSym.args = methSym.args + (TreeBuilder.ThisName -> thisSym)

    val annotation = AnnotationSymbol(Constants.TExtensionMethodAnnotation).setType(Types.ExtensionMethodAnnotation)
    newMethSym.addAnnotation(annotation)
    val thisArg = Formal(extensionDecl.extendedType, thisId).setSymbol(thisSym)

    // Replace references to this with the this variable
    val newStat = meth.stat map { s => replaceThis(s, thisId) }
    MethodDecl(meth.id, modifiers, meth.annotations, thisArg :: meth.args, meth.retType, newStat).setSymbol(newMethSym)
  }

  private def replaceThis(stat: StatTree, thisId: VariableID) = {
    val transformThis = new Trees.Transformer {
      def transformation: TreeTransformation = {
        case This()                                                      =>
          thisId
        case acc@Access(obj, app)                                        =>
          // Don't replace VariableIDs in applications since that
          // would replace e.g A.i with A.$this.i
          val a = app match {
            case _: VariableID => app
            case _             => apply(app)
          }
          copier.NormalAccess(acc, apply(obj), a)
        case v@VariableID(name) if v.getSymbol.isInstanceOf[FieldSymbol] =>
          NormalAccess(thisId, v).setType(v)
      }
    }
    transformThis(stat)
  }
}
