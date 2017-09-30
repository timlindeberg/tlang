package tlang.testutils

import tlang.compiler.ast.Trees._
import tlang.compiler.imports.Imports
import tlang.utils.Extensions._

trait TreeTesting extends UnitSpec {

  val IntType    = ClassID("Int")
  val StringType = ClassID("String")


  def createClass(name: String, methods: List[MethodDeclTree] = Nil): ClassDecl = {
    ClassDecl(ClassID(name), methods = methods)
  }

  def createMethod(name: String, args: List[TypeTree] = Nil, retType: Option[TypeTree] = None): MethodDecl = {
    val a = args
      .zip("ABCDEFGHIJKLMNOPQRSTUVXYZ".toCharArray)
      .map { case (tpe, name) => Formal(tpe, VariableID("" + name)) }
    MethodDecl(MethodID(name), args = a, retType = retType)
  }

  def createCU(classes: ClassDecl*)(imports: Imports = mockImports): CompilationUnit = {
    CompilationUnit(Package(), classes.toList, imports)
  }

  def mockImports: Imports = {
    mock[Imports] use { imports =>
      imports.imports returns Nil
    }
  }

}
