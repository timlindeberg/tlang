package tlang
package compiler
package testutils

import tlang.compiler.ast.Trees._
import tlang.compiler.imports.Imports
import tlang.testutils.MockitoSugar


trait TreeTesting extends MockitoSugar {

  val IntType = ClassID("Int")
  val StringType = ClassID("String")

  implicit def stringToClassId(str: String): ClassID = ClassID(str)
  implicit def stringToVariableId(str: String): VariableID = VariableID(str)
  implicit def stringToMethodId(str: String): MethodID = MethodID(str)
  implicit def valueToOption[T](t: T): Option[T] = Some(t)

  def createMethod(name: String, args: List[TypeTree] = Nil, retType: Option[TypeTree] = None): MethodDecl = {
    val a = args
      .zip("abcdefghijklmnopqrstuvxyz")
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
