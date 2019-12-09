package tlang
package repl
package evaluation

import tlang.compiler.ast.Trees._
import tlang.compiler.imports.Imports
import tlang.compiler.testutils.TreeTesting
import tlang.formatting.Formatter
import tlang.formatting.textformatters.SyntaxHighlighter
import tlang.testutils.UnitSpec

class ExtractorSpec extends UnitSpec with TreeTesting {

  behavior of "An extractor"

  private val syntaxHighlighter = mockedSyntaxHighlighter

  it should "extract classes" in {
    val replState = mock[ReplState]
    val extractor = makeExtractor(replState)
    val replClass = ClassDecl(Evaluator.ReplClassID)
    val classA = ClassDecl("A")
    val classB = ClassDecl("B")
    val classC = ClassDecl("C")

    val cu = createCU(replClass, classA, classB, classC)()

    val messages = extractor(cu)
    messages should contain theSameElementsInOrderAs Seq(
      "Defined class A",
      "Defined class B",
      "Defined class C"
    )

    there was one(replState).addClasses(List(classA, classB, classC))
  }

  it should "extract methods" in {
    val replState = mock[ReplState]
    val extractor = makeExtractor(replState)

    val methodA = createMethod(name = "A", retType = IntType)
    val methodB = createMethod(
      name = "B",
      args = List(StringType),
      retType = UnitType()
    )
    val methodC = createMethod(
      name = "C",
      args = List(IntType, StringType),
      retType = StringType
    )

    val replClass = ClassDecl(Evaluator.ReplClassID, methods = List(methodA, methodB, methodC))

    val cu = createCU(replClass)()

    val messages = extractor(cu)

    messages should contain theSameElementsInOrderAs Seq(
      "Defined method A(): Int",
      "Defined method B(String): Unit",
      "Defined method C(Int, String): String"
    )

    there was one(replState).addMethods(List(methodA, methodB, methodC))
  }

  it should "extract imports" in {
    val replState = mock[ReplState]
    val extractor = makeExtractor(replState)
    val imports = mock[Imports]

    imports.imports returns List(
      RegularImport("ABC" :: "DEF" :: "GHI" :: Nil),
      RegularImport("java" :: "lang" :: "String" :: Nil),
      WildCardImport("java" :: "util" :: Nil)
    )

    val cu = createCU()(imports)

    val messages = extractor(cu)

    messages should contain theSameElementsInOrderAs Seq(
      "Imported ABC::DEF::GHI",
      "Imported java::lang::String",
      "Imported java::util::*"
    )

    there was one(replState).addImports(imports)
  }

  it should "extract statements" in {
    val replState = mock[ReplState]
    val extractor = makeExtractor(replState)

    val statement1 = VarDecl("A", tpe = Some(IntType))
    val statement2 = Plus(IntLit(1), IntLit(1))
    val statement3 = VarDecl("B", tpe = Some(StringType))
    val statement4 = VarDecl("C", tpe = None)

    val statements = List(statement1, statement2, statement3, statement4)

    val mainMethod = MethodDeclTree.mainMethod(statements)

    val replClass = ClassDecl(Evaluator.ReplClassID, methods = List(mainMethod))

    val cu = createCU(replClass)()

    val messages = extractor(cu)

    messages should contain theSameElementsInOrderAs Seq(
      "Defined variable A: Int",
      "Defined variable B: String",
      "Defined variable C"
    )

    there was one(replState).setNewStatements(statements)
  }

  it should "extract multiple" in {
    val replState = mock[ReplState]
    val extractor = makeExtractor(replState)

    val classA = ClassDecl("A")
    val methodA = MethodDecl("A", retType = IntType)

    val imports = mock[Imports]
    imports.imports returns List(RegularImport("ABC" :: "DEF" :: "GHI" :: Nil))

    val statement = VarDecl("A", tpe = Some(IntType))
    val mainMethod = MethodDeclTree.mainMethod(List(statement))

    val replClass = ClassDecl(Evaluator.ReplClassID, methods = List(mainMethod, methodA))

    val cu = createCU(classA, replClass)(imports)
    val messages = extractor(cu)

    messages should contain theSameElementsInOrderAs Seq(
      "Imported ABC::DEF::GHI",
      "Defined class A",
      "Defined method A(): Int",
      "Defined variable A: Int"
    )

    there was one(replState).addImports(imports)
    there was one(replState).addClasses(List(classA))
    there was one(replState).addMethods(List(methodA))
    there was one(replState).setNewStatements(List(statement))
  }

  private def formatter: Formatter = {
    val syntaxHighlighter = mock[SyntaxHighlighter]
    syntaxHighlighter.apply(*).forwardsArg(0)
    testFormatter(useColor = false, syntaxHighlighter = syntaxHighlighter)
  }

  private def makeExtractor(replState: ReplState): Extractor = {
    Extractor(syntaxHighlighter, replState)(formatter)
  }
}
