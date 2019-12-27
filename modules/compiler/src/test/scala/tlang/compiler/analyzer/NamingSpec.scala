package tlang
package compiler
package analyzer

import tlang.compiler.analyzer.Symbols._
import tlang.compiler.ast.Trees._
import tlang.compiler.imports.{ClassSymbolLocator, Imports}
import tlang.compiler.messages.Reporter
import tlang.compiler.output.PrettyOutputHandler
import tlang.formatting.{ErrorStringContext, Formatter}
import tlang.testutils.{TreeTesting, UnitSpec}
import tlang.testutils.matcher.SymbolMatchers

class NamingSpec extends UnitSpec with SymbolMatchers with TreeTesting {

  behavior of "A Name Analyzer"

  it should "add symbols to classes" in {
    val classes = List(
      ClassDecl("A"),
      TraitDecl("B"),
      ClassDecl("C")
    )

    val globalScope = new GlobalScope(mock[ClassSymbolLocator])
    val nameAnalyser = createNameAnalyzer(classes, globalScope)
    nameAnalyser.addSymbols()

    all(classes) should haveSymbol

    val symbolA :: symbolB :: symbolC :: Nil = classes.map(_.getSymbol)
    symbolA.name shouldBe "A"
    symbolB.name shouldBe "B"
    symbolC.name shouldBe "C"

    symbolB.isAbstract shouldBe true

    globalScope.classNames should contain allOf("A", "B", "C")

    globalScope.lookupClass(imports, "A").value shouldBe symbolA
    globalScope.lookupClass(imports, "B").value shouldBe symbolB
    globalScope.lookupClass(imports, "C").value shouldBe symbolC
  }

  it should "add symbols to methods" in {
    val methods = List(
      MethodDecl("A", modifiers = Set(Static(), Final(), Private())),
      MethodDecl("B", stat = Block(Nil), modifiers = Set(Static(), Public())),
      ConstructorDecl(MethodID("new"), stat = Block(Nil), modifiers = Set(Protected(), Implicit())),
      OperatorDecl(Plus(Empty(), Empty()), stat = Block(Nil))
    )
    val classA = ClassDecl("A", methods = methods)

    val nameAnalyser = createNameAnalyzer(classA)
    nameAnalyser.addSymbols()

    all(methods) should haveSymbol

    val methodSymbols = methods.map(_.getSymbol)
    all(methodSymbols.map(_.classSymbol)) shouldBe classA.getSymbol

    val symbolA :: symbolB :: symbolNew :: symbolPlus :: Nil = methodSymbols
    symbolA.name shouldBe "A"
    symbolB.name shouldBe "B"
    symbolNew.name shouldBe "new"
    symbolPlus.name shouldBe "$Plus"

    symbolA shouldBe a[MethodSymbol]
    symbolB shouldBe a[MethodSymbol]
    symbolNew shouldBe a[MethodSymbol]
    symbolPlus shouldBe an[OperatorSymbol]

    symbolA.modifiers should contain allOf(Static(), Final(), Private())
    symbolB.modifiers should contain allOf(Static(), Public())
    symbolNew.modifiers should contain allOf(Protected(), Implicit())

    symbolNew.annotations should contain(AnnotationSymbol(Constants.TImplicitConstructorAnnotation))

    symbolA.isAbstract shouldBe true
  }

  it should "add symbols to fields" in {
    val fields = List(
      VarDecl("A", modifiers = Set(Static(), Final(), Private())),
      VarDecl("B", modifiers = Set(Static(), Public())),
      VarDecl("C", modifiers = Set(Protected()))
    )
    val classA = ClassDecl("A", fields = fields)

    val nameAnalyser = createNameAnalyzer(classA)
    nameAnalyser.addSymbols()

    all(fields) should haveSymbol
    all(fields.map(_.id)) should haveSymbol

    val symbols = fields.map(_.getSymbol)
    all(symbols) shouldBe a[FieldSymbol]
    val fieldSymbols = symbols.map(_.asInstanceOf[FieldSymbol])
    all(fieldSymbols.map(_.classSymbol)) shouldBe classA.getSymbol

    val symbolA :: symbolB :: symbolC :: Nil = fieldSymbols
    symbolA.name shouldBe "A"
    symbolB.name shouldBe "B"
    symbolC.name shouldBe "C"

    symbolA.modifiers should contain allOf(Static(), Final(), Private())
    symbolB.modifiers should contain allOf(Static(), Public())
    symbolC.modifiers should contain(Protected())

    classA.getSymbol.fields should have size 3
    classA.getSymbol.fields.keys should contain allOf("A", "B", "C")
    classA.getSymbol.fields.values should contain allElementsOf fieldSymbols
  }

  it should "add symbols to method arguments" in {
    val methodArgs = List(Formal(ClassID("A"), VariableID("a")), Formal(ClassID("B"), VariableID("b")))
    val constructorArgs = List(Formal(ClassID("A"), VariableID("a")), Formal(ClassID("B"), VariableID("b")))
    val operatorArgs = List(Formal(ClassID("A"), VariableID("a")), Formal(ClassID("B"), VariableID("b")))
    val methods = List(
      MethodDecl("A", args = methodArgs),
      ConstructorDecl(MethodID("new"), args = constructorArgs),
      OperatorDecl(Plus(Empty(), Empty()), args = operatorArgs)
    )
    val classA = ClassDecl("A", methods = methods)

    val nameAnalyser = createNameAnalyzer(classA)
    nameAnalyser.addSymbols()

    all(methodArgs) should haveSymbol
    all(methodArgs.map(_.id)) should haveSymbol

    all(constructorArgs) should haveSymbol
    all(constructorArgs.map(_.id)) should haveSymbol

    all(operatorArgs) should haveSymbol
    all(operatorArgs.map(_.id)) should haveSymbol

    val methA :: methB :: Nil = methodArgs.map(_.getSymbol)
    methA.name shouldBe "a"
    methB.name shouldBe "b"

    val consA :: consB :: Nil = constructorArgs.map(_.getSymbol)
    consA.name shouldBe "a"
    consB.name shouldBe "b"

    val opA :: opB :: Nil = operatorArgs.map(_.getSymbol)
    opA.name shouldBe "a"
    opB.name shouldBe "b"

    methA.modifiers should contain allOf(Private(), Final())
    consA.modifiers should contain allOf(Private(), Final())
    opA.modifiers should contain allOf(Private(), Final())

    val methSym :: consSym :: opSym :: Nil = methods.map(_.getSymbol)

    methSym.args should have size 2
    methSym.args should contain allOf("a" -> methA, "b" -> methB)
    methSym.argList should have size 2
    methSym.argList should contain allOf(methA, methB)

    consSym.args should have size 2
    consSym.args should contain allOf("a" -> consA, "b" -> consB)
    consSym.argList should have size 2
    consSym.argList should contain allOf(consA, consB)

    opSym.args should have size 2
    opSym.args should contain allOf("a" -> opA, "b" -> opB)
    opSym.argList should have size 2
    opSym.argList should contain allOf(opA, opB)
  }

  it should "add parent symbols in binding stage" ignore {}
  it should "add methods to class symbol in binding stage" ignore {}

  /* -------------------- Helpers ---------------------*/

  private implicit val formatter: Formatter = testFormatter(useColor = false)

  private val errorStringContext = ErrorStringContext()
  private val reporter = mock[Reporter]
  private val ctx = Context(reporter, PrettyOutputHandler())
  private val imports = Imports(ctx, errorStringContext, Nil)

  private def createNameAnalyzer(clazz: ClassDeclTree): NameAnalyser = {
    createNameAnalyzer(List(clazz))
  }
  private def createNameAnalyzer(clazz: ClassDeclTree, globalScope: GlobalScope): NameAnalyser = {
    createNameAnalyzer(List(clazz), globalScope)
  }
  private def createNameAnalyzer(classes: List[ClassDeclTree]): NameAnalyser = {
    createNameAnalyzer(classes, new GlobalScope(mock[ClassSymbolLocator]))
  }

  private def createNameAnalyzer(classes: List[ClassDeclTree], globalScope: GlobalScope): NameAnalyser = {
    val cu = CompilationUnit(Package(), classes, imports)
    NameAnalyser(reporter, errorStringContext, cu, globalScope)
  }
}
