package tlang
package compiler
package lowering

import tlang.compiler.analyzer.Symbols.{FieldSymbol, VariableSymbol}
import tlang.compiler.analyzer.Types
import tlang.compiler.ast.Trees._
import tlang.testutils.{TreeTesting, UnitSpec}

class LoweringSpec extends UnitSpec with TreeTesting {

  it should "lower the elvis operator" in {
    // v ?: 5
    // ---------------------------
    // val $tmp = v
    // $tmp == null ? 5 : $tmp

    val a = createVariable("a")
    val int5 = createIntLit(5)
    val elvisOperator = Elvis(a, int5).setType(Types.Int).setRandomPos

    val lowered = ElvisOperatorLowerer()(elvisOperator)
    lowered shouldBe GeneratedExpr(List(
      VarDecl("$tmp", None, Some(a), Set(Private())),
      PutOnStack(
        Ternary(Equals(VariableID("$tmp"), NullLit()), int5, VariableID("$tmp"))
      )
    ))
    lowered.getPos shouldBe elvisOperator.getPos
  }

  it should "lower extension calls" in {
    test("normal call") {
      // a.ExtensionMethod(1, 2, 3)
      // ---------------------------
      // ext$AExtension::ExtensionMethod(a, 1, 2, 3)
      val aClass = createClass("A")
      val a = createVariable("a", aClass.getSymbol.getType)
      val args = List(createIntLit(1), createIntLit(2), createIntLit(3))
      val meth = createMethod("ExtensionMethod", args = List(Types.Int, Types.Int, Types.Int))
      createExtensionClass("AExtension", aClass, methods = List(meth))

      val methodCall = NormalAccess(a, MethodCall(meth.id, args)).setRandomPos

      val lowered = ExtensionCallLowerer()(methodCall)
      lowered shouldBe NormalAccess(
        ClassID("ext$AExtension"),
        MethodCall(MethodID("ExtensionMethod"), a :: args))
      lowered.getPos shouldBe methodCall.getPos
    }

    test("static call") {
      // A::StaticExtensionMethod(1, 2, 3)
      // ---------------------------
      // ext$AExtension::ExtensionMethod(1, 2, 3)
      val aClass = createClass("A")
      val args = List(createIntLit(1), createIntLit(2), createIntLit(3))
      val meth = createMethod(
        "ExtensionMethod",
        args = List(Types.Int, Types.Int, Types.Int),
        modifiers = Set(Public(), Static())
      )
      createExtensionClass("AExtension", aClass, methods = List(meth))
      val methodCall = NormalAccess(aClass.id, MethodCall(meth.id, args)).setRandomPos

      val lowered = ExtensionCallLowerer()(methodCall)
      lowered shouldBe NormalAccess(
        ClassID("ext$AExtension"),
        MethodCall(MethodID("ExtensionMethod"), args))
      lowered.getPos shouldBe methodCall.getPos
    }
  }

  it should "lower extension declarations" in {
    // extension AExtension : A =
    //
    //   Def M1(a: Int) =
    //     this.OtherMethod() + a + this.i
    //
    //   Def static StaticMethod(a: Int) = a + 1
    // ---------------------------
    // class ext$Extension =
    //
    //   Def static Method($this: A, arg1: A, arg2: Int) =
    //     $this.OtherMethod() + arg1.i + $this.i + arg2.M()
    //
    //   Def static StaticMethod(a: Int) = a + 1

    val aClass = createClass("A")
    val m1Stat =
      Plus(
        Plus(
          NormalAccess(This(), MethodCall(MethodID("OtherMethod"), Nil)),
          createVariable("a")
        ),
        NormalAccess(This(), VariableID("i").setSymbol(new FieldSymbol("i", Set(Public()), aClass.getSymbol)))
      )
    val aExtensionClass = createExtensionClass(
      "AExtension",
      aClass,
      methods = List(
        createMethod(
          "M1",
          args = List(Types.Int),
          stat = Some(m1Stat)),
        createMethod(
          "M2",
          args = List(Types.Int),
          modifiers = Set(Public(), Static()),
          stat = Some(Plus(createVariable("a"), createIntLit(1)))
        )
      )
    )

    val lowered = ExtensionDeclarationLowerer()(aExtensionClass)
    lowered shouldBe ClassDecl(
      "ext$AExtension",
      methods = List(
        MethodDecl(
          "M1",
          modifiers = Set(Public(), Static()),
          args = List(Formal("A", "$this"), Formal(IntType, "a")),
          retType = Some(IntType),
          stat = Some(
            Plus(
              Plus(
                NormalAccess(VariableID("$this"), MethodCall(MethodID("OtherMethod"), Nil)),
                VariableID("a").setSymbol(new VariableSymbol("a"))
              ),
              NormalAccess(VariableID("$this"), VariableID("i").setSymbol(new FieldSymbol("i", Set(Public()), aClass.getSymbol)))
            )
          )
        ),
        MethodDecl(
          "M2",
          modifiers = Set(Public(), Static()),
          args = List(Formal(IntType, "a")),
          retType = Some(IntType),
          stat = Some(Plus(VariableID("a"), IntLit(1)))
        )
      )
    )
    lowered.getPos shouldBe aExtensionClass.getPos
  }
}
