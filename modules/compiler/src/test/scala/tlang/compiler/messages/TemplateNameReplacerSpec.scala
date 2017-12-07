package tlang.compiler.messages

import tlang.testutils.UnitSpec

class TemplateNameReplacerSpec extends UnitSpec {

  it should "replace template names" in {
    TemplateNameReplacer("-A$T-") shouldBe "A<T>"
    TemplateNameReplacer("-A$T1$T2$T3-") shouldBe "A<T1, T2, T3>"
    TemplateNameReplacer("This is class -A$T1$T2$T3-, very cool") shouldBe "This is class A<T1, T2, T3>, very cool"
    TemplateNameReplacer("A::B::-C$T1$T2-") shouldBe "A::B::C<T1, T2>"
    TemplateNameReplacer("-A$-B$T1$T2--") shouldBe "A<B<T1, T2>>"
    TemplateNameReplacer("-A$-B$T1$-C$T2$T3--$T4$-D$T5$T6-$T7-") shouldBe "A<B<T1, C<T2, T3>>, T4, D<T5, T6>, T7>"
  }

}
