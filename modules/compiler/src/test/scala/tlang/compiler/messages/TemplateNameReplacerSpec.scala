package tlang
package compiler
package messages

import tlang.testutils.UnitSpec

class TemplateNameReplacerSpec extends UnitSpec {

  it should "replace template names" in {
    replace("-A$T-") shouldBe "A<T>"
    replace("-A$T1$T2$T3-") shouldBe "A<T1, T2, T3>"
    replace("This is class -A$T1$T2$T3-, very cool") shouldBe "This is class A<T1, T2, T3>, very cool"
    replace("A::B::-C$T1$T2-") shouldBe "A::B::C<T1, T2>"
    replace("-A$-B$T1$T2--") shouldBe "A<B<T1, T2>>"
    replace("-A$-B$T1$-C$T2$T3--$T4$-D$T5$T6-$T7-") shouldBe "A<B<T1, C<T2, T3>>, T4, D<T5, T6>, T7>"
    replace("-A$-B$T1$-C$T2$T3--$T4$-D$T5$T6-$T7-") shouldBe "A<B<T1, C<T2, T3>>, T4, D<T5, T6>, T7>"
    replace("TryTraverse(-Vec2$Int-, -A$-B$T1$T2--, Object).") shouldBe "TryTraverse(Vec2<Int>, A<B<T1, T2>>, Object)."
  }

  private def replace(s: String): String = TemplateNameReplacer(s)
}
