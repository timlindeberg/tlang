annotation AnnotationA
annotation AnnotationB =
	Def S(): String


@AnnotationA
annotation AnnotationC =
	Def A(): Int
	Def B(): Double
	Def C(): String

@AnnotationA
class A

@AnnotationB(S = "ABC")
trait B

@AnnotationB(S = "DEF") @AnnotationA class C

@AnnotationC(
    A = 5,
    B = 5.0,
    C = "GHI",
) extension A

@AnnotationA
@AnnotationB(S = "JKL") class E =

	@AnnotationA @AnnotationB(S = "ABC")
	Def new() = ;

	@AnnotationA
	@AnnotationB(S = "ABC")
	@AnnotationC(B = 5.0, C = "GHI", A = 5)
	Def MyMethod(): String = "ABC"

	@AnnotationA Def MyMethod2(): Int = 5

	@AnnotationA @AnnotationB(S = "ABC") Def +(e: E, i: Int) = i

	@AnnotationA
	@AnnotationB(S = "ABC")
	Val MyValue = 25



val e = new A()
val c = e.getClass()
val annotations = c.getAnnotations()
// This test should probably use runtime visible annotations but for that
// we need enums in annotations (and enums in the language) so for now
// it's just empty
println(annotations) // res: []
