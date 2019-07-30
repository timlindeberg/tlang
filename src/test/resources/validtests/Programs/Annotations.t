class AnnotationA : Annotation
class AnnotationB : Annotation =
	Var S: String

	Def new(s: String) = (S = s)

class AnnotationC : Annotation =
	Var A: Int
	Var B: Double
	Var C: String

	Def new(a: Int, b: Double, c: String) =
		A = a
		B = b
		C = c


@AnnotationA
class A

@AnnotationB("ABC")
class B

@AnnotationB("DEF") @AnnotationA class C

@AnnotationC(5, 5.0, "GHI") class D

@AnnotationA
@AnnotationB("JKL") class E =

	@AnnotationA @AnnotationB("ABC")
	Def new() = ;

	@AnnotationA
	@AnnotationB("ABC")
	@AnnotationC(5, 5.0, "GHI")
	Def MyMethod(): String = "ABC"

	@AnnotationA Def MyMethod2(): Int = 5

	@AnnotationA @AnnotationB("ABC") Def +(e: E, i: Int) = i

	@AnnotationA
	@AnnotationB("ABC")
	Val MyValue = 25
