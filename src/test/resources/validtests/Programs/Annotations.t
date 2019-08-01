trait AnnotationA : Annotation
trait AnnotationB : Annotation =
	Def S(): String

trait AnnotationC : Annotation =
	Def A(): Int
	Def B(): Double
	Def C(): String

@AnnotationA
class A

@AnnotationB(S = "ABC")
class B

@AnnotationB(S = "DEF") @AnnotationA class C

@AnnotationC(
    A = 5,
    B = 5.0,
    C = "GHI",
) class D

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
