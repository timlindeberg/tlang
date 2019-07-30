class AnnotationA : Annotation
class AnnotationB : Annotation =
	Val S: String

class AnnotationC : Annotation =
	Val A: Int
	Val B: Double
	Val C: String

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
	Def MyMethod(): String = "ABC"

	@AnnotationA Def MyMethod2(): Int = 5

	@AnnotationA @AnnotationB("ABC") Def +(e: E, i: Int) = i

	@AnnotationA
	@AnnotationB("ABC")
	Val MyValue = 25
