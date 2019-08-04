trait AnnotationB

@AnnotationB(a = 1 + 1, b = MyFunc()) // res: N2024, N2024
class A =

	@AnnotationB(a = 'a'.ToUpper()) // res: N2024
	def B() = 1

Def MyFunc() = "ABC"
