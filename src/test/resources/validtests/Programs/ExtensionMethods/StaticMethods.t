val a = new A()
val b = new A()

a.StaticTest() // res: Static A
A.StaticTest() // res: Static A

extension AExtension : A =

	Def static StaticTest() = println("Static A")

class A =

	Var i = 1

	Def GetInt() = 5

	Def -(a: A) = new A()
