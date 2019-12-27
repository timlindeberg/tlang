val a = new A()
val b = new A()

a.StaticTest()
A.StaticTest()

extension AExtension : A =

	Def static StaticTest() = println("Static A") // res: Static A, Static A

class A =

	Var i = 1

	Def GetInt() = 5

	Def -(a: A) = new A()
