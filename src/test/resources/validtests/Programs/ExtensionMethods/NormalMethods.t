val a = new A()
val b = new A()

a.Test() // res: A
a.Test(5) // res: 5
val x = a.Test2()
println(x) // res: 6

extension AExtension : A =

	Def Test(i: Int) = println(i)
	Def Test() = println("A")
	Def Test2() = GetInt() + this.i

class A =

	Var i = 1

	Def GetInt() = 5

	Def -(a: A) = new A()
