var a = new A()

a.test()
// Uses D:s version
println(a.implemented(0)) // res: 3

class A : B, C, D =

	Def method(i: Int) = return i + 1

	Def implemented(i: Int) = super<D>.implemented(i)

class B =

	Def test() = println("lol") // res: lol

trait C =

	Def method(i: Int): Int

	Def implemented(i: Int) = return method(i) + 1

trait D =

	Def method(i: Int): Int

	Def implemented(i: Int) = return method(i) + 2
