var a1: A = new A()
var a2: A = new A()
var b: B = new B()

a1.Test(a2)
b.Test2(a2)
println(a1.J = 1)      // res: 1
println(a1.J += a1.J)  // res: 2

class A =

	Var J: Int
	var i: Int
	var protected k: Int

	Def Test(a: A): Unit =
		println(a.i = 1)    // res: 1
		println(a.i += a.i) // res: 2

class B : A =

	Def Test2(a: A): Unit =
		println(a.k = 1)    // res: 1
		println(a.k += a.k) // res: 2
