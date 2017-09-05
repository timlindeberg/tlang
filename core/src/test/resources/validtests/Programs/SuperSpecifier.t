var c = new C()
println(c.Test())  // res: A
println(c.TestV()) // res: A

var d = new D()
println(d.Test())  // res: B
println(d.TestV()) // res: B

var f = new F()
println(f.Test())   // res: A
println(f.Test2())  // res: E
println(f.TestV())  // res: A
println(f.TestV1()) // res: E

trait A =

	Val static V = "A"
	Def Test() = "A"

trait B =

	Val static V = "B"
	Def Test() = "B"

class C: A, B =

	Def TestV() = super<A>.V
	Def Test() = super<A>.Test()

class D: A, B =

	Def TestV() = super<B>.V
	Def Test() = super<B>.Test()

trait E =

	Val static V1 = "E"
	Def Test2() = "E"

class F: A, E =

	Def Test() = super.Test()
	Def Test2() = super.Test2()
	Def TestV() = super.V
	Def TestV1() = super.V1
