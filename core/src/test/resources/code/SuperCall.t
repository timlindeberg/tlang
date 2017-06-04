new B().Test()

class A =

	Var i = 1

	Def A() = "A"

class B : A =

	Var i = 2
	Def A() = "B"

	Def Test() =
		println(i) // res: 2
		println(super.i) // res: 1
		println(A()) // res: B
		println(super.A()) // res: A
