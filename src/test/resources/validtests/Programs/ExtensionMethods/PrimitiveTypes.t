val x = 5
println(5 + new A()) // res: 10
x[3] = 2 // res: Int setter
println(Int.GetValue()) // res: 1
println(x.subtract(5)) // res: 0
println(5.subtract(10)) // res: -5

extension IntExtensions : T::lang::Int =

	Def subtract(x: Int) = this - x

	Def static GetValue() = 1

	Def +(a: Int, rhs: A): Int = a + rhs.i

	Def []=(index: Int, value: Int): Unit = println("Int setter")


class A =
	Val i: Int = 5
