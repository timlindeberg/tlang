var a: A? = new A(5)
println(a) // res: 5

println(a + 5) // res: 10

a.i = 1
println(a + 5) // res: 6

a = null

println(a + 5) // res: 5

var i: Int? = 2
println(a + i) // res: 2

i = null
println(a + i) // res: 0

println(a - i) // res: null

a = new A(10)
println(a + i) // res: 10

println(a - i) // res: 10
println(a - 5) // res: 5

class A =

	Var i: Int = 0

	Def new(i: Int) = (this.i = i)

	Def +(a: A?, i: Int?): A =
		val v1 = a?.i ?: 0
		val v2 = i ?: 0
		new A(v1 + v2)

	Def -(a: A?, i: Int?): A? =
		if(!a)
			return null
		new A(a.i - (i ?: 0))

	Def toString() = "" + i