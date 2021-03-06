var a = new A()
println(a.Test()) // res: test
println(a.Test2()) // res: 10
println(a.Test3()) // res: 5
println(a.Test4(6)) // res: 25
println(a.Test4(5)) // res: 10
println(a.Test5(6)) // res: 25
println(a) // res: 0
a.Test6(6)
println(a) // res: 6
println(a.Test7()) // res: test
println(a.Test8()) // res: test

class A =

	var i: Int = 0

	Def Test() = "test"

	Def Test2() =
		var x = 5
		x + 5

	Def Test3() =
		var y = 0
			 var x = 5
			 x + 5 // useless statement warning
		5

	Def Test4(x: Int) = (x == 5 ? 10 : 25)
	Def Test5(x: Int) =
		x == 5 ? 10 : 25

	Def Test6(x: Int) = Assign(x)

	Def Test7() = Test()
	Def Test8() =
		Test()

	Def Assign(x: Int) = (i = x)

	Def toString() = "" + i