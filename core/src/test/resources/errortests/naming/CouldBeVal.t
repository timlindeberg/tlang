class A =

	Def Test() =
		val y = 10
		var x = 5 // res: N1004

		for(var i in [ 1, 2, 3] ) // res: N1004
			println(i)

		for(var i = 0; i < 5;) // res: N1004
			println(i)

		return x + y
