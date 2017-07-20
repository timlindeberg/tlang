println(Return(9)) // res: -1
println(Return(4)) // res: 6

println(Break(2)) // res: 111
println(Break(4)) // res: 1

println(Continue(0)) // res: 11111
println(Continue(8)) // res: 11111111

Def Return(v: Int) =
	val x = GetValue(v)
	val y = x ?: return -1

	return y + 5

Def Break(v: Int) =
	for(var i = 0; i < 5; i++)
		val x = GetValue(v + i)
		val y = x ?: break
		print(x)
	println()

Def Continue(v: Int) =
	for(var i = 0; i < 10; i++)
		val x = GetValue(v + i)
		val y = x ?: continue
		print(x)
	println()


Def GetValue(i: Int): Int? = if(i < 5 || i > 10) 1 else null