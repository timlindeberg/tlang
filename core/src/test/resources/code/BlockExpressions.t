val a =
	val y = 2
	val z = 5
	y * z + 25

println(a) // res: 35

println( // res: 25
	val y = 5
	val z = 5
	y * z
)

val b = 1 +
	val x = 5
	x * 5

println(b) // res: 26


val c = GetValue(6) ?:
	val y = 5
	val z = 6
	y + z

println(c) // res: 11

val d =
	val y = 5
	val z = 6
	y + z
+ 5

println(d) //  res: 16?

println(X(0)) // res: null-1
println(X(6)) // res: 6

Def GetValue(v: Int): Int? = if(v > 5) 1 else null

Def X(v: Int) =
	val x = GetValue(v)
	val y = x ?:
		print("null")
		return -1
	return y + 5
