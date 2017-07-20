val x = if(5 + 5 > 9) "x" else "y"
val y = if(5 + 5 > 10) "x" else "y"
println(x) // res: x
println(y) // res: y

println(if(x + y == "xy") "a" else "b") // res: a

val z = if(x != "x")
	val a = 1
	val b = 2
	a + b
else
	val a = 3
	val b = 4
	a + b

println(z) // res: 7

println( // res: 3
if(x == "x")
	val a = 1
	val b = 2
	a + b
else
	val a = 3
	val b = 4
	a + b
)



val a: Object = if(true)
	1
else
	"2"

println(a) // res: 1

val b = if(1 > 3)
	1
else if(2 > 3)
	2
else if(3 > 3)
	3
else
	4

println(b) // res: 4