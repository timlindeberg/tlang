var a: A? = new A()

a.Test()

a = null

a.Test() // res: F2001

a = GetA()
val b: A? = GetA()

GetA().Test() // res: F2002

a.Test() // res: F2000
println(a + b) // res: F2000, F2000

var bool = a && a.b
bool = !a || a.b

if(a != null && b != null)
	println(a + b)
	println(a < b)
	println(++a)
	println(a++)
	println(#a)
	println(a[1])
	println(a[1] = 5)

if(a != null || b != null)
	println(a + b) // res: F2000, F2000
	println(a < b) // res: F2000, F2000
	println(++a) // res: F2000
	println(a++) // res: F2000
	println(#a) // res: F2000
	println(a[1]) // res: F2000
	println(a[1] = 5) // res: F2000

if(a != null)
	println(a + b) // res: F2000

if(b != null)
	println(a + b) // res: F2000

var arr: Int[]? = [ 1, 2, 3 ]

println(arr[0])

arr = GetArr()

if(arr != null)
	println(arr[0])
println(arr[0]) // res: F2000

if(arr != null)
	for(val i in arr)
		println(i)

for(val i in arr) // res: F2000
	println(i)

/*------------------------------------------------------------------------*/

Def GetA(): A? = null
Def GetArr(): Int[]? = null

class A =

	Val b = true
	var i = 0

	Def Test() = println("Test")

	Def +(a: A, b: A) = 5
	Def <(a: A, b: A) = a.i < b.i
	Def ==(a: A, b: A) = a.b == b.b
	Def ++(a: A) = new A()
	Def #(a: A) = a.i
	Def [](index: Int) = i + index
	Def []=(index: Int, value: Int) = i = (index + value)
