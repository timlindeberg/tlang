var impl: Implicit = 1
println(impl) // res: 1
impl = 5

println(impl) // res: 5
var i = 10

impl = i
println(impl) // res: 10
PrintImplicit(10)
PrintImplicit(GetInt())

var implArray: Implicit[] = [ 1, 2, 3 ]

implArray[1] = 5

for(var i = 0; i < 3; i++)
	println(implArray[i].GetValue()) // res: 1, 5, 3

PrintImplicitArray([ 1, 2, 3 ])

impl = 10 as Implicit
println(impl) // res: 10


var arr = [ 1, 2, 3 ]

var implArr: ImplicitArr = arr

println(implArr) // res: "1 2 3 "

println(new A().impl) //res: 5
println(A.staticImpl) //res: 10

Def GetInt() = return 5
Def PrintImplicit(impl: Implicit): Unit = println(impl) // res: 10, 5
Def PrintImplicitArray(implArray: Implicit[]) =
	for(var i = 0; i < implArray.Size(); i++)
		println(implArray[i]) // res: 1, 2, 3

class Implicit =

	var x: Int

	Def implicit new(i: Int) = x = i
	Def GetValue()           = return x
	Def toString()           = return "" + x

class ImplicitArr =

	var impl: Int[]

	Def implicit new(arr: Int[]) = this.impl = arr
	Def toString() =
		var str = ""
		for(var i = 0; i < impl.Size(); i++)
			str += impl[i] + " "
		return str

class A =
	Val impl: Implicit = 5
	Val static staticImpl: Implicit = 10
