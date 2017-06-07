var impl: Implicit = 1
println(impl) // res: 1
impl = 5

println(impl) // res: 5
var i = 10

impl = i
println(impl) // res: 10
PrintImplicit(10) // res: 10
PrintImplicit(GetInt()) // res: 5

var implArray: Implicit[] = [ 1, 2, 3 ]

implArray[1] = 5

for(var i = 0; i < 3; i++)
	print(implArray[i].GetValue() + " ") // res: 1 5 3
println("")


PrintImplicitArray([ 1, 2, 3 ]) // res: 1 2 3

impl = 10 as Implicit
println(impl) // res: 10


var arr = [ 1, 2, 3 ]

var implArr: ImplicitArr = arr

println(implArr) // res: 1 2 3

Def GetInt() = return 5
Def PrintImplicit(impl: Implicit): Unit = println(impl)
Def PrintImplicitArray(implArray: Implicit[]) =
	for(var i = 0; i < implArray.Size(); i++)
		print(implArray[i] + " ")
	println("")

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
