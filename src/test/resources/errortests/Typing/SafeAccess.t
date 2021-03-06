var x: Int? = GetNullable()?.X
var s: String? = GetNullable()?.Test()
var y: Int? = GetNullable()?.GetB()?.Y
var s2: String? = GetNullable()?.GetB()?.Test()

val a = new A()

x = a?.X // res: T2034
s = a?.Test() // res: T2034
x = a.GetB()?.Y // res: T2034
s = a.GetB()?.Test() // res: T2034

Def GetNullable() = true ? null : new A()

class A =
	Val X: Int = 5

	Def GetB() = new B()
	Def Test() = "A"

class B =

	Val Y: Int = 6
	Def Test() =  "B"
