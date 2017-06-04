var a = new A("Test")
var b = new B("Test")

println(a + "Test") // res: TestTestTest
println(b + "Test") // res: TestTest

class A =

	var x: String

	Def new(s: String) = (x = s)

	Def +(a: A, s: String) = return new A(a.x + s + s)

	Def toString() = return x

class B =

	var x: String

	Def new(s: String) = (x = s)

	Def toString() = return x
