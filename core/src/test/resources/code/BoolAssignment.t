var b: Bool
b = true
println(b) // res: true
b = false
println(b) // res: false
b = false || true
println(b) // res: true
b = true && false
println(b) // res: false
b = false && Test.Foo()
println(b) // res: false
b = true || Test.Foo()
println(b) // res: true

class Test =

	Def static Foo(): Bool =
		println("Never called")
		return true
