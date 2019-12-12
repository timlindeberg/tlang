var b: Bool   = true
var c: Char   = 'a'
var i: Int    = 2
var l: Long   = 2l
var f: Float  = 2f
var d: Double = 2.0
var s: String = "str"
var a: A      = new A()

println(b = true)    // res: true

// Char
println(c = 'a')     // res: a
println(c = 97)      // res: 97

// Int
println(i = 2)       // res: 2
println(i = 'a')     // res: a

// Long
println(l = 2l)      // res: 2
println(l = 2)       // res: 2
println(l = 'a')     // res: a

// Float
println(f = 2.0f)    // res: 2.0
println(f = 2)       // res: 2
println(f = 2l)      // res: 2
println(f = 'a')     // res: a

// Double
println(d = 2.0)     // res: 2.0
println(d = 2.0f)    // res: 2.0
println(d = 2)       // res: 2
println(d = 2l)      // res: 2
println(d = 'a')     // res: a

// String
println(s = "str")   // res: str

// String
println(a = new A()) // res: A

class A =
	Def toString(): String = return "A";
