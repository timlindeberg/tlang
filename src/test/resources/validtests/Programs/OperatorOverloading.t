var a: A = new A(1)
var b: A = new A(2)

println(a + b) // res: 3
println(a - b) // res: -1
println(a * b) // res: 2
println(a / b) // res: 0
println(a % b) // res: 1

println(a & b) // res: 0
println(a | b) // res: 3
println(a ^ b) // res: 3

println(a << 1) // res: 2
println(a >> 1) // res: 0

println(a < b) // res: true
println(a <= b) // res: true
println(a > b) // res: false
println(a >= b) // res: false

println(a == b) // res: false
println(a != b) // res: true

println(a++) // res: 2
println(a) // res: 2
println(++a) // res: 3
println(a) // res: 3

println(a--) // res: 2
println(a) // res: 2
println(--a) // res: 1
println(a) // res: 1

println(~a) // res: -2
println(-a) // res: -1
println(#a) // res: 1

println(a[1]) // res: 2
println(a[1] = 2) // res: 2
println(a[::]) // res: 0
println(a[1::]) // res: 1
println(a[:2:]) // res: 2
println(a[::3]) // res: 3
println(a[:2:3]) // res: 5
println(a[1:2:3]) // res: 6

class A =

	Var I: Int

	Def new(i: Int) = I = i

	Def [](index: Int): Int = return I + index
	Def []=(index: Int, value: Int): Unit = I = index + value
	Def [:](start: Int?, end: Int?, step: Int?): Int = (start ?: 0) + (end ?: 0) + (step ?: 0)


	// Binary operators
	Def +(a: A, b: A): Int = return a.I + b.I
	Def -(a: A, b: A): Int = return a.I - b.I
	Def *(a: A, b: A): Int = return a.I * b.I
	Def /(a: A, b: A): Int = return a.I / b.I
	Def %(a: A, b: A): Int = return a.I % b.I

	// Logical operators
	Def &(a: A, b: A): Int = return a.I & b.I
	Def |(a: A, b: A): Int = return a.I | b.I
	Def ^(a: A, b: A): Int = return a.I ^ b.I

	// Shift operators
	Def <<(a: A, shift: Int): Int = return a.I << shift
	Def >>(a: A, shift: Int): Int = return a.I >> shift

	// Comparisons
	Def <(a: A, b: A):  Bool = return a.I <  b.I
	Def <=(a: A, b: A): Bool = return a.I <= b.I
	Def >(a: A, b: A):  Bool = return a.I >  b.I
	Def >=(a: A, b: A): Bool = return a.I >= b.I

	// Equality
	Def ==(a: A, b: A): Bool = return a.I == b.I
	Def !=(a: A, b: A): Bool = return a.I != b.I

	 // Unary
	Def ~(a: A): Int = return ~a.I
	Def -(a: A): Int = return -a.I
	Def #(a: A): Int = return a.I

	Def ++(a: A): A =
		a.I += 1
		return a

	Def --(a: A): A =
		a.I -= 1
		return a

	Def toString(): String = return "" + I
