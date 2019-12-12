var a1 = new A() // res: T2009
var a2 = new A(1) // res: T2009
var a = new A("")

a.A() // res: T2009
a.B() // res: T2009
a.C()

A.D() // res: T2009
A.E() // res: T2009
A.F()

print(a.x) // res: T2009
print(a.y) // res: T2009
print(a.z)

print(a1 + a2) // res: T2009
print(a1 - a2) // res: T2009
print(a1 * a2)

print(a[1]) // res: T2009
a[1] = 1 // res: T2009, T2009

class A =

	var x: Int = 1
	var protected y: Int = 1
	Var z: Int = 1

	def new() = ;
	def protected new(i: Int) = ;
	Def new(s: String) = ;

	def A() = return 1
	def protected B() = return 1
	Def C() = return 1

	def static D() = return 1
	def protected static E() = return 1
	Def static F() = return 1

	def +(lhs: A, rhs: A) = return 1
	def protected -(lhs: A, rhs: A) = return 1
	Def *(lhs: A, rhs: A) = return 1

	def protected [](index: Int) = return 1
	def []=(index: Int, value: Int) = ;

class B : A =

	Def G() =
		var a: A = new A(1)
		print(a.B())
		print(A.E())
		print(a[1])
		a[1] = 5 // res: T2009
