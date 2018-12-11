val a1 = new A<Int, Int>()
val a2 = new A<A<Int, String>, A<Int, String>>("hej") // res: T2016
val a3 = new A<Int, Int>("hej") // res: T2016
val a4 = new A<Int, Int>(new A<Int, Int>()) // res: T2016

val b1 = new B(5)
val b2 = new B() // res: T2016

val c1 = new C()
val c2 = new C("ABC") // res: T2016

val d1 = new D(5)
val d2 = new D("ABC") // res: T2016
val d3 = new D() // res: T2016

class A<T, K>

class B =
	var X: Int
	Def new(i: Int) = X = i

class C: Parent
class D: Parent =
	Def new(i: Int) = println(i)

class Parent =
	var X: String
	Def new(x: String) = X = x