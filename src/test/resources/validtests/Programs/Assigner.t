var f: Foo
var t: Test
var a: A
var c: C
f = new Foo()
println(f.bar(10000))   // res: 5
println(f.baz(1))       // res: 1
println(f.foo())        // res: foo
t = new Test()
println(t.Method())     // res: String
t = new Test4()
println(t.Method())     // res: Test4

a = new A()
println(a.foo(0, "yo")) // res: 0
a = new B()
println(a.foo(0, "yo")) // res: 1
c = new C()
println(c.foo())        // res: 0
c = new D()
println(c.foo())        // res: 1

class Foo =
	Def bar(a: Int): Int = 5

	Def baz(a: Int): Int = a

	Def foo(): String = "foo"

class Test =
	Def Method (): String = "String"

class Test2 : Test
class Test3 : Test2
class Test4 : Test3 =
	Def Method () : String = "Test4"

class A =
	Def foo(a: Int, b: String): Int = 0

class B : A =
	Def foo(a: Int, b: String): Int = 1

class C =
	Def foo():Int = 0

class D : C =
	Def foo(): Int = 1

