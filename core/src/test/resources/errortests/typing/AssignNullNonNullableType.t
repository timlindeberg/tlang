var a: A? = new A()

a = null

var b: A = new A()

b = null // res: T2000

var c = new A()

c = null // res: T2000

var i: Int? = 1

i = null

var j = 1

j = null // res: T2000

var k: Int = 5

k = null // res: T2000

Test1(null)
Test2(null) // res: T2001

Def Test1(i: Int?) =
	println(i)

Def Test2(i: Int) =
	println(i)

class A
