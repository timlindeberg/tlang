val a = GetA()
val b = GetA()

val b1 = a && b

if(b1)
	a.Test()
	b.Test()

if(!b1)
	a.Test() // res: F2000
	b.Test() // res: F2000

if(b1 == true)
	a.Test()
	b.Test()

if(b1 == false)
	a.Test() // res: F2000
	b.Test() // res: F2000

val b2 = a != null && b == null


if(b2)
	a.Test()
	b.Test() // res: F2001

if(!b2)
	a.Test() // res: F2000
	b.Test() // res: F2000

if(b2 == true)
	a.Test()
	b.Test() // res: F2001

if(b2 == false)
	a.Test() // res: F2000
	b.Test() // res: F2000

val c = GetA()
val d = GetA()

val b3 = a && c
val b4 = b && d

if(b3 && b4)
	a.Test()
	b.Test()
	c.Test()
	d.Test()

/*------------------------------------------------------------------------*/

Def GetA(): A? = null

class A =
	Def Test() = println("Test")
