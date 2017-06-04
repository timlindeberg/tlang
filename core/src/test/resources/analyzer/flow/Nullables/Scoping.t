val a = GetA()
val b = GetA()
val c = GetA()
val d = GetA()
val e = GetA()

if(a)
	a.Test()
	if(b)
		a.Test()
		b.Test()
		if(c)
			a.Test()
			b.Test()
			c.Test()
			if(e)
				a.Test()
				b.Test()
				c.Test()
				e.Test()
			else
				a.Test()
				b.Test()
				c.Test()
				e.Test() // res: F2001
		else
			a.Test()
			b.Test()
			c.Test() // res: F2001
			e.Test() // res: F2000
	else
		a.Test()
		b.Test() // res: F2001
		c.Test() // res: F2000
		e.Test() // res: F2000
else
	a.Test() // res: F2001
	b.Test() // res: F2000
	c.Test() // res: F2000
	e.Test() // res: F2000

/*------------------------------------------------------------------------*/

Def GetA(): A? = null

class A =
	Def Test() = println("Test")
