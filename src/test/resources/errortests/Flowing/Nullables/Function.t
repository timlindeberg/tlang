Def MoreTests(b: A?) =
	var a = b
	a.Test() // res: F2000
	if(a != null)
		a.Test()
	else
		a.Test() // res: F2001

	if(a.b) // res: F2000
		println()

	if(a != null && a.b)
		println()

	if(a == null)
		a.Test() // res: F2001
	else
		a.Test()

	if(a == new A() && a != new A() && a.b) // res: F2000
		a.Test() // res: F2000

	if(a == new A() && a != null && a.b)
		a.Test()

	while(a != null && a.b)
		a.Test()
		a = GetA()
		a.Test() // res: F2000

	a = null
	while(a == null)
		a.Test() // res: F2001
		a = new A()
		a.Test()

	a = GetA()

	for(var i = 0; a != null; i++)
		a.Test()
		a = GetA()
		a.Test() // res: F2000

	for(var i = 0; a == null; i++)
		a.Test() // res: F2001
		a = new A()
		a.Test()

	for(a = GetA(); a == b; a.Update(), a.Update())
		a.Test() // res: F2000
		a = new A()

	if(a) a.Test()
	if(!a) a.Test() // res: F2001


	val c = GetA()

	if(a != null && c != null)
		a.Test()
		c.Test()

	a = GetA()
	if(a != null && (a = GetA()) != null)
		a.Test()

	// Horrible edge case where the variable is reassigned
	// in the condition.
	if(a != null && (a = GetA()) == new A())
		a.Test() // res: F2000

	if(!(a == null || c == null))
		a.Test()
		c.Test()

	val i = 4
	val e = GetA()
	val f = GetA()
	if(!(a == null || c != null || (i == 5 && i != 4) || e == null || f == null))
		// De morgans: a != null && c == null && !(i == 5 && i != 4) && e != null && f != null
		a.Test()
		c.Test()  // res: F2001
		e.Test()
		f.Test()

/*------------------------------------------------------------------------*/

Def GetA(): A? = null

class A =

	Val b = true
	Def Test() = println("Test")

	Def Update() = println(5)
