class A =

	val b1: B? = new B()
	val b2: B? = null
	val b3: B? = GetB(0)

	var b4: B? = GetB(0)

	Def Test() =
		b1.Test()
		b2.Test() // res: F2001
		b3.Test() // res: F2000

		if(b3)
			b3.Test()
			UpdateFields()
			b3.Test()

		b4.Test() // res: F2000
		if(b4)
			// b4 could change at any time since it's a variable field
			b4.Test() // res: F2000

		var b = b4
		if(b)
			// b however cannot change
			b.Test()

	Def UpdateFields() = println("Update")

	Def GetB(i: Int): B? = i < 5 ? new B() : null

class B =
	Def Test() = println("Test")
