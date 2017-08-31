class A =

	Val F = 1
	val f = 1

	Val static G = 1
	val static g = 1

	Def Test(b: B) =
		val x = new B()
		val y: B = new B()
		b = new B() // res: F2005
		x = new B() // res: F2005
		y = new B() // res: F2005
		F = 2 // res: F2005
		f = 2 // res: F2005
		G = 2 // res: F2005
		g = 2 // res: F2005
		x++ // res: F2005
		++x // res: F2005
		x-- // res: F2005
		--x // res: F2005
		F++ // res: F2005
		++F // res: F2005
		F-- // res: F2005
		--F // res: F2005

		for(val i in [ 1, 2, 3 ])
			i = 5 // res: F2005

		for(val i = 0; i < 5; i++); // res: F2005

		B.F = 2 // res: F2005

class B =
	Val static F = 1

	Def ++(b: B) = new B()
	Def --(b: B) = new B()
