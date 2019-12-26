class A =

	Val X: Int = 5
	Val Y: Int

	Def new() =
		Y = 6

class B =

	Val X: Int = 5
	Val Y: Int

	Def new() =
		X = 5 // res: F2005
		Y = 6

class C =

	Val X: Int
	Val Y: Int

	Def new() =
		X = 5
		Y = 6

class D =

	Val X: Int
	Val Y: Int

	Def new() =
		X = 5
		Y = 6
		X = 5 // res: F2005
		Y = 6 // res: F2005

class E =

	Val X: Int
	Val Y: Int

	Def new() = // res: F2008
		X = 5

class F =

	Val X: Int
	Val Y: Int

	Def new() = ; // res: F2008, F2008


class G =

	Val X: Int
	Val Y: Int

	Def new() = // res: F2008
		X = 5

	Def new(y: Int) = // res: F2008
		Y = y

class H =

	Val X: Int
	Val Y: Int

	Def new() = // res: F2008, F2008
		init()

	def init() =
		X = 5 // res: F2005
		Y = 5 // res: F2005

class I = // res: F2008, F2008

	Val X: Int
	Val Y: Int

class J =

	Val X: Int
	Val Y: Int

	Def new() = // res: F2008, F2008
		if(true)
			X = 5
		else
			Y = 6

class K =

	Val X: Int
	Val Y: Int

	Def new() = // res: F2008
		if(true)
			X = 5
			Y = 5
		else
			Y = 6

class L =

	Val X: Int
	Val Y: Int

	Def new() = // res: F2008
		if(true)
			X = 5
		else
			Y = 6
		X = 10 // res: F2005

class M =

	Val X: Int
	Val Y: Int
	Val Z: Int

	Def new() =
		if(true)
			if(false)
				X = 5
			else
				Y = 7
		else
			Z = 6
		X = 10 // res: F2005
		Y = 11 // res: F2005
		Z = 12 // res: F2005


class N = // res: F2008

	Var X: Int
	Val Y: Int

class O =

	Var X: Int
	Val Y: Int

	def new() =
		Y = 5


class P =

	Val X: Int
	Val Y: Int

	def new() =
		this.X = 5
		this.Y = 5

class Q =

	Val X: Int
	Val Y: Int
	Val Z: Int

	def new() = (X = Y = Z = 5)