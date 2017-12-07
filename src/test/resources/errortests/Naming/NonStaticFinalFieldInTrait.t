trait A =

	val static A: Int = 1

	var a = 1 // res: N2022
	Var b = 1 // res: N2022
	val c = 1 // res: N2022
	Val d = 1 // res: N2022
	var static e = 1 // res: N2022
	Var static f = 1 // res: N2022
