class MyClass

annotation A =
	Def new() = ; // res: N2026

	Def static A(): String = "ABC" // res: N2025, N2028
	Def +(a: A, i: Int) = 5 // res: N2027

	Def B(): String = "ABC" // res: N2028
	Def C(): MyClass // res: N2029
	def E(): Int // res: N2030
