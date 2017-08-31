trait A =

	Def Test1(): String
	Def Test2(): String
	Def Test3(): String = "Test"

trait B : A =

	Def Test1() = "Test1"
	Def Test4(): String

class C : A = // res: T2025

	Def Test(): Int = 1

class D : A =

	Def Test1() = "Test1"
	Def Test2() = "Test2"

class E : B // res: T2025
