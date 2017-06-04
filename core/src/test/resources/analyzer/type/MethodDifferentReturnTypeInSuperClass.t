class A =
	Def Test(): Int = 1

trait B =
	Def Test(): Int = 1

class C : A =
	Def Test() = " Hej" // res: T2027

class D : B =
	Def Test() = 1.0f // res: T2027
