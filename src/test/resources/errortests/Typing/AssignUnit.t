class A =
	Def X() = println(5)

val t1 = new A().X() // res: T2010
t1 = new A().X() // res: T2010
