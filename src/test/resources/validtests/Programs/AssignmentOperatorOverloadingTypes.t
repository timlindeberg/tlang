class A =
	Def []=(a: Int, b: Long): Unit = ;
	Def [](a: Int): Int = 5

val a = new A()

val x = (a[0] = 12345678910111213L)
println(x) // res: 12345678910111213