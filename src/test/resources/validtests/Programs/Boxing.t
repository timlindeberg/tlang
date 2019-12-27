Test(1)
Test(2)
Test("Hej")

val arr = [ 1, 1L, 1.0, 1.0F, 'c', new A() ]

for(val x in arr)
	     if(x is Int)    println("Int "    + (x as Int))    // res: Int 1
	else if(x is Long)   println("Long "   + (x as Long))   // res: Long 1
	else if(x is Double) println("Double " + (x as Double)) // res: Double 1.0
	else if(x is Float)  println("Float "  + (x as Float))  // res: Float 1.0
	else if(x is Char)   println("Char "   + (x as Char))   // res: Char c
	else if(x is A)      println("Object " + (x as A))      // res: Object A

Def Test(i: Object) =
	if(i is Int)
		val x = i as Int
		println("Int " + x) // res: Int 1, Int 2
	else if (i is String)
		val x = i as String
		println("String " + x) // res: String Hej
	else
		println("Does not happen")

class A =
	Def toString() = "A"
