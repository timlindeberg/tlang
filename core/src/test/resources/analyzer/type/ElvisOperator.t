val a: A = new A()
val b: A? = null

val c = a ?: new A() // res: T2036

val d: A = a ?: new A() // res: T2036

val e = b ?: new A()
val f: A = b ?: new A()

val g = b ?: 1 // res: T2000
val h = b ?: "hej" // res: T2000

class A