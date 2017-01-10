val i: Int? = 5
println(i!!)

val j: Int = 5
println(j!!) // res: T2035

val a: A? = new A()
println(a!!.Test())

val b: A = new A()
println(b!!.Test()) // res: T2035

PrintA(a!!)
PrintA(b!!) // res: T2035

Def PrintA(a: A) = println("An A!")

class A {

    Def Test() = "A"
}