val a = new A()
val b = new A()

println(a + b) // res: 5
println(#a) // res: 10
a[5] = 5
println(a[5]) // res: 15

extension A {

    Def +(lhs: A, rhs: A) = 5
    Def #(a: A) = (-a).GetInt() + 5
    Def [](index: Int) = i + index
    Def []=(index: Int, value: Int) = (i = index + value)

}

class A {

    Var i = 1

    Def GetInt() = 5

    Def -(a: A) = new A()
}