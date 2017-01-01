var i: Int = new Int(1)
println(i) // res: 1

i = new Int('a')
println(i) // res: 97

i = new Int()
println(i) // res: 0

var d = new Double(2)
println(d) // res: 2.0
d = new Double(2.0f)
println(d) // res: 2.0
d = new Double()
println(d) // res: 0.0


var a1 = new A<Int>()
println(a1) // res: 0

var a2 = new A<String>()
println("test" + a2.toString()) // res: test

var a3 = new A<Float>()
println(a3) // res: 0.0



class A<T> {

    var t: T

    Def new() = (t = new T())

    Def toString() = return "" + t

}