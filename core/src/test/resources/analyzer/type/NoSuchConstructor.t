var a1 = new A<Int, Int>()
var a2 = new A<A<Int, String>, A<Int, String>>("hej") // res: T2016
var a3 = new A<Int, Int>("hej") // res: T2016
var a4 = new A<Int, Int>(new A<Int, Int>()) // res: T2016

class A<T, K>
