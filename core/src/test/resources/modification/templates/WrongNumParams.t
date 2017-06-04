var a1 = new A<Int>() //res: G2000
var a2 = new A<Int, Int>() //res: G2000
var a3 = new A<Int, Int, Int>() //res: G2000
var a4 = new A<Int, Int, Int, Int>() //res: G2000
var a5 = new A<Int, Int, Int, Int, Int, Int>() //res: G2000

class A<T1, T2, T3, T4, T5>
