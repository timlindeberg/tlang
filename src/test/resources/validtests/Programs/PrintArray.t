class A =
	Def toString() = "ABC"

val a = [1, 2, 3]
println(a) // res: [1, 2, 3]

val b = [new A(), new A(), new A()]
println(b) // res: [ABC, ABC, ABC]

val c = [[1, 2], [3, 4], [5, 6]]
println(c) // res: [[1, 2], [3, 4], [5, 6]]
