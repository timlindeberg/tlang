val C = new Container()

for(val v in C)
	println(v) // res: A, A, A, A, A, A, A, A, A, A

for(val v: Int in [ 1, 2, 3, 4, 5 ])
	println(v) // res: 1, 2, 3, 4, 5


val arr: Int[] = [ 1, 2, 3, 4, 5 ]

for(var v in arr)
	println(++v) // res: 2, 3, 4, 5, 6

for(val v in arr)
	println(v) // res: 1, 2, 3, 4, 5

class A =

	Def toString() = "A"

class Container =

	Def Iterator(): CIterator = new CIterator()

class CIterator =

	var count = 0

	Def HasNext() = count < 10
	Def Next(): A =
		count++
		new A()
