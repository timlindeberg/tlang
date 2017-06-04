val C = new Container()

for(val v in C)
	print(v) // res: AAAAAAAAAA
println()

for(val v: Int in {1, 2, 3, 4, 5})
	print(v) // res: 12345
println()


val arr: Int[] = {1, 2, 3, 4, 5}

for(var v in arr)
	print(++v) // res: 23456
println()

for(val v in arr)
	print(v) // res: 12345
println()

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
