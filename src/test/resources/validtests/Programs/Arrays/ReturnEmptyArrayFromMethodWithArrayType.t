Def X(i: Int): Int[] =
	if(i == 3) return [1, 2, 3]
	if(i == 2) return [1, 2]
	if(i == 1) return [1]
	return []

println(X(3)) // res: [1, 2, 3]
println(X(2)) // res: [1, 2]
println(X(1)) // res: [1]
println(X(0)) // res: []