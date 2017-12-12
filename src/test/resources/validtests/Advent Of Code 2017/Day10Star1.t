Def Mod(a: Int, b: Int) = (a % b + b) % b

Def Swap(x: Int[], a: Int, b: Int) =
	val temp = x[a]
	x[a] = x[b]
	x[b] = temp

Def Reverse(x: Int[], start: Int, length: Int) =
	val N = x.Size()
	for(var i = 0; i < length / 2; i++)
		val a = (start + i) % N
		val b = Mod(start + length - i - 1, N)
		Swap(x, a, b)


val input = [94,84,0,79,2,27,81,1,123,93,218,23,103,255,254,243]

val N = 256

val x = new Int[N]
for(var i = 0; i < N; i++)
	x[i] = i

var pos = 0
var skip = 0
for(val length in input)
	if(length > N)
		continue

	Reverse(x, pos, length)
	pos = (pos + length + skip) % N
	skip++

println(x[0] * x[1]) // Res: 23715