/*---------------------------------------------------------------------------*/
/*                       Advent of Code 2017 - Day 10                        */
/*---------------------------------------------------------------------------*/

import java::lang::StringBuilder

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

Def CreateLengths(input: String, extra: Int[]) =
	val E = extra.Size()
	val I = input.Size()
	val lengths = new Int[I + E]
	for(var i = 0; i < I; i++)
		lengths[i] = input[i]

	for(var i = I; i < I + E; i++)
		lengths[i] = extra[i - I]

	lengths

Def CreateNumberArray(size: Int) =
	val x = new Int[size]
	for(var i = 0; i < size; i++)
		x[i] = i
	x

Def Shuffle(x: Int[], lengths: Int[], times: Int) =
	var pos = 0
	var skip = 0
	for(var i = 0; i < times; i++)
		for(val length in lengths)
			if(length > x.Size())
				continue

			Reverse(x, pos, length)
			pos = (pos + length + skip) % x.Size()
			skip++

Def DenseHash(x: Int[]) =
	val dense = new Int[16]
	for(var i = 0; i < 16; i++)
		for(var j = 0; j < 16; j++)
			dense[i] ^= x[16 * i + j]

	dense

Def HashCode(lengths: Int[]) =
	val x = CreateNumberArray(256)
	Shuffle(x, lengths, 64)

	val sb = new StringBuilder()

	for(val hash in DenseHash(x))
		val x = hash.ToHexString()
		if(x.Size() == 1)
			sb.append("0")
		sb.append(x)

	sb.toString()

val input = "94,84,0,79,2,27,81,1,123,93,218,23,103,255,254,243"
val extra = [17, 31, 73, 47, 23]


val lengths = CreateLengths(input, extra)

println(HashCode(lengths)) // Res: 541dc3180fd4b72881e39cf925a50253
