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

Def CreateLengths(input: String) =
	val extra = [17, 31, 73, 47, 23]

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

Def KnotHash(s: String) =
	val lengths = CreateLengths(s)
	val x = CreateNumberArray(256)
	Shuffle(x, lengths, 64)

	val sb = new StringBuilder()

	for(val hash in DenseHash(x))
		val x = hash.ToHexString()
		if(x.Size() == 1)
			sb.append("0")
		sb.append(x)

	sb.toString()

Def PrintGrid(grid: Int[][]) =
	for(var i = 0; i < grid.Size(); i++)
		for(var j = 0; j < grid[0].Size(); j++)
			print(grid[i][j] == 0 ? '.' : '#')
		println()

Def MakeGrid(input: String) =
	val grid = new Int[128][128]
	for(var i = 0; i < 128; i++)
		val hash = KnotHash(input + "-" + i)
		for(var j = 0; j < hash.Size(); j++)
			val x = Int.Parse("" + hash[j], 16)
			for(var k = 0; k < 4; k++)
				grid[i][4 * j + 3 - k] = x[k]
	grid

class Visitor =

	var graph: Int[][]
	val visited = new Bool[128 * 128]

	Def ShouldVisit(i: Int, j: Int) = (i >= 0 && i < 128 && j >= 0 && j < 128) && !visited[128 * i + j]

	Def new(graph: Int[][]) =
		this.graph = graph

	Def DFS(graph: Int[][], i: Int, j: Int): Bool =
		if(i < 0 || i >= 128 || j < 0 || j >= 128)
			return false

		if(visited[128 * i + j])
			return false

		if(graph[i][j] == 0)
			return false

		visited[128 * i + j] = true

		DFS(graph, i + 1, j)
		DFS(graph, i - 1, j)
		DFS(graph, i, j + 1)
		DFS(graph, i, j - 1)
		true

	Def NumRegions(): Int =
		var count = 0
		for(var i = 0; i < 128; i++)
			for(var j = 0; j < 128; j++)
				if(DFS(graph, i, j)) count++

		count

val input = "jxqlasbh"
val visitor = new Visitor(MakeGrid(input))

println("Count: " + visitor.NumRegions()) // Res: Count: 1182



