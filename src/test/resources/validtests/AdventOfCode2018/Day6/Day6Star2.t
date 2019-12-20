// Ignore
import T::std::Vector
import java::lang::Math
import T::std::HashMap

val input =`77, 279
216, 187
72, 301
183, 82
57, 170
46, 335
55, 89
71, 114
313, 358
82, 88
78, 136
339, 314
156, 281
260, 288
125, 249
150, 130
210, 271
190, 258
73, 287
187, 332
283, 353
66, 158
108, 97
237, 278
243, 160
61, 52
353, 107
260, 184
234, 321
181, 270
104, 84
290, 109
193, 342
43, 294
134, 211
50, 129
92, 112
309, 130
291, 170
89, 204
186, 177
286, 302
188, 145
40, 52
254, 292
270, 287
238, 216
299, 184
141, 264
117, 129`

val testInput = `1, 1
1, 6
8, 3
3, 4
5, 5
8, 9`

class Point =
	Var X: Int
	Var Y: Int

	Def new(x: Int, y: Int) =
		X = x
		Y = y

	Def ==(a: Point, b: Point) = (a.X == b.X && a.Y == b.Y)

	Def #(a: Point) = 31 * a.X ^ a.Y

	Def toString() = "(" + X + ", " + Y + ")"

class Day6 =

	val MAX_DISTANCE = 10_000
	var grid: Bool[][]
	var checked: Bool[][]
	val coords = new Vector<Point>()
	val names = new HashMap<Point, String>()

	Def new(input: String) =
		var c = 'a'
		var maxX = 0
		var maxY = 0
		for(val line in input.Lines())
			val s = line.Split(", ")
			val coord = new Point(s[0].ToInt(), s[1].ToInt())
			names[coord] = "" + c++
			coords.Add(coord)
			maxX = Math.max(maxX, coord.X)
			maxY = Math.max(maxY, coord.Y)
		grid = new Bool[maxY + MAX_DISTANCE * 2][maxX + MAX_DISTANCE * 2]
		checked = new Bool[grid.Size()][grid[0].Size()]

	Def Run() =
		CalculateDistances()
		var maxArea = 0
		for(var y = 0; y < grid.Size(); y++)
			for(var x = 0; x < grid[0].Size(); x++)
				if(checked[y][x])
					continue
				maxArea = Math.max(maxArea, GetArea(new Point(x, y)))
		println(maxArea)

	Def GetArea(p: Point): Int =
		val queue = new Vector<Point>()
		queue.Add(p)
		var area = 0
		while(!queue.IsEmpty())
			val p = queue.Pop()
			val x = p.X
			val y = p.Y
			if(checked[y][x]) continue

			checked[y][x] = true
			if(!grid[y][x]) continue

			area += 1
			if(Valid(x + 1, y)) queue.Add(new Point(x + 1, y))
			if(Valid(x - 1, y)) queue.Add(new Point(x - 1, y))
			if(Valid(x, y + 1)) queue.Add(new Point(x, y + 1))
			if(Valid(x, y - 1)) queue.Add(new Point(x, y - 1))

		area

	Def Valid(x: Int, y: Int) =
		y >= 0 && y < checked.Size() &&
		x >= 0 && x < checked[0].Size() &&
		!checked[y][x]

	Def CalculateDistances() =
		for(var y = 0; y < grid.Size(); y++)
			for(var x = 0; x < grid[0].Size(); x++)
				grid[y][x] = GetTotalDistance(x - MAX_DISTANCE, y - MAX_DISTANCE) < MAX_DISTANCE

	Def Distance(p1: Point, p2: Point): Int = Math.abs((p1.X - p2.X)) + Math.abs((p1.Y - p2.Y))

	Def GetTotalDistance(x: Int, y: Int) =
		var totalDistance = 0
		for(val c in coords)
			val distance = Math.abs(c.X - x) + Math.abs(c.Y - y)
			totalDistance += distance
		totalDistance

new Day6(input).Run() // res: 42998