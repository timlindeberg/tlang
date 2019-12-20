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

val MAX_SEARCH = 700

class Point =
	Var X: Int
	Var Y: Int

	Def new(x: Int, y: Int) =
		X = x
		Y = y

	Def ==(a: Point, b: Point) = (a.X == b.X && a.Y == b.Y)

	Def #(a: Point) = 31 * a.X ^ a.Y

	Def toString() = "(" + X + ", " + Y + ")"

Def Distance(p1: Point, p2: Point): Int = Math.abs((p1.X - p2.X)) + Math.abs((p1.Y - p2.Y))

Def ClosestCoord(closestCoords: HashMap<Point, Point>, coords: Vector<Point>, p: Point) =
	var closest = closestCoords.Get(p)
	if(closest)
		return closest
	var minDistance = Int.MaxValue()
	var numMin = 0
	for(val coord in coords)
		val dist = Distance(coord, p)
		if(dist == minDistance)
			numMin++
		if(dist < minDistance)
			closest = coord
			minDistance = dist
			numMin = 0

	if(numMin > 0)
		return null
	closestCoords[p] = closest
	closest

val closest = new HashMap<Point, Point>()
val coords = new Vector<Point>()
val names = new HashMap<Point, String>()
var c = 'a'
for(val line in input.Lines())
	val s = line.Split(", ")
	val coord = new Point(s[0].ToInt(), s[1].ToInt())
	names[coord] = "" + c++
	coords.Add(coord)

val areas = new HashMap<Point, Int>()
for(val coord in coords)
	var area = 0
	for(var y = -MAX_SEARCH; y < MAX_SEARCH; y++)
		for(var x = -MAX_SEARCH; x < MAX_SEARCH; x++)
			val p = new Point(coord.X + x, coord.Y + y)
			val closest = ClosestCoord(closest, coords, p)

			if(closest && closest == coord)
				area++
	areas[coord] = area

for(val e in areas)
	println(names[e.Key()] + ": " + e)

// res: 3006