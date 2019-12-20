import T::std::Vector
import T::std::Vec2
import T::std::Comparator
import java::lang::Math

val input = `
#.#.###.#.#....#..##.#....
.....#..#..#..#.#..#.....#
.##.##.##.##.##..#...#...#
#.#...#.#####...###.#.#.#.
.#####.###.#.#.####.#####.
#.#.#.##.#.##...####.#.##.
##....###..#.#..#..#..###.
..##....#.#...##.#.#...###
#.....#.#######..##.##.#..
#.###.#..###.#.#..##.....#
##.#.#.##.#......#####..##
#..##.#.##..###.##.###..##
#..#.###...#.#...#..#.##.#
.#..#.#....###.#.#..##.#.#
#.##.#####..###...#.###.##
#...##..#..##.##.#.##..###
#.#.###.###.....####.##..#
######....#.##....###.#..#
..##.#.####.....###..##.#.
#..#..#...#.####..######..
#####.##...#.#....#....#.#
.#####.##.#.#####..##.#...
#..##..##.#.##.##.####..##
.##..####..#..####.#######
#.#..#.##.#.######....##..
.#.##.##.####......#.##.##
`.Trim()

val center = new Vec2<Double>(13, 17)

def AlmostEqual(a: Double, b: Double) = Math.abs(a - b) < 0.0000000001

Def IsBlocked(p1: Vec2<Double>, p2: Vec2<Double>, p3: Vec2<Double>) =
	if (p3 == p1 || p3 == p2 || p1.Distance(p3) >= p1.Distance(p2))
		return false

	val v1 = p2 - p1
	val v2 = p3 - p1
	val dot = v1.Normalized().Dot(v2.Normalized())
	AlmostEqual(dot, 1.0)

Def GetAsteroids(input: String) =
	val asteroids = new Vector<Vec2<Double>>()
	val lines = input.Lines()
	for (var y = 0; y < lines.Size(); y++)
		for (var x = 0; x < lines[0].Size(); x++)
			if (lines[y][x] == '#')
				val asteroid = new Vec2<Double>(x, y)
				asteroids.Add(asteroid)
	asteroids

class ClockwiseSort: Comparator<Vec2<Double>> =

	var center: Vec2<Double>

	Def new(center: Vec2<Double>) =
		this.center = center

	Def Compare(a: Vec2<Double>, b: Vec2<Double>): Int = compare(a, b) ? 1 : -1

	def compare(a: Vec2<Double>, b: Vec2<Double>): Bool =
		if (a.X - center.X >= 0 && b.X - center.X < 0)
			return false
		if (a.X - center.X < 0 && b.X - center.X >= 0)
			return true
		if (a.X - center.X == 0 && b.X - center.X == 0)
			if (a.Y - center.Y >= 0 || b.Y - center.Y >= 0)
				return a.Y > b.Y
			return b.Y > a.Y

		// compute the cross product of vectors (center -> a) x (center -> b)
		val det = (a.X - center.X) * (b.Y - center.Y) - (b.X - center.X) * (a.Y - center.Y)
		if (det < 0)
			return true
		if (det > 0)
			return false

		// points a and b are on the same line from the center
		// check which point is closer to the center
		val d1 = (a.X - center.X) * (a.X - center.X) + (a.Y - center.Y) * (a.Y - center.Y)
		val d2 = (b.X - center.X) * (b.X - center.X) + (b.Y - center.Y) * (b.Y - center.Y)
		return d1 > d2

val asteroids = GetAsteroids(input)
asteroids.Sort(new ClockwiseSort(center))

var numRemoved = 0
while(asteroids.NonEmpty())
	var i = 0

	while(i < asteroids.Size())
		val v1 = asteroids.RemoveIndex(i)
		if (numRemoved == 200)
			println((v1.X * 100 + v1.Y) as Int) // res: 612
			return
		numRemoved++
		while(i < asteroids.Size() - 1 && IsBlocked(center, asteroids[i], v1))
			i++




