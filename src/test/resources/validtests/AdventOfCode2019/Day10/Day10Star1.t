import T::std::Vector
import T::std::Vec2
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

Def AlmostEqual(a: Double, b: Double) = Math.abs(a - b) < 0.0000000001

Def IsBlocked(p1: Vec2<Double>, p2: Vec2<Double>, p3: Vec2<Double>) =
	if (p3 == p1 || p3 == p2 || p1.Distance(p3) >= p1.Distance(p2))
		return false

	val v1 = p2 - p1
	val v2 = p3 - p1
	val dot = v1.Normalized().Dot(v2.Normalized())
	AlmostEqual(dot, 1.0)

Def IsBlocked(asteroids: Vector<Vec2<Double> >, p1: Vec2<Double>, p2: Vec2<Double>) =
	for (val p3 in asteroids)
		if (IsBlocked(p1, p2, p3))
			return true
	return false

Def NumVisible(asteroids: Vector<Vec2<Double> >, p1: Vec2<Double>) =
	var numVisible = 0
	for (val p2 in asteroids)
		if (p2 != p1 && !IsBlocked(asteroids, p1, p2))
			numVisible++

	return numVisible

Def GetAsteroids(input: String) =
	val asteroids = new Vector<Vec2<Double>>()
	val lines = input.Lines()
	for (var y = 0; y < lines.Size(); y++)
		for (var x = 0; x < lines[0].Size(); x++)
			if (lines[y][x] == '#')
				val asteroid = new Vec2<Double>(x, y)
				asteroids.Add(asteroid)
	asteroids

val asteroids = GetAsteroids(input)
var maxVisible = 0
for (val asteroid in asteroids)
	val visible = NumVisible(asteroids, asteroid)
	if (visible > maxVisible)
		maxVisible = visible

println(maxVisible) // res: 269