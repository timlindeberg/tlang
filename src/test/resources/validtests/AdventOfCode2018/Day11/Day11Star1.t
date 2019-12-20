import T::std::Vector
import T::std::HashMap
import T::std::HashSet
import java::lang::Math
import java::util::regex::Matcher
import java::util::regex::Pattern


class Point =
	Var X: Int
	Var Y: Int

	Def new(x: Int, y: Int) =
		X = x
		Y = y

	Def ==(a: Point, b: Point) = (a.X == b.X && a.Y == b.Y)
	Def +(a: Point, b: Point) = new Point(a.X + b.X, a.Y + b.Y)

	Def #(a: Point) = 31 * a.X ^ a.Y

	Def toString() = "(" + X + ", " + Y + ")"

class Day11 =

	val SerialNumber = 9445
	val GridSize = 300

	val grid = new Int[GridSize][GridSize]

	Def Run() =
		for(var y = 0; y < GridSize; y++)
			for(var x = 0; x < GridSize; x++)
				grid[y][x] = GridPower(x, y)

		var max = 0
		var maxX = 0
		var maxY = 0

		for(var y = GridSize - 2; y >= 1; y--)
			for(var x = GridSize - 2; x >= 1; x--)
				val power = SquarePower(x, y)
				if(power >= max)
					max = power
					maxX = x - 1
					maxY = y - 1

		println(maxX + "," + maxY)

	Def SquarePower(x: Int, y: Int) =
		var power = 0
		for(var j = 0; j < 3; j++)
			for(var i = 0; i < 3; i++)
				power += grid[y - 1 + j][x - 1 + i]
		power

	Def GridPower(x: Int, y: Int) =
		val rackId = x + 10
		var power = rackId * y
		power += SerialNumber
		power *= rackId
		HundrethDigit(power) - 5

	Def HundrethDigit(i: Int) =
		val s = i.toString()
		s[s.Size() - 3] - '0'

new Day11().Run() // res: 233,36
