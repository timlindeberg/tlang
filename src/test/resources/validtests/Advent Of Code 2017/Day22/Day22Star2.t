val input = `..######.###...######...#
.##..##.#....#..##.#....#
.##.#....###..##.###.#.#.
#.#.###.#####.###.##.##.#
.###.#.#.###.####..##.###
..####.##..#.#.#####...##
....##.###..#.#..#...####
.#.##.##.#..##...##.###..
.######..#..#.#####....##
###.##.###.########...###
.#.#.#..#.##.#..###...#..
.#.##.#.####.#.#.....###.
##..###.###..##...#.##.##
##.#.##..#...##...#...###
##..#..###.#..##.#.#.#.#.
.##.#####..##....#.#.#..#
..#.######.##...#..#.##..
#.##...#.#....###.#.##.#.
.#..#.#.#..#.####..#.####
.##...##....##..#.#.###..
..##.#.#.##..##.#.#....#.
###.###.######.#.########
..#.####.#.#.##..####...#
#.##..#.#.####...#..#..##
###.###.#..##..#.###....#`

val _testInput = `..#
#..
...`

Def PrintPattern(x: Int, y: Int, pattern: Char[][]) =
	val size = pattern.Size()
	println("-------")
	for(var i = 0; i < size; i++)
		print(" ")
		for(var j = 0; j < size; j++)
			if(i == y && j == x)
				print("[" + pattern[i][j] + "]")
			else
				print(" " + pattern[i][j] + " ")
		println()

Def Mod(a: Int, b: Int) =
	var ret = a % b
	ret < 0 ? (ret + b) : ret


/* -------------------------------------------------------------------------- */


val lines = input.Split("\r?\n")

val MAX_SIZE = 501
val CLEAN = '.'
val WEAKENED = 'W'
val INFECTED = '#'
val FLAGGED = 'F'

val grid = new Char[MAX_SIZE][MAX_SIZE]

for(var i = 0; i < MAX_SIZE; i++)
	for(var j = 0; j < MAX_SIZE; j++)
		grid[i][j] = '.'

for(var i = 0; i < lines.Size(); i++)
	for(var j = 0; j < lines[0].Size(); j++)
		val x = ((MAX_SIZE - lines.Size()) / 2) + i
		val y = ((MAX_SIZE - lines[0].Size()) / 2) + j
		grid[x][y] = lines[i][j]

var x = MAX_SIZE / 2
var y = MAX_SIZE / 2
var dir = 0

val directions = ["up", "right", "down", "left"]

var turned = 0
for(var i = 0; i < 10000000; i++)
	val current = grid[y][x]
	if(current == CLEAN)
		dir = Mod(dir - 1, directions.Size())
		grid[y][x] = WEAKENED
	else if(current == WEAKENED)
		grid[y][x] = INFECTED
		turned++
	else if(current == INFECTED)
		dir = Mod(dir + 1, directions.Size())
		grid[y][x] = FLAGGED
	else
		dir = Mod(dir + 2, directions.Size())
		grid[y][x] = CLEAN

	if(dir == 0)      y--
	else if(dir == 1) x++
	else if(dir == 2) y++
	else              x--

println(turned) // res: 2511702