import java::lang::Math

class Day3 =

	Val W = 10
	Val GOAL = 277678

	Val RIGHT = 0
	Val UP    = 1
	Val LEFT  = 2
	Val DOWN  = 3
	val board: Int[][] = new Int[W][W]

	def outOfBounds(i: Int, j: Int) = i < 0 || j < 0 || i >= W || j >= W

	def value(i: Int, j: Int) = outOfBounds(i, j) ? 0 : board[i][j]

	def sumOfSurrounding(i: Int, j: Int) =
		value(i + 1, j    ) +
		value(i + 1, j + 1) +
		value(i    , j + 1) +
		value(i - 1, j + 1) +
		value(i - 1, j    ) +
		value(i - 1, j - 1) +
		value(i    , j - 1) +
		value(i + 1, j - 1)

	def printBoard() =
		println("----------------------------------------------------")
		for(val line in board)
			for(val v in line)
				print(v + " ")
			println()
		println("----------------------------------------------------")

	Def Calculate() =

		var steps = 0
		var length = 1

		var i = (W - 1) / 2
		var j = (W - 1) / 2

		var shouldIncreaseLength = false
		var dir = RIGHT

		while(!outOfBounds(i, j))
			val sum = Math.max(1, sumOfSurrounding(i, j))
			if(sum > GOAL)
				println("Sum: " + sum) // res: Sum: 279138
				break

			board[i][j] = sum

			     if(dir == RIGHT) j += 1
			else if(dir == UP)    i -= 1
			else if(dir == LEFT)  j -= 1
			else if(dir == DOWN)  i += 1

			steps += 1
			if(steps >= length)
				steps = 0
				if(shouldIncreaseLength)
					length += 1
				shouldIncreaseLength = !shouldIncreaseLength
				dir = (dir + 1) % 4

new Day3().Calculate()
