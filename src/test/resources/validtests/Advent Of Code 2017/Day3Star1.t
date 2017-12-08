// ignore
// Can't be used as a test since the solution was found manually
import java::lang::Math

val input = 277678

// 37 36  35  34  33  32 31
// 38 17  16  15  14  13 30
// 39 18   5   4   3  12 29
// 40 19   6   1   2  11 28
// 41 20   7   8   9  10 27
// 42 21  22  23  24  25 26
// 43 44  45  46  47  48 49
//
// 10
// 22
//
// 1 -> 1
// 3 -> 8
// 5 -> 16
// 7
// 9
//
//
// 1
// 8
// 16
//
// (x * 2) + ((x - 2) * 2)
//
// . . . . . . . . . . . . . . . . . . .
// . . . . . . . . . . . . . . . . . . .
// . . . . . . . . . . . . . . . . . . .
// . . . . . . . . . . . . . . . . . . .
// . . . . . . . . . . . . . . . . . . .
// . . . . . . . . . . . . .57 . . . . .
// . . . . . . . . . . . .31 . . . . . .
// . . . . . . . . . . .13 . . . . . . .
// . . . . . . . . . . 3 . . . . . . . .
// . . . . . . . . . 1 . . . . . . . . .
// . . . . . . . . . . . . . . . . . . .
// . . . . . . . . . . . . . . . . . . .
// . . . . . . . . . . . . . . . . . . .
// . . . . . . . . . . . . . . . . . . .
// . . . . . . . . . . . . . . . . . . .
// . . . . . . . . . . . . . . . . . . .
// . . . . . . . . . . . . . . . . . . .
// . . . . . . . . . . . . . . . . . . .
// . . . . . . . . . . . . . . . . . . 27729

def circumference(w: Int) = (w * 2) + ((w - 2) * 2)

var circ = 1
var w = 1
while(circ < input)
	w += 2
	circ += circumference(w)


val halfWidth = (w - 1) / 2

println("halfWidth:" + halfWidth)
println("circumference(w): " + circumference(w))
println("circ: " + circ)
println("input: " + input)
println("diff: " + (input - circ))
println("w: " + w)

// MATH IS HARD
// Last width is 527, and 2779 is 51 steps to the left
// Correct answer is 475!

