/*---------------------------------------------------------------------------*/
/*                       Advent of Code 2017 - Day 13                        */
/*---------------------------------------------------------------------------*/

import T::std::Vector
import T::std::LinkedList

val input = `0: 3
1: 2
2: 4
4: 6
6: 4
8: 6
10: 5
12: 8
14: 8
16: 6
18: 8
20: 6
22: 10
24: 8
26: 12
28: 12
30: 8
32: 12
34: 8
36: 14
38: 12
40: 18
42: 12
44: 12
46: 9
48: 14
50: 18
52: 10
54: 14
56: 12
58: 12
60: 14
64: 14
68: 12
70: 17
72: 14
74: 12
76: 14
78: 14
82: 14
84: 14
94: 14
96: 14`


class Layer =
	Var Depth: Int

	Def new(depth: Int) = (Depth = depth - 1)

	Def toString() = "" + Depth

	Def Position(time: Int) =
		if(Depth <= 0)
			return 0

		val t = time % (2 * Depth)
		if(t <= Depth)
			return t
		return Depth - (t - Depth)


val lines = input.split("\r?\n")

val numLayers = lines[lines.Size() - 1].Split(":")[0].ToInt() + 1

val layers = new Layer?[numLayers]
for(val line in lines)
	val s = line.Split(":")
	layers[s[0].ToInt()] = new Layer(s[1].Trim().ToInt())

for(var i = 0; i < numLayers; i++)
	if(!layers[i])
		layers[i] = new Layer(0)

var severity = 0
for(var i = 0; i < numLayers; i++)
	val layer = layers[i]!!
	if(layer.Position(i) == 0)
		severity += i * (layer.Depth + 1)

println(severity) // res: 1876
