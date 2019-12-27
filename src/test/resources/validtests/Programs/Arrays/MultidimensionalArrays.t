var intArray: Int[][][][] = new Int[1][2][3][4]
val h: H = new H()

println(intArray.Size()) // res: 1
println(intArray[0].Size()) // res: 2
println(intArray[0][0].Size()) // res: 3
println(intArray[0][0][0].Size()) // res: 4

for(var i = 0; i < intArray.Size(); i++)
	for(var j = 0; j < intArray[0].Size(); j++)
		for(var k = 0; k < intArray[0][0].Size(); k++)
			for(var l = 0; l < intArray[0][0][0].Size(); l++)
				intArray[i][j][k][l] = i + j + k + l

for(var i = 0; i < intArray.Size(); i++)
	for(var j = 0; j < intArray[0].Size(); j++)
		for(var k = 0; k < intArray[0][0].Size(); k++)
			for(var l = 0; l < intArray[0][0][0].Size(); l++)
				println(intArray[i][j][k][l]) // res: 0, 1, 2, 3, 1, 2, 3, 4, 2, 3, 4, 5, 1, 2, 3, 4, 2, 3, 4, 5, 3, 4, 5, 6

intArray[0][0][0] = [ 1, 2, 3 ]
for(var i = 0; i < intArray[0][0][0].Size(); i++)
	println(intArray[0][0][0][i]) // res: 1, 2, 3

intArray[0][0] = [ [1, 2], [3, 4] ]
for(var i = 0; i < intArray[0][0].Size(); i++)
	for(var j = 0; j < intArray[0][0][0].Size(); j++)
		println(intArray[0][0][i][j]) // res: 1, 2, 3, 4

val longArray: Long[][] = [[1, 2], [3, 4]]
longArray[1] = [1, 2]
for(var i = 0; i < longArray.Size(); i++)
	for(var j = 0; j < longArray[0].Size(); j++)
		println(longArray[i][j]) // res: 1, 2, 1, 2

intArray = new Int[2][3][4][5]
println(intArray.Size()) // res: 2
println(intArray[0].Size()) // res: 3
println(intArray[0][0].Size()) // res: 4
println(intArray[0][0][0].Size()) // res: 5

intArray[0] = new Int[4][5][6]
println(intArray.Size()) // res: 2
println(intArray[0].Size()) // res: 4
println(intArray[0][0].Size()) // res: 5
println(intArray[0][0][0].Size()) // res: 6

intArray[0][0] = new Int[6][7]
println(intArray.Size()) // res: 2
println(intArray[0].Size()) // res: 4
println(intArray[0][0].Size()) // res: 6
println(intArray[0][0][0].Size()) // res: 7

intArray[0][0][0] = new Int[8]
println(intArray.Size()) // res: 2
println(intArray[0].Size()) // res: 4
println(intArray[0][0].Size()) // res: 6
println(intArray[0][0][0].Size()) // res: 8

println(H.GetArray().Size()) // res: 2
println(H.GetArray()[0].Size()) // res: 2

println(H.GetArray()[0][0]) // res: 0
println(H.GetArray()[0][1]) // res: 1
println(H.GetArray()[1][0]) // res: 2
println(H.GetArray()[1][1]) // res: 3

println(H.GetArray()[0][0] = 1) // res: 1

println(h.GetFieldArray()[0][0] = 5) // res: 5
println(h.ArrField[0][0]) // res: 5


class H =

	Var ArrField: Int[][] = new Int[2][2]

	Def static GetArray(): Int[][] =
		val i: Int[][] = new Int[2][2]
		i[0][0] = 0 ; i[0][1] = 1
		i[1][0] = 2 ; i[1][1] = 3
		return i

	Def GetFieldArray(): Int[][] = return ArrField
