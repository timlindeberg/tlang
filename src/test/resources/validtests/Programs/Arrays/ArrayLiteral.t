var intArray = [ 1 + 1, 2, 3 ]
println(intArray.Size()) // res: 3
println(intArray[0]) // res: 2
println(intArray[1]) // res: 2
println(intArray[2]) // res: 3

var stringArray  = [ "1", Test(), "3" ]
println(stringArray[0]) // res: 1
println(stringArray[1]) // res: Test
println(stringArray[2]) // res: 3

Test2([ 1, 8, 6, 9, 3 ])

var m: Int[][][] = [ [ [ 1, 2 ], [ 3, 4 ] ], [ [ 5, 6 ], [ 7, 8 ] ] ]

println(m.Size()) // res: 2
println(m[0].Size()) // res: 2
println(m[0][0].Size()) // res: 2

for(var i = 0; i < m.Size(); i++)
	for(var j = 0; j < m[0].Size(); j++)
		for(var k = 0; k < m[0][0].Size(); k++)
			println(m[i][j][k]) // res: 1, 2, 3, 4, 5, 6, 7, 8


Def Test() = return "Test"

Def Test2(arr: Int[]) =
	for(var i = 0; i < arr.Size(); i++)
		println(arr[i]) // res: 1, 8, 6, 9, 3
