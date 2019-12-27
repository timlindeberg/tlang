var i: Int
var j: Int
for(i = 0; i < 5; i++)
	println(i) // res: 0, 1, 2, 3, 4

for(i = 0, j = 5; i < 5 && j > 0; i++, j--)
	println(i + " " + j) // res: 0 5, 1 4, 2 3, 3 2, 4 1