var a = new Int[5]
println(a[-1]) // res: F2004

println(a[5]) // res: F2004

a = [ 1, 2, 3, 4, 5 ]

var i = 5
var j = 5
println(a[i]) // res: F2004
println(a[i - 1])
println(a[i - j])
println(a[i - j - 1]) // res: F2004

a = GetArr()

println(a[-1]) // res: F2004
println(a[0])
println(a[5])

var k = ((i + j) * (10 + (-i)) ) << 1
a = new Int[k]

println(a[100]) // res: F2004

k = (((~i & (j + 1)) ^ 8) >> (j - 4)) | (i % (j - 2))
a = new Int[k]

println(a[7]) // res: F2004

// Can't infer value when increment/decrement is used
k = (i++ + --j) * (i + j)
a = new Int[k]
println(a[90])

if(true)
	println("true")
	i = 2
	j = 1
else
	i = 2
	j = 5

a = new Int[i]
println(a[2])  // res: F2004

a = new Int[j]
println(a[2])

Def GetArr() = return [ 1, 2, 3, 4, 5 ]