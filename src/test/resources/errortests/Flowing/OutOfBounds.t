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


a = new Int['q' - 'a']
println(a['q'])  // res: F2004

a = [1, 2, 3]
i = 5
while(true)
	println(a[i]) // res: F2004

for(;;)
	if (j > 0)
		i = GetInt()
	println(a[i])

// Could have been reassigned in the loop
while(true)
	println(a[i])

i = 5
while(true)
	println(a[i]) // res: F2004

while(true)
	for(val x in [1, 2, 3])
		if(x > GetInt())
			i = GetInt()
	println(a[i])

for(i = -1; i < GetInt(); i++)
	println(a[i]) // res: F2004

for(i = -1; i < GetInt(); i++)
	if(i > 5)
		println(a[i])

val x = 50
a = new Int[5]

for(var i = 0; i < GetInt(); i++)
	val number = -x + i
	val v = number >= 0 && number <= 5 && a[number] == '#'
	println(v)

for(var i = 0; i < GetInt(); i++)
	val number = -x + i
	val b = number < 0 || number > 25
	val v = !b && a[number] == '#'
	println(v)

a = new Int[5]

for(var i = -1; i < 5; i++)
	println(a[i]) // res: F2004

	if(i > 0)
		println(a[i])

	if(i > -5)
		println(a[i]) // res: F2004

	if(i < 5)
		println(a[i]) // res: F2004

	if(i < 5 && i >= 1)
		println(a[i])

	println(i >= 0 && a[i] == 5)


Def GetInt() = 25

Def GetArr() = return [ 1, 2, 3, 4, 5 ]

class A =
	var x: Int = 5

	Def Test() =
		val a = [1, 2, 3]
		println(a[x])
