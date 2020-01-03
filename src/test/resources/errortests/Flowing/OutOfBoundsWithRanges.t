val a = new Int[5]

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