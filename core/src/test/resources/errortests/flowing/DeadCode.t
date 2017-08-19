val a = 1
if(a == 5)
	val b = 5
	return
	println(b) // res: F1000
	println(b)
	println(b)

for(var i = 0; i < 5; i++)
	println(a)
	if(true)
		break
		println( /**/ a) // res: F1000
		println(a)

	if(false)
		continue
		println(a) // res: F1000

if(a == 5)
	error("ERROR")
	println(a) // res: F1000
	println(a)
	println(a)
	println(a)
	println(a)
	println(a)

Def BothPaths1() =
	val a = 5
	if(a == 5)
		return
		println(a) // res: F1000
	else
		return // res: F1002
		println(a) // res: F1000

	println(a) // res: F1000
	println(a)
	println(a)

Def BothPaths2() =
	val a = 5
	if(a == 5)
		if(a == 4)
			return
			println(a)  // res: F1000
		else
			return // res: F1002
			println(a)  // res: F1000
	else
		if(a == 4) // res: F1002
			return
			println(a)  // res: F1000
		else
			return // res: F1002
			println(a)  // res: F1000
	println(a) // res: F1000
