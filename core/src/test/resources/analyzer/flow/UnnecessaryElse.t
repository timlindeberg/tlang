val a = 5
if(a == 5)
	println()
	return
else
	println() // res: F1002

if(a == 5)
	println()
	return
else if(a == 6) // res: F1002
	println()
	return
else
	println() // res: F1002

if(a == 5)
	if(a == 5)
		println()
		return
	else
		println() // res: F1002
		return
else
	println() // res: F1002

for(var i = 0; i < 5; i++)
	if(a == 5)
		continue
	else
		println("") // res: F1002

	if(a == 5)
		break
	else
		println("") // res: F1002

Def Test() =
	val a = 5
	if(a == 5)
		println(5)
		return
	else
		println(5) // res: F1002
		return
