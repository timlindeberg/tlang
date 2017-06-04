var a = GetA()
var b = GetA()
a.Test() // res: F2000
if(a == null)
	return

a.Test()

a = GetA()

if(a != null)
	return

a.Test() // res: F2001

if(a == null || b == null) 
	a.Test() // res: F2001
	b.Test() // res: F2000
else
	a.Test() // res: F2001
	b.Test()

if(a == null || b == null)
	return

a.Test() // res: F2001
b.Test()

a = GetA()

if(a == null || a == b )
	a.Test() // res: F2000
	b.Test()
else
	a.Test()
	b.Test()

if(a == null)
	error("")

a.Test()

a = GetA()

for(var i = 0; i < 5; i++)
	if(a == null)
		continue

	a.Test()
	a = GetA()
	if(a != null)
		continue

	a.Test() // res: F2001

for(var i = 0; i < 5; i++)
	if(a == null)
		break

	a.Test()
	a = GetA()
	if(a != null)
		break

	a.Test() // res: F2001

var i = 0
a = GetA()
if(a != null)
	println()
	i = 1
else
	i = 1
	return

a.Test()

i = 0
a = GetA()
if(a == null)
	i = 1
	return
else
	println()
	i = 1

a.Test()
/*------------------------------------------------------------------------*/

Def GetA(): A? = null

class A =
	Def Test() = println("Test")
