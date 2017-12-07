var i: Int
if(i == 0)
	println(i)

var fl: Float
if(fl == 0.0)
	println(fl)

var a: A
a.Test() // res: F2006

a = new A()

a.Test()


var b: A
if(true)
	b = new A()
else
	b = new A()


b.Test()


var c: A

if(true)
	c = new A()
else
	println()

c.Test() // res: F2006


var d: A

if(true)
	println()
else
	d = new A()

d.Test() // res: F2006


var e: A

if(true)
	println()
else
	e = new A()

e.Test() // res: F2006


var f: A

while(true)
	f = new A()

f.Test() // res: F2006


var g: A

for(var i = 0; i < 5; i++)
	g = new A()

g.Test() // res: F2006

for(val x in [ new A(), new A(), new A() ])
	x.Test()

class A =
	Def Test() = println()
