val i: Int? = GetInt()

Test(i)
Test(GetInt())
Test2(i) // res:  F2000
Test2(GetInt()) // res:  F2002

if(i)
	Test(i)
	Test2(i)

val a: A? = GetA()

Test(a) // res:  F2000
Test(GetA()) // res:  F2002

if(a)
	Test(a)

Def GetInt(): Int? = null
Def GetA(): A? = null

Def Test(i: Int?) = println((i ?: 0) + 1)
Def Test2(i: Int) = println(i + 1)

Def Test(a: A) = a.Test()

class A =
	Def Test() = println("A")
