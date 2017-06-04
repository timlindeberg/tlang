val a = GetA()
// This should in theory not give an error
// but seems like a very strange case. It would only apply when
// we know the result of the ternary check is true or false
if(a == null ? true : false)
	a.Test() // res: F2000

val b1 = a == null ? 1 : a.GetInt()
val b2 = a != null ? a.GetInt() : 1
val b3 = a == null ? a.GetInt() : 1 // res: F2001
val b4 = a != null ? 1 : a.GetInt() // res: F2001

val b5 = !a ? 1 : a.GetInt()
val b6 = a  ? a.GetInt() : 1
val b7 = !a ? a.GetInt() : 1 // res: F2001
val b8 = a  ? 1 : a.GetInt() // res: F2001

/*------------------------------------------------------------------------*/

Def GetA(): A? = null

class A =

	Def GetInt() = 5
	Def Test() = println("Test")
