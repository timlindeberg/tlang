var i: Int? = GetNullableInt1(0)

if(i != null)
	println("not null")
else
	PrintNullable(i)
	println("null") // res: null

i = GetNullableInt2(5)

if(i != null)
	println(i + 1) // res: 6
	PrintNullable(i)
	PrintInt(i)

var j = GetNullableInt3(4)

println(j ?: 0) // res: 0

j = GetNullableInt3(5)

println(j ?: 0) // res: 5

val A: A? = new A()

i = A?.GetInt()
println(i!!) // res: 5

i = A?.GetB()?.GetInt()
println(i!!) // res: 6

// TODO: Fix nullable methods
i = A?.GetC()?.GetInt()
// println(i) // res: null

i = A?.GetB()?.GetC()?.GetInt()
println(i) // res: 5

i = A?.GetC()?.GetInt() ?: -1
println(i) // res: -1

i = A?.GetB()?.GetC()?.GetInt() ?: -1
println(i) // res: 5

// This should be inferred as Int?
Def GetNullableInt1(i: Int) =
	if(i < 5)
		return null
	else
		return i

// This should be inferred as Int?
Def GetNullableInt2(i: Int) = i < 5 ? null : i

Def GetNullableInt3(i: Int): Int? = i < 5 ? null : i

Def GetNullableA(i: Int) = i < 5 ? null : new A()

Def PrintNullable(int: Int?) = println(int ?: -1) // res: -1, 5
Def PrintInt(int: Int) = println(int) // res: 5

class A =

	Def GetInt() = 5
	Def GetB(): B? = new B()
	Def GetC(): B? = null

class B =

	Def GetInt() = 6
	Def GetC(): C? = new C()

class C =

	Def GetInt() = 5
