val x = new A()
val xasd = new A()
x.Blest()
xasd.Blest(5) // Not the same types, this should not give a suggestion
x.Blest("5").Rest()
println(x.bield)
test()

class A =

	val field = 0
	val field2 = test()
	Def Test() = println("Test")
	Def Test(s: String) = println(s)

class B =

	Def Test() =
		test("hej" + "hej" +
		     "Hej" + "hej")
