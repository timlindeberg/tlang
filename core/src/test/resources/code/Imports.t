import java::util::Locale
import java::util::Random
import java::lang::System
import T::lang::extension java::lang::Object
import T::lang::extension java::lang::String

// TODO: This test only works in CET on a computer with english Locale :)

new A().AD() // res: 1
new A().BC() // res: 0
new A().TimeZone() // res: Central European Time

val locale = new Locale("sv", "SE")
println(locale.getDisplayLanguage()) // res: Swedish

val ran = new Random(0)
println(ran.nextInt()) // res: -1155484576

Def Lol() =
	val x = 5 +
			5

	println("HEJ!" +
	        "hej" +
	        "hej!"
	        )

	val x = 10
		val x = 5
		println(x)
	println(x)

	for(val x in 0..10)
		println(x)
		println(x + 5)
		if(x > 10)
			println(10 * x)
			println(100 * x)

	println("END")


val st = new java::util::IntSummaryStatistics()
st.accept(8)
println(st.getMin()) // res: 8

println(1 + java::lang::Math.PI + 1) // res: 5.141592653589793

class A : java::util::GregorianCalendar =

	Def AD() = println(AD)
	Def BC() = println(BC)

	Def TimeZone() = println(getTimeZone().getDisplayName())

class B: java::util::GregorianCalendar =

	Def Löööl()

	class C: A =
		Def Bröööl()
