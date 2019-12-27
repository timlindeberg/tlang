import java::util::Locale
import java::util::Random
import java::lang::System


new A().AD()
new A().BC()
new A().TimeZone()

val locale = new Locale("sv", "SE")
println(locale.getDisplayLanguage()) // res: Swedish

val ran = new Random(0)
println(ran.nextInt()) // res: -1155484576

val st = new java::util::IntSummaryStatistics()
st.accept(8)
println(st.getMin()) // res: 8

println(1 + java::lang::Math.PI + 1) // res: 5.141592653589793

class A : java::util::GregorianCalendar =

	Def AD() = println(AD) // res: 1
	Def BC() = println(BC) // res: 0

	// TODO: This test only works in CET on a computer with english Locale :)
	Def TimeZone() = println(getTimeZone().getDisplayName()) // res: Central European Time
