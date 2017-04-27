import java::util::*
import java::math::*

val a = { 5, 4, 3, 2, 1 }
Arrays.sort(a)
println(Arrays.toString(a)) // res: [1, 2, 3, 4, 5]

val bitSet = new BitSet(10)
println(bitSet.size()) // res: 64

val date = new Date(0)
println(date) // res: Thu Jan 01 01:00:00 CET 1970

val bigInt = new BigInteger("1234567890")
println(bigInt) // res: 1234567890

val bigDecimal = new BigDecimal("0.0000000000000000000000000000000000001")
println(bigDecimal) // res: 1E-37
