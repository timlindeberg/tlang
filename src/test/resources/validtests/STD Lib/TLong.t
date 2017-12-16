println(Long.MaxValue()) // res: 9223372036854775807
println(Long.MinValue()) // res: -9223372036854775808
println(5L.Size())       // res: 64
println(5L.Bytes())       // res: 8


println(0b1111L.BitCount()) // res: 4
println(20L.HighestOneBit()) // res: 16
println(20L.LowestOneBit()) // res: 4

var x = 1L
println(x.NumberOfLeadingZeros()) // res: 63
x = 16L
println(x.NumberOfTrailingZeros()) // res: 4

println(16L.Reverse()) // res: 576460752303423488
println(16L.ReverseBytes()) // res: 1152921504606846976

println(1L.RotateLeft(4)) // res: 16
println(1L.RotateRight(1)) // res: -9223372036854775808

println(1L.Sign()) // res: 1
println(-1L.Sign()) // res: -1

println(123456789L.ToBinaryString()) // res: 111010110111100110100010101
println(123456789L.ToHexString())    // res: 75bcd15
println(123456789L.ToOctalString())  // res: 726746425

println(123456789L.toString())  // res: 123456789

println(123456789L.BitsToDouble())  // res: 6.0995758E-316


x = 0b1111L
println(x[0]) // res: 1
println(x[1]) // res: 1
println(x[4]) // res: 0


println(0xFFFFFFFFFFFFFFFFL[:]) // res: -1
println(0xFFFFFFFFFFFFFFFFL[0:63]) // res: -1
println(0xFFFFFFFFFFFFFFFFL[48:]) // res: 65535
println(0xFFFFFFFFFFFFFFFFL[63:]) // res: 1
println(0xFFFFFFFFFFFFFFFFL[1:5]) // res: 8935141660703064064
