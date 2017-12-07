println(Int.MaxValue()) // res: 2147483647
println(Int.MinValue()) // res: -2147483648
println(5.Size())       // res: 32
println(5.Bytes())      // res: 4

println(0b1111.BitCount())  // res: 4
println(20.HighestOneBit()) // res: 16
println(20.LowestOneBit())  // res: 4

var x = 1
println(x.NumberOfLeadingZeros()) // res: 31
x = 16
println(x.NumberOfTrailingZeros()) // res: 4

println(16.Reverse()) // res: 134217728
println(16.ReverseBytes()) // res: 268435456

println(1.RotateLeft(4)) // res: 16
println(1.RotateRight(1)) // res: -2147483648

println(1.Sign()) // res: 1
println(-1.Sign()) // res: -1

println(123456789.ToBinaryString()) // res: 111010110111100110100010101
println(123456789.ToHexString())    // res: 75bcd15
println(123456789.ToOctalString())  // res: 726746425

println(123456789.toString())  // res: 123456789

println(123456789.BitsToFloat())  // res: 1.6535997E-34


x = 0b1111
println(x[0]) // res: 1
println(x[1]) // res: 1
println(x[4]) // res: 0
