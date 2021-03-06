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

println(0xFFFFFFFF[:]) // res: -1
println(0xFFFFFFFF[0:31]) // res: -1
println(0xFFFFFFFF[16:]) // res: 65535
println(0xFFFFFFFF[31:]) // res: 1
println(0xFFFFFFFF[1:5]) // res: 2080374784

println(0.SetBit(0)) // res: 1
println(0.SetBit(1)) // res: 2
println(0.SetBit(2)) // res: 4
println(1.ClearBit(0)) // res: 0
println(2.ClearBit(1)) // res: 0
println(3.ClearBit(1)) // res: 1
println(3.ToggleBit(1)) // res: 1
println(0.ToggleBit(1)) // res: 2
println(0.ToggleBit(0).ToggleBit(0)) // res: 0