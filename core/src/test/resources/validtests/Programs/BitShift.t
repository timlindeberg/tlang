var i: Int = 1

println(i << 0) // res: 1
println(i << 1) // res: 2
println(i << 2) // res: 4
println(i << 3) // res: 8
println(i << 4) // res: 16
println(i << 5) // res: 32
i = i << 5

println(i >> 0) // res: 32
println(i >> 1) // res: 16
println(i >> 2) // res: 8
println(i >> 3) // res: 4
println(i >> 4) // res: 2
println(i >> 5) // res: 1
