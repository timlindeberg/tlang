val a = [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]
var slice = a[:]
println(slice.Size()) // res: 10
for(val v in slice)
    print(v) // res: 12345678910
println("")

slice = a[:5]
println(slice.Size()) // res: 5
for(val v in slice)
    print(v) // res: 12345
println("")

slice = a[5:]
println(slice.Size()) // res: 5
for(val v in slice)
    print(v) // res: 678910
println("")

slice = a[3:5]
println(slice.Size()) // res: 2
for(val v in slice)
    print(v) // res: 45
println("")

slice = a[:1]
println(slice.Size()) // res: 1
for(val v in slice)
    print(v) // res: 1
println("")

slice = a[9:]
println(slice.Size()) // res: 1
for(val v in slice)
    print(v) // res: 10
println("")

slice = a[0:0]
println(slice.Size()) // res: 0


slice = a[0:9:2]
println(slice.Size()) // res: 5
for(val v in slice)
    print(v) // res: 13579
println("")

slice = a[0:10:2]
println(slice.Size()) // res: 5
for(val v in slice)
    print(v) // res: 13579
println("")

slice = a[1:10:2]
println(slice.Size()) // res: 5
for(val v in slice)
    print(v) // res: 246810
println("")


slice = a[:10:2]
println(slice.Size()) // res: 5
for(val v in slice)
    print(v) // res: 13579
println("")

slice = a[2::2]
println(slice.Size()) // res: 4
for(val v in slice)
    print(v) // res: 3579
println("")

slice = a[2:10:]
println(slice.Size()) // res: 8
for(val v in slice)
    print(v) // res: 345678910
println("")

slice = a[::2]
println(slice.Size()) // res: 5
for(val v in slice)
    print(v) // res: 13579
println("")

slice = a[:2:]
println(slice.Size()) // res: 2
for(val v in slice)
    print(v) // res: 12
println("")

slice = a[2::]
println(slice.Size()) // res: 8
for(val v in slice)
    print(v) // res: 345678910
println("")

slice = a[::]
println(slice.Size()) // res: 10
for(val v in slice)
    print(v) // res: 12345678910
println("")

