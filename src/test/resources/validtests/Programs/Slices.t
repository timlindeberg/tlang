val a = [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]
var slice = a[:]
println(slice.Size()) // res: 10
for(val v in slice)
    println(v) // res: 1, 2, 3, 4, 5, 6, 7, 8, 9, 10

slice = a[:5]
println(slice.Size()) // res: 5
for(val v in slice)
    println(v) // res: 1, 2, 3, 4, 5

slice = a[5:]
println(slice.Size()) // res: 5
for(val v in slice)
    println(v) // res: 6, 7, 8, 9, 10

slice = a[3:5]
println(slice.Size()) // res: 2
for(val v in slice)
    println(v) // res: 4, 5

slice = a[:1]
println(slice.Size()) // res: 1
for(val v in slice)
    println(v) // res: 1

slice = a[9:]
println(slice.Size()) // res: 1
for(val v in slice)
    println(v) // res: 10

slice = a[0:0]
println(slice.Size()) // res: 0


slice = a[0:9:2]
println(slice.Size()) // res: 5
for(val v in slice)
    println(v) // res: 1, 3, 5, 7, 9

slice = a[0:10:2]
println(slice.Size()) // res: 5
for(val v in slice)
    println(v) // res: 1, 3, 5, 7, 9

slice = a[1:10:2]
println(slice.Size()) // res: 5
for(val v in slice)
    println(v) // res: 2, 4, 6, 8, 10


slice = a[:10:2]
println(slice.Size()) // res: 5
for(val v in slice)
    println(v) // res: 1, 3, 5, 7, 9

slice = a[2::2]
println(slice.Size()) // res: 4
for(val v in slice)
    println(v) // res: 3, 5, 7, 9

slice = a[2:10:]
println(slice.Size()) // res: 8
for(val v in slice)
    println(v) // res: 3, 4, 5, 6, 7, 8, 9, 10

slice = a[::2]
println(slice.Size()) // res: 5
for(val v in slice)
    println(v) // res: 1, 3, 5, 7, 9

slice = a[:2:]
println(slice.Size()) // res: 2
for(val v in slice)
    println(v) // res: 1, 2

slice = a[2::]
println(slice.Size()) // res: 8
for(val v in slice)
    println(v) // res: 3, 4, 5, 6, 7, 8, 9, 10

slice = a[::]
println(slice.Size()) // res: 10
for(val v in slice)
    println(v) // res: 1, 2, 3, 4, 5, 6, 7, 8, 9, 10

