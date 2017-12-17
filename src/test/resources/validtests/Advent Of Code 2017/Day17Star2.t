import T::std::LinkedList

val step = 363

var pos = 0
var value = -1
for(var i = 1; i <= 50000000; i++)
	pos = (pos + step) % i + 1
	if(pos == 1)
		value = i

println(value) // res: 1080289
