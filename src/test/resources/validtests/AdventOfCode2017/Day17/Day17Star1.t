import T::std::Vector

val step = 363


val buffer = new Vector<Int>()
buffer.Add(0)

var pos = 0
for(var i = 1; i <= 2017; i++)
	pos = (pos + step) % i + 1
	buffer.Add(pos, i)

println(buffer[buffer.IndexOf(2017) + 1]) // res: 136
