// Generator A starts with 289
// Generator B starts with 629

class Generator =
	var n: Long
	var factor: Long
	var validMultiple: Long

	Def new(start: Long, factor: Long, validMultiple: Long) =
		n = start
		this.factor = factor
		this.validMultiple = validMultiple

	Def Next(): Long =
		n = (n * factor) % 2147483647
		while(n % validMultiple != 0)
			n = (n * factor) % 2147483647
		n

val genA = new Generator(289, 16807, 4)
val genB = new Generator(629, 48271, 8)

val num = 5_000_000
var count = 0
for(var i = 0; i < num; i++)
	val a = genA.Next()[48:]
	val b = genB.Next()[48:]
	if(a == b)
		count++

println("Count: " + count) // res: Count: 343