import T::std::HashMap

val input =`
`

var numTwo = 0
var numThree = 0
for(val line in input.Lines())
	val counts = new HashMap<Char, Int>()
	for(val c in line)
		counts[c] = (counts.Get(c) ?: 0) + 1

	var countedTwo = false
	var countedThree = false
	for(val entry in counts)
		val v = entry.Value()
		if(!countedTwo && v == 2)
			numTwo++
			countedTwo = true
		else if(!countedThree && v == 3)
			numThree++
			countedThree = true
		else if(countedTwo && countedThree)
			break

println(numTwo * numThree) // res: 4712

