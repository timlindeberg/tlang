class Day12 =

	val PotsToAdd = 150

	Def new(initial: String, input: String) =
		val N = 250
		val Pots = new Bool[N]
		for(var i = 0; i < Pots.Size(); i++)
			val number = -PotsToAdd + i
			val hasPot = number >= 0 && number < N && initial[number] == '#'
			Pots[i] = hasPot

