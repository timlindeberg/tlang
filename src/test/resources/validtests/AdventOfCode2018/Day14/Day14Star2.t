import T::std::Vector
import T::std::HashMap
import T::std::HashSet
import T::std::Comparator
import java::lang::Math
import java::util::Arrays
import java::lang::StringBuilder
import java::util::regex::Matcher
import java::util::regex::Pattern


class Day13 =

	val NumRecipes = "503761"

	val elves = new Int[2]
	val recipes: Vector<Int> = [3, 7]
	val recipesToSee = new Vector<Int>()

	Def new() =
		elves[0] = 0
		elves[1] = 1
		for(val i in NumRecipes)
			recipesToSee.Add(i - '0')

	Def Run() =
		var numSeen = 0
		while(numSeen != recipesToSee.Size())
			val score = recipes[elves[0]] + recipes[elves[1]]
			for(val digit in score.toString())
				if(numSeen == recipesToSee.Size())
					break
				val d = digit - '0'
				recipes.Add(d)
				if(d == recipesToSee[numSeen])
					numSeen++
				else
					numSeen = 0
					if(d == recipesToSee[numSeen])
						numSeen++
			for(var i = 0; i < 2; i++)
				elves[i] = (elves[i] + 1 + recipes[elves[i]]) % recipes.Size()
		println(recipes.Size() - recipesToSee.Size()) // res: 20185425


	def Print() =
		for(var i = 0; i < recipes.Size(); i++)
			val recipe = recipes[i]
			if(i == elves[0])      print("(" + recipe + ")")
			else if(i == elves[1]) print("[" + recipe + "]")
			else                   print(" " + recipe + " ")

		println()

new Day13().Run()
