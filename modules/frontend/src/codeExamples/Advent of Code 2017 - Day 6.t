/*---------------------------------------------------------------------------*/
/*                        Advent of Code 2017 - Day 6                        */
/*---------------------------------------------------------------------------*/

import java::util::Arrays
import T::std::HashMap

val input = [ 11, 11, 13, 7, 0, 15, 5, 5, 4, 4, 1, 1, 7, 1, 15, 11 ]

class Day6 =

	val seen = new HashMap<Int, Int>()
	var memoryBanks: Int[]

	Def new(memoryBanks: Int[]) = this.memoryBanks = memoryBanks

	def seenCurrent(hash: Int) = seen.Contains(hash)

	def redistribute() =
		var maxIndex = 0
		var max = 0
		val N = memoryBanks.Size()
		for(var i = 0; i < N; i++)
			val bank = memoryBanks[i]
			if(bank > max)
				maxIndex = i
				max = bank
		memoryBanks[maxIndex++] = 0
		while(max-- > 0)
			memoryBanks[(maxIndex++) % N]++

	Def Run() =
		var hash = Arrays.hashCode(memoryBanks)
		while(!seenCurrent(hash))
			seen.Add(hash, hash)
			redistribute()
			hash = Arrays.hashCode(memoryBanks)

		val seenHash = hash
		redistribute()
		hash = Arrays.hashCode(memoryBanks)

		var count = 1

		while(hash != seenHash)
			seen.Add(hash, hash)
			redistribute()
			hash = Arrays.hashCode(memoryBanks)
			count++
		println("Count: " + count) // res: Count: 2793

new Day6(input).Run()
