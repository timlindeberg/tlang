import T::std::Vector
import T::std::HashMap
import T::std::HashSet
import java::lang::Math
import java::util::Arrays
import java::lang::StringBuilder
import java::util::regex::Matcher
import java::util::regex::Pattern

val initial = "##.##.##..#..#.#.#.#...#...#####.###...#####.##..#####.#..#.##..#..#.#...#...##.##...#.##......####."
val input = `##.#. => #
#.#.. => #
##... => .
...## => #
###.# => #
#.##. => #
#.### => #
####. => #
.#..# => #
...#. => .
#..#. => .
#.#.# => .
.##.# => .
..#.. => .
.#.## => #
..##. => .
.#.#. => #
#..## => #
..#.# => #
#.... => .
..### => .
#...# => .
##### => #
###.. => #
....# => .
##.## => #
.#### => .
..... => .
##..# => #
.##.. => .
.###. => .
.#... => #`

val testInitial = "#..#.#..##......###...###"
val testInput = `...## => #
..#.. => #
.#... => #
.#.#. => #
.#.## => #
.##.. => #
.#### => #
#.#.# => #
#.### => #
##.#. => #
##.## => #
###.. => #
###.# => #
####. => #`

class Entries =
	var entries: Bool[]

	Def new(entries: Bool[]) = (this.entries = entries)

	Def ==(a: Entries, b: Entries) = Arrays.equals(a.entries, b.entries)

	Def toString() =
		val sb = new StringBuilder()
		for(val e in entries)
			if(e) sb.append("#")
			else  sb.append(".")
		sb.toString()

class Rule =
	Var ProducesPlant: Bool
	Var Entries: Entries

	Def new(producesPlant: Bool, entries: Entries) =
		ProducesPlant = producesPlant
		Entries = entries

	Def toString() = Entries + " => " + (ProducesPlant ? "#" : ".")

class Pot =
	Var Number: Int
	Var HasPlant: Bool

	Def new(number: Int, hasPlant: Bool) =
		Number = number
		HasPlant = hasPlant

	Def toString() = "(" + Number + ", " + HasPlant + ")"

	Def #(p: Pot) = p.Number
	Def ==(p1: Pot, p2: Pot) = p1.Number == p2.Number

class Day12 =

	val PotsToAdd = 150

	var Pots: Pot[]
	var Rules: Rule[]

	Def new(initial: String, input: String) =
		val N = initial.Size()
		Pots = new Pot[PotsToAdd * 2 + N]
		for(var i = 0; i < Pots.Size(); i++)
			val number = -PotsToAdd + i
			val hasPot = number >= 0 && number < N && initial[number] == '#'
			Pots[i] = new Pot(-PotsToAdd + i, hasPot)

		val lines = input.Lines()
		Rules = new Rule[lines.Size()]
		for(var i = 0; i < lines.Size(); i++)
			val line = lines[i]
			val s = line.Split(" => ")
			val pattern = s[0]
			val entries = [
			    pattern[0] == '#',
			    pattern[1] == '#',
			    pattern[2] == '#',
			    pattern[3] == '#',
			    pattern[4] == '#',
			]
			val producesPlant = s[1] == "#"
			Rules[i] = new Rule(producesPlant, new Entries(entries))

	Def Run() =
		for(var generation = 1; generation <= 20; generation++)
			val changes = new HashMap<Pot, Bool>()
			for(val pot in Pots)
				val rule = FindMatchingRule(pot)
				changes[pot] = rule?.ProducesPlant ?: false

			for(val e in changes)
				e.Key().HasPlant = e.Value()

		println(GetSum()) // res: 2930

	Def GetSum() =
		var sum = 0
		for(val pot in Pots)
			if(pot.HasPlant) sum += pot.Number
		sum

	Def FindMatchingRule(pot: Pot) =
		val e = new Bool[5]
		for(var i = 0; i < 5; i++)
			val index = pot.Number + PotsToAdd + i - 2
			if(index >= 0 && index < Pots.Size()    )
				e[i] = Pots[index].HasPlant
		val entries = new Entries(e)
		for(val rule in Rules)
			if(rule.Entries == entries)
				return rule
		null

	Def PrintPots() =
		for(val pot in Pots)
			print(pot.HasPlant ? "#" : ".")
		println()


new Day12(initial, input).Run()
