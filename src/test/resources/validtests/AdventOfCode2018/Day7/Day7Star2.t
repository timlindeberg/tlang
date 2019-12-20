import T::std::Vector
import T::std::HashMap
import T::std::HashSet
import T::std::Comparator
import java::util::regex::Matcher
import java::util::regex::Pattern
import java::lang::Math

val input = `Step Q must be finished before step O can begin.
Step Z must be finished before step G can begin.
Step W must be finished before step V can begin.
Step C must be finished before step X can begin.
Step O must be finished before step E can begin.
Step K must be finished before step N can begin.
Step P must be finished before step I can begin.
Step X must be finished before step D can begin.
Step N must be finished before step E can begin.
Step F must be finished before step A can begin.
Step U must be finished before step Y can begin.
Step M must be finished before step H can begin.
Step J must be finished before step B can begin.
Step B must be finished before step E can begin.
Step S must be finished before step L can begin.
Step A must be finished before step L can begin.
Step E must be finished before step L can begin.
Step L must be finished before step G can begin.
Step D must be finished before step I can begin.
Step Y must be finished before step I can begin.
Step I must be finished before step G can begin.
Step G must be finished before step R can begin.
Step V must be finished before step T can begin.
Step R must be finished before step H can begin.
Step H must be finished before step T can begin.
Step S must be finished before step E can begin.
Step C must be finished before step E can begin.
Step P must be finished before step T can begin.
Step I must be finished before step H can begin.
Step O must be finished before step P can begin.
Step M must be finished before step L can begin.
Step S must be finished before step D can begin.
Step P must be finished before step D can begin.
Step P must be finished before step R can begin.
Step I must be finished before step R can begin.
Step Y must be finished before step G can begin.
Step Q must be finished before step L can begin.
Step N must be finished before step R can begin.
Step J must be finished before step E can begin.
Step N must be finished before step T can begin.
Step B must be finished before step V can begin.
Step Q must be finished before step B can begin.
Step J must be finished before step H can begin.
Step F must be finished before step B can begin.
Step W must be finished before step X can begin.
Step S must be finished before step T can begin.
Step J must be finished before step G can begin.
Step O must be finished before step R can begin.
Step K must be finished before step B can begin.
Step Z must be finished before step O can begin.
Step Q must be finished before step S can begin.
Step K must be finished before step V can begin.
Step B must be finished before step R can begin.
Step J must be finished before step T can begin.
Step E must be finished before step T can begin.
Step G must be finished before step V can begin.
Step D must be finished before step Y can begin.
Step M must be finished before step Y can begin.
Step F must be finished before step G can begin.
Step C must be finished before step P can begin.
Step V must be finished before step R can begin.
Step R must be finished before step T can begin.
Step J must be finished before step Y can begin.
Step U must be finished before step R can begin.
Step Z must be finished before step F can begin.
Step Q must be finished before step V can begin.
Step U must be finished before step M can begin.
Step J must be finished before step R can begin.
Step L must be finished before step V can begin.
Step W must be finished before step K can begin.
Step B must be finished before step Y can begin.
Step O must be finished before step N can begin.
Step D must be finished before step V can begin.
Step P must be finished before step B can begin.
Step U must be finished before step I can begin.
Step O must be finished before step T can begin.
Step S must be finished before step G can begin.
Step X must be finished before step A can begin.
Step U must be finished before step T can begin.
Step A must be finished before step I can begin.
Step B must be finished before step G can begin.
Step N must be finished before step Y can begin.
Step Z must be finished before step J can begin.
Step M must be finished before step D can begin.
Step U must be finished before step A can begin.
Step S must be finished before step R can begin.
Step Z must be finished before step A can begin.
Step Y must be finished before step R can begin.
Step E must be finished before step Y can begin.
Step N must be finished before step G can begin.
Step Z must be finished before step X can begin.
Step P must be finished before step X can begin.
Step Z must be finished before step T can begin.
Step Z must be finished before step P can begin.
Step V must be finished before step H can begin.
Step P must be finished before step L can begin.
Step L must be finished before step H can begin.
Step X must be finished before step V can begin.
Step W must be finished before step G can begin.
Step N must be finished before step D can begin.
Step Z must be finished before step U can begin.`

val testInput = `Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin.
`

class CharDescending: Comparator<Char> =
	Def Compare(a: Char, b: Char) = b - a

class Worker =
	Var FinishedAt = -1
	Var Task: Char? = null

	Def toString() = Task ? "(" + Task!! + ": " + FinishedAt + ")" : "Free"

class Day7 =

	Val NumWorkers = 5
	Val BaseTime = 60

	var time = 0
	val graph = new HashMap<Char, Vector<Char>>()
	val parents = new HashMap<Char, Vector<Char>>()
	val workers = new Worker[NumWorkers]
	var nodes = new HashSet<Char>()
	var finished = new HashSet<Char>()

	Def new(input: String) =
		ParseGraph(input)
		for(var i = 0; i < NumWorkers; i++)
			workers[i] = new Worker()

	Def Run() =
		val queue = new Vector<Char>()

		queue.AddAll(FindStarts())
		queue.Sort(new CharDescending())

		while(finished.Size() != nodes.Size())
			var finishedWorker = GetFinishedWorker()
			while(finishedWorker)
				val node = finishedWorker.Task!!
				finishedWorker.Task = null
				finished.Add(node)

				for(val e in graph[node])
					if(FinishedParents(e))
						queue.Add(e)
				finishedWorker = GetFinishedWorker()
				queue.Sort(new CharDescending())

			var worker = GetFreeWorker()
			while(worker && !queue.IsEmpty())
				val node = queue.Pop()
				worker.Task = node
				worker.FinishedAt = time + BaseTime + 1 + (node - 'A')
				worker = GetFreeWorker()

			time++

		println(time - 1)

	Def GetFreeWorker(): Worker? =
		for(val worker in workers)
			if(!worker.Task)
				return worker
		return null

	Def GetFinishedWorker(): Worker? =
		for(val worker in workers)
			if(worker.Task && time == worker.FinishedAt)
				return worker
		return null

	Def FinishedParents(node: Char) =
		for(val p in parents[node])
			if(!finished[p])
				return false
		true

	Def FindStarts() =
		val starts = new Vector<Char>()
		for(val e in parents)
			if(e.Value().IsEmpty())
				starts.Add(e.Key())
		starts

	Def ParseGraph(input: String) =
		val r = Pattern.compile(`Step ([A-Z]) must be finished before step ([A-Z]) can begin.`)

		for(val line in input.Lines())
			val m = r.matcher(line)
			m.matches()
			val from = m.group(1)[0]
			val to = m.group(2)[0]
			nodes.Add(from)
			nodes.Add(to)

			val n = graph.GetOrDefault(from, new Vector<Char>())
			n.Add(to)
			graph.GetOrDefault(to, new Vector<Char>())

			val p = parents.GetOrDefault(to, new Vector<Char>())
			p.Add(from)
			parents.GetOrDefault(from, new Vector<Char>())

new Day7(input).Run() // res: 914
