// Ignore
// This is pretty much the same as Star 2 so there's
// no need to test both since they take around 2-3 seconds.
import java::lang::Math
import T::std::LinkedList
import T::std::Stack
import T::std::List
import T::std::Vector

val input = `32/31
2/2
0/43
45/15
33/24
20/20
14/42
2/35
50/27
2/17
5/45
3/14
26/1
33/38
29/6
50/32
9/48
36/34
33/50
37/35
12/12
26/13
19/4
5/5
14/46
17/29
45/43
5/0
18/18
41/22
50/3
4/4
17/1
40/7
19/0
33/7
22/48
9/14
50/43
26/29
19/33
46/31
3/16
29/46
16/0
34/17
31/7
5/27
7/4
49/49
14/21
50/9
14/44
29/29
13/38
31/11`

val _testInput = `0/2
2/2
2/3
3/4
3/5
0/1
10/1
9/10`


class Component =

	Val Ports = new Int[2]
	Val Used = new Bool[2]
	Var Strength: Int

	Def new(line: String) =
		val s = line.Split("/")
		Ports[0] = s[0].ToInt()
		Ports[1] = s[1].ToInt()
		Used[0] = Used[1] = false
		Strength = Ports[0] + Ports[1]

	Def toString() = Ports[0] + "/" + Ports[1]


class MaxBridge =
	val bridge: Stack<Component> = new Vector<Component>()
	val unused: List<Component> = new Vector<Component>()

	Var Max = 0

	Def new(input: String) =
		val lines = input.Lines()

		for(var i = 0; i < lines.Size(); i++)
			unused.Add(new Component(lines[i]))

	Def ValidPorts(component: Component): Int[] =
		val lastComponent = bridge.Peek()
		val port = lastComponent ? (lastComponent.Used[0] ? lastComponent.Ports[1] : lastComponent.Ports[0]) : 0
		if(component.Ports[0] == port && component.Ports[1] == port)
			return [0, 1]
		if(component.Ports[0] == port)
			return [0]
		if(component.Ports[1] == port)
			return [1]
		return []

	Def BridgeStrength() =
		var sum = 0
		for(val component in bridge)
			sum += component.Strength
		return sum

	Def Run(depth: Int): Unit =
		Max = Math.max(Max, BridgeStrength())
		val size = unused.Size()
		for(var i = 0; i < size; i++)
			val component = unused[i]
			val validPorts = ValidPorts(component)
			for(val port in validPorts)
				component.Used[port] = true
				unused.RemoveIndex(i)
				bridge.Push(component)

				Run(depth + 1)

				bridge.Pop()
				unused.Add(i, component)
				component.Used[port] = false

val m = new MaxBridge(input)
m.Run(0)
println(m.Max) // res: 1511