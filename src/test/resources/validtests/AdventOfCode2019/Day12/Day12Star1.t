// ignore
import T::std::Vector
import T::std::Set
import T::std::HashSet
import T::std::Vec3
import T::std::Matrix
import java::util::regex::Matcher
import java::util::regex::Pattern
import java::lang::Math

val input = `
<x=-9, y=10, z=-1>
<x=-14, y=-8, z=14>
<x=1, y=5, z=6>
<x=-19, y=7, z=8>
`.Trim()

val testInput = `
<x=-1, y=0, z=2>
<x=2, y=-10, z=-7>
<x=4, y=-8, z=8>
<x=3, y=5, z=-1>
`.Trim()

class Day12 =

	Val Steps = 1000000

	var pos: Vector<Vec3<Int>>
	var vel: Vector<Vec3<Int>>

	var states: Set<Int>

	val r = Pattern.compile(`\<x=(\-?\d+), y=(\-?\d+), z=(\-?\d+)\>`)

	Def ParseVec(s: String) =
		val m = r.matcher(s)
		m.matches()
		new Vec3<Int>(m.group(1).ToInt(), m.group(2).ToInt(), m.group(3).ToInt())

	Def AdjustVelocity(i: Int, j: Int) =
		if (pos[i].X > pos[j].X)
			vel[i].X--
			vel[j].X++
		else if(pos[i].X < pos[j].X)
			vel[i].X++
			vel[j].X--

		if (pos[i].Y > pos[j].Y)
			vel[i].Y--
			vel[j].Y++
		else if(pos[i].Y < pos[j].Y)
			vel[i].Y++
			vel[j].Y--

		if (pos[i].Z > pos[j].Z)
			vel[i].Z--
			vel[j].Z++
		else if(pos[i].Z < pos[j].Z)
			vel[i].Z++
			vel[j].Z--

	Def PrintState(step: Int) =
		println("After " + step + " steps:")
		for (var i = 0; i < pos.Size(); i++)
			println("pos=<" + pos[i] + ">, vel=<" + vel[i] + ">")

	Def Run(input: String) =
		pos = new Vector<Vec3<Int>>()
		vel = new Vector<Vec3<Int>>()
		states = new HashSet<Int>()
		for(val s in input.Lines())
			pos.Add(ParseVec(s))
			vel.Add(new Vec3<Int>(0, 0, 0))

		pos[0] = new Vec3<Int>(0, 0, 0)

		val initialPos = pos.Copy()
		val initialVel = vel.Copy()

		for (var step = 0L; step < Steps; step++)
			for (var i = 0; i < pos.Size(); i++)
				for (var j = 0; j < i; j++)
					AdjustVelocity(i, j)

			for (var i = 0; i < pos.Size(); i++)
				pos[i] += vel[i]

		println("Done!")



new Day12().Run(testInput)