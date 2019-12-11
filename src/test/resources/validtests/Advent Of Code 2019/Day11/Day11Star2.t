// ignore

import T::std::Vector
import T::std::Vec2
import T::std::Matrix

val input = "3,8,1005,8,351,1106,0,11,0,0,0,104,1,104,0,3,8,102,-1,8,10,1001,10,1,10,4,10,108,1,8,10,4,10,102,1,8,28,3,8,1002,8,-1,10,101,1,10,10,4,10,1008,8,0,10,4,10,1002,8,1,51,1006,0,85,2,1109,8,10,3,8,1002,8,-1,10,101,1,10,10,4,10,1008,8,0,10,4,10,102,1,8,80,1,2,2,10,1,1007,19,10,1,1001,13,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,108,1,8,10,4,10,1001,8,0,113,1,2,1,10,1,1109,17,10,1,108,20,10,2,1005,3,10,3,8,102,-1,8,10,1001,10,1,10,4,10,108,1,8,10,4,10,1002,8,1,151,2,5,19,10,1,104,19,10,1,109,3,10,1006,0,78,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,0,10,4,10,1002,8,1,189,1006,0,3,2,1004,1,10,3,8,1002,8,-1,10,101,1,10,10,4,10,1008,8,1,10,4,10,1001,8,0,218,1,1008,6,10,1,104,8,10,1006,0,13,3,8,1002,8,-1,10,101,1,10,10,4,10,1008,8,0,10,4,10,102,1,8,251,1006,0,17,1006,0,34,1006,0,24,1006,0,4,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,0,10,4,10,102,1,8,285,1006,0,25,2,1103,11,10,1006,0,75,3,8,1002,8,-1,10,1001,10,1,10,4,10,108,1,8,10,4,10,101,0,8,316,2,1002,6,10,1006,0,30,2,106,11,10,1006,0,21,101,1,9,9,1007,9,1072,10,1005,10,15,99,109,673,104,0,104,1,21101,0,937151009684,1,21101,0,368,0,1105,1,472,21102,386979963796,1,1,21102,379,1,0,1106,0,472,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,21101,179410325723,0,1,21101,426,0,0,1106,0,472,21101,0,179355823195,1,21102,437,1,0,1106,0,472,3,10,104,0,104,0,3,10,104,0,104,0,21101,0,825460785920,1,21101,460,0,0,1105,1,472,21102,1,838429614848,1,21102,1,471,0,1105,1,472,99,109,2,21202,-1,1,1,21102,40,1,2,21102,1,503,3,21101,493,0,0,1105,1,536,109,-2,2106,0,0,0,1,0,0,1,109,2,3,10,204,-1,1001,498,499,514,4,0,1001,498,1,498,108,4,498,10,1006,10,530,1101,0,0,498,109,-2,2106,0,0,0,109,4,2101,0,-1,535,1207,-3,0,10,1006,10,553,21101,0,0,-3,21202,-3,1,1,22101,0,-2,2,21101,0,1,3,21101,572,0,0,1105,1,577,109,-4,2105,1,0,109,5,1207,-3,1,10,1006,10,600,2207,-4,-2,10,1006,10,600,21202,-4,1,-4,1106,0,668,21202,-4,1,1,21201,-3,-1,2,21202,-2,2,3,21102,619,1,0,1105,1,577,22102,1,1,-4,21101,0,1,-1,2207,-4,-2,10,1006,10,638,21101,0,0,-1,22202,-2,-1,-2,2107,0,-3,10,1006,10,660,22101,0,-1,1,21101,660,0,0,106,0,535,21202,-2,-1,-2,22201,-4,-2,-4,109,-5,2105,1,0"
val testInput = "104,1125899906842624,99"

class Param =
	Var Base: Long
	Var Value: Long
	Var Mode: Int

	var program: Vector<Long>

	Def new(program: Vector<Long>, base: Long, value: Long, mode: Int) =
		this.program = program
		Base = base
		Value = value
		Mode = mode

	Def Get(): Long = Mode == 0 ? program[Value as Int] :
	                  Mode == 1 ? Value :
	                  program[(Base + Value) as Int]

	Def toString() = Value + "(" + Mode + "): " + Get()

class Day11 =

	Val Up = new Vec2<Int>(0, -1)
	Val Down = new Vec2<Int>(0, 1)
	Val Left = new Vec2<Int>(-1, 0)
	Val Right = new Vec2<Int>(1, 0)

	Val Width = 85
	Val Height = 85

	var program: Vector<Long>
	var base: Long = 0
	var pc = 0

	var panels = new Long[Height][Width]
	var pos = new Vec2<Int>(Width / 2, Height / 2)
	var direction = Up

	Def Run(input: String) =
		program = ReadProgram(input)

		panels[pos.Y][pos.X] = 1

		var first = true
		var i = 0
		while (true)
			val output: Long? = Execute(panels[pos.Y][pos.X])
			if (!output)
				break
			if(first)
				panels[pos.Y][pos.X] = output
			else
				Rotate(output == 0)
				pos += direction
				i++

			first = !first

		PrintPanels()

	Def ReadProgram(input: String) =
		val program = new Vector<Long>(input.Size() * 100, 0)

		val split = input.Split(",")
		var i = 0
		for (val x in split)
			program[i++] = x.ToLong()
		program

	Def PrintPanels() =
		for (var y = 0; y < Height; y++)
			for (var x = 0; x < Width; x++)
				if (x == pos.X && y == pos.Y)
					val dir = direction == Up   ? '^' :
					          direction == Down ? 'v' :
					          direction == Left ? '<' :
					                              '>'
					print(dir)
				else
					print(panels[y][x] == 0 ? '.' : '#')
			println()

	Def Rotate(left: Bool) =
		if(direction == Up)
			direction = left ? Left : Right
		else if(direction == Down)
			direction = left ? Right : Left
		else if(direction == Right)
			direction = left ? Up : Down
		else if(direction == Left)
			direction = left ? Down : Up

	Def Execute(input: Long): Long? =
		var output: Long? = null
		for (;;)
			val s = program[pc].toString()
			val opCode = s[-2:].ToInt()
			if(opCode == 99)
				break
			val parameterModes = s[:-2]
			if (opCode == 1)
				val params = GetParameters(pc, parameterModes, 3)
				Write(params[2], params[0].Get() + params[1].Get())
				pc += 4
			if (opCode == 2)
				val params = GetParameters(pc, parameterModes, 3)
				Write(params[2], params[0].Get() * params[1].Get())
				pc += 4
			if (opCode == 3)
				val params = GetParameters(pc, parameterModes, 1)
				Write(params[0], input)
				pc += 2
			if (opCode == 4)
				val params = GetParameters(pc, parameterModes, 1)
				output = params[0].Get()
				pc += 2
				break
			if (opCode == 5)
				val params = GetParameters(pc, parameterModes, 2)
				pc = params[0].Get() != 0 ? params[1].Get() as Int : pc + 3
			if (opCode == 6)
				val params = GetParameters(pc, parameterModes, 2)
				pc = params[0].Get() == 0 ? params[1].Get() as Int : pc + 3
			if (opCode == 7)
				val params = GetParameters(pc, parameterModes, 3)
				val v1 = params[0].Get()
				val v2 = params[1].Get()
				Write(params[2], v1 < v2 ? 1 : 0)
				pc += 4
			if (opCode == 8)
				val params = GetParameters(pc, parameterModes, 3)
				val v1 = params[0].Get()
				val v2 = params[1].Get()
				Write(params[2], v1 == v2 ? 1 : 0)
				pc += 4
			if (opCode == 9)
				val params = GetParameters(pc, parameterModes, 1)
				base += params[0].Get()
				pc += 2
		output

	Def Write(destination: Param, value: Long) =
		var dest = destination.Value
		if(destination.Mode == 2)
			dest += destination.Base
		program[dest as Int] = value

	Def GetParameters(pc: Int, parameterModes: String, numParameters: Int) =
		val values = new Long[numParameters]
		for(var i = 0; i < numParameters; i++)
			values[i] = program[pc + i + 1]

		val params = new Vector<Param>()
		val n = parameterModes.Size();
		for(var i = 0 ; i < values.Size(); i++)
			val mode = i < n ? parameterModes[n - i - 1].NumericValue() : 0
			params.Add(new Param(program, base, values[i], mode))

		return params

new Day11().Run(input) // res: 3235019597