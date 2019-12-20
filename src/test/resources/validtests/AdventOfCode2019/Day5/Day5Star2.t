import T::std::Vector

val input = "3,225,1,225,6,6,1100,1,238,225,104,0,1001,152,55,224,1001,224,-68,224,4,224,1002,223,8,223,1001,224,4,224,1,224,223,223,1101,62,41,225,1101,83,71,225,102,59,147,224,101,-944,224,224,4,224,1002,223,8,223,101,3,224,224,1,224,223,223,2,40,139,224,1001,224,-3905,224,4,224,1002,223,8,223,101,7,224,224,1,223,224,223,1101,6,94,224,101,-100,224,224,4,224,1002,223,8,223,101,6,224,224,1,224,223,223,1102,75,30,225,1102,70,44,224,101,-3080,224,224,4,224,1002,223,8,223,1001,224,4,224,1,223,224,223,1101,55,20,225,1102,55,16,225,1102,13,94,225,1102,16,55,225,1102,13,13,225,1,109,143,224,101,-88,224,224,4,224,1002,223,8,223,1001,224,2,224,1,223,224,223,1002,136,57,224,101,-1140,224,224,4,224,1002,223,8,223,101,6,224,224,1,223,224,223,101,76,35,224,1001,224,-138,224,4,224,1002,223,8,223,101,5,224,224,1,223,224,223,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,1008,677,677,224,1002,223,2,223,1006,224,329,1001,223,1,223,8,677,226,224,102,2,223,223,1006,224,344,101,1,223,223,1107,226,226,224,1002,223,2,223,1006,224,359,1001,223,1,223,1108,677,226,224,102,2,223,223,1005,224,374,1001,223,1,223,1007,226,226,224,102,2,223,223,1006,224,389,1001,223,1,223,108,677,677,224,1002,223,2,223,1005,224,404,1001,223,1,223,1007,677,677,224,102,2,223,223,1005,224,419,1001,223,1,223,8,226,677,224,102,2,223,223,1005,224,434,101,1,223,223,1008,677,226,224,102,2,223,223,1006,224,449,1001,223,1,223,7,677,677,224,102,2,223,223,1006,224,464,1001,223,1,223,8,226,226,224,1002,223,2,223,1005,224,479,1001,223,1,223,7,226,677,224,102,2,223,223,1006,224,494,1001,223,1,223,7,677,226,224,1002,223,2,223,1005,224,509,101,1,223,223,107,677,677,224,102,2,223,223,1006,224,524,101,1,223,223,1007,677,226,224,102,2,223,223,1006,224,539,101,1,223,223,107,226,226,224,1002,223,2,223,1006,224,554,101,1,223,223,1008,226,226,224,102,2,223,223,1006,224,569,1001,223,1,223,1107,677,226,224,1002,223,2,223,1005,224,584,101,1,223,223,1107,226,677,224,102,2,223,223,1005,224,599,101,1,223,223,1108,226,677,224,102,2,223,223,1005,224,614,101,1,223,223,108,677,226,224,102,2,223,223,1005,224,629,101,1,223,223,107,226,677,224,102,2,223,223,1006,224,644,1001,223,1,223,1108,226,226,224,1002,223,2,223,1006,224,659,101,1,223,223,108,226,226,224,102,2,223,223,1005,224,674,101,1,223,223,4,223,99,226"
val testInput = "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"

class Param =
	Var Value: Int
	Var Mode: Int

	var program: Vector<Int>

	Def new(program: Vector<Int>, value: Int, mode: Int) =
		this.program = program
		Value = value
		Mode = mode

	Def Get() = Mode == 0 ? program[Value] : Value

	Def toString() = Value + "(" + Mode + "): " + Get()

class Day5 =

	val program = new Vector<Int>()
	val input = 5

	Def Run(input: String) =
		val split = input.Split(",")
		for(val x in split)
			program.Add(x.ToInt())
		Execute(program)


	Def Execute(program: Vector<Int>) =
		var output = -1

		var pc = 0

		for (;;)
			val s = program[pc].toString()
			val opCode = s[-2:].ToInt()
			if(opCode == 99)
				break
			val parameterModes = s[:-2]
			if (opCode == 1)
				val params = GetParameters(pc, parameterModes, 3)
				program[params[2].Value] = params[0].Get() + params[1].Get()
				pc += 4
			if (opCode == 2)
				val params = GetParameters(pc, parameterModes, 3)
				program[params[2].Value] = params[0].Get() * params[1].Get()
				pc += 4
			if (opCode == 3)
				val params = GetParameters(pc, parameterModes, 1)
				program[params[0].Value] = input
				pc += 2
			if (opCode == 4)
				val params = GetParameters(pc, parameterModes, 1)
				output = params[0].Get()
				pc += 2
			if (opCode == 5)
				val params = GetParameters(pc, parameterModes, 2)
				pc = params[0].Get() != 0 ? params[1].Get() : pc + 3
			if (opCode == 6)
				val params = GetParameters(pc, parameterModes, 2)
				pc = params[0].Get() == 0 ? params[1].Get() : pc + 3
			if (opCode == 7)
				val params = GetParameters(pc, parameterModes, 3)
				val v1 = params[0].Get()
				val v2 = params[1].Get()
				program[params[2].Value] = v1 < v2 ? 1 : 0
				pc += 4
			if (opCode == 8)
				val params = GetParameters(pc, parameterModes, 3)
				val v1 = params[0].Get()
				val v2 = params[1].Get()
				program[params[2].Value] = v1 == v2 ? 1 : 0
				pc += 4
		println(output) // res: 1409363

	Def GetParameters(pc: Int, parameterModes: String, numParameters: Int) =
		val values = new Int[numParameters]
		for(var i = 0; i < numParameters; i++)
			values[i] = program[pc + i + 1]

		val params = new Vector<Param>()
		val n = parameterModes.Size();
		for(var i = 0 ; i < values.Size(); i++)
			val mode = i < n ? parameterModes[n - i - 1].NumericValue() : 0
			params.Add(new Param(program, values[i], mode))

		return params

new Day5().Run(input)