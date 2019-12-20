import T::std::Vector
import java::lang::Math

val input = "3,8,1001,8,10,8,105,1,0,0,21,34,59,68,89,102,183,264,345,426,99999,3,9,102,5,9,9,1001,9,5,9,4,9,99,3,9,101,3,9,9,1002,9,5,9,101,5,9,9,1002,9,3,9,1001,9,5,9,4,9,99,3,9,101,5,9,9,4,9,99,3,9,102,4,9,9,101,3,9,9,102,5,9,9,101,4,9,9,4,9,99,3,9,1002,9,5,9,1001,9,2,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,99"
val testInput = "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"

class Tuple<T1, T2> =
	Var First: T1
	Var Second: T2

	Def new(t1: T1, t2: T2) =
		First = t1
		Second = t2

	Def toString() = "(" + First + ", " + Second + ")"

class Amplifier =
	Var Program: Vector<Int>
	Var PC: Int = 0
	Var PhaseSetting: Int
	Var First: Bool = true

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

class Day7 =

	val baseProgram = new Vector<Int>()
	val NUM_AMPLIFIERS = 5
	var max = 0

	Def Run(input: String) =
		val split = input.Split(",")

		for (val x in split)
			baseProgram.Add(x.ToInt())

		val phaseSettings = new Int[NUM_AMPLIFIERS];
		for (var i = 0; i < NUM_AMPLIFIERS; i++)
			phaseSettings[i] = i + 5;


		Permute(phaseSettings, 0, NUM_AMPLIFIERS - 1)
		println(max) // res: 30872528

	Def Permute(phaseSettings: Int[], l: Int, r: Int): Unit =
		if (l == r)
			max = Math.max(max, TryPhaseSettings(phaseSettings))
			return
		for (var i = l; i <= r; i++)
			Swap(phaseSettings, l, i)
			Permute(phaseSettings, l + 1, r)
			Swap(phaseSettings, l, i)

	Def Swap(elements: Int[], a: Int, b: Int) =
		val tmp = elements[a]
		elements[a] = elements[b]
		elements[b] = tmp

	Def TryPhaseSettings(phaseSettings: Int[]) =
		val amplifiers = new Vector<Amplifier>()
		for(val phaseSetting in phaseSettings)
			val amplifier = new Amplifier()
			amplifier.Program = baseProgram.Copy()
			amplifier.PhaseSetting = phaseSetting
			amplifiers.Add(amplifier)

		var i = 0

		var lastInput = 0
		for(;;)
			val amplifier = amplifiers[i]
			i = (i + 1) % amplifiers.Size()
			val t = Execute(amplifier.Program, amplifier.PhaseSetting, lastInput, amplifier.PC, amplifier.First)
			val input = t.First
			amplifier.PC = t.Second
			amplifier.First = false
			if (input == -1)
				break
			lastInput = input

		return lastInput

	Def Execute(program: Vector<Int>, phaseSetting: Int, input: Int, originalPc: Int, first: Bool): Tuple<Int, Int> =
		var output = -1
		var f = first
		var pc = originalPc
		for (;;)
			val s = program[pc].toString()
			val opCode = s[-2:].ToInt()
			if(opCode == 99)
				break

			val parameterModes = s[:-2]
			if (opCode == 1)
				val params = GetParameters(program, pc, parameterModes, 3)
				program[params[2].Value] = params[0].Get() + params[1].Get()
				pc += 4
			if (opCode == 2)
				val params = GetParameters(program, pc, parameterModes, 3)
				program[params[2].Value] = params[0].Get() * params[1].Get()
				pc += 4
			if (opCode == 3)
				val params = GetParameters(program, pc, parameterModes, 1)
				val v = f ? phaseSetting : input
				f = false
				program[params[0].Value] = v
				pc += 2
			if (opCode == 4)
				val params = GetParameters(program, pc, parameterModes, 1)
				output = params[0].Get()
				pc += 2
				break
			if (opCode == 5)
				val params = GetParameters(program, pc, parameterModes, 2)
				pc = params[0].Get() != 0 ? params[1].Get() : pc + 3
			if (opCode == 6)
				val params = GetParameters(program, pc, parameterModes, 2)
				pc = params[0].Get() == 0 ? params[1].Get() : pc + 3
			if (opCode == 7)
				val params = GetParameters(program, pc, parameterModes, 3)
				val v1 = params[0].Get()
				val v2 = params[1].Get()
				program[params[2].Value] = v1 < v2 ? 1 : 0
				pc += 4
			if (opCode == 8)
				val params = GetParameters(program, pc, parameterModes, 3)
				val v1 = params[0].Get()
				val v2 = params[1].Get()
				program[params[2].Value] = v1 == v2 ? 1 : 0
				pc += 4

		return new Tuple<Int, Int>(output, pc)

	Def GetParameters(program: Vector<Int>, pc: Int, parameterModes: String, numParameters: Int) =
		val values = new Int[numParameters]
		for (var i = 0; i < numParameters; i++)
			values[i] = program[pc + i + 1]

		val params = new Vector<Param>()
		val n = parameterModes.Size();
		for (var i = 0 ; i < values.Size(); i++)
			val mode = i < n ? parameterModes[n - i - 1].NumericValue() : 0
			params.Add(new Param(program, values[i], mode))

		return params

new Day7().Run(input)