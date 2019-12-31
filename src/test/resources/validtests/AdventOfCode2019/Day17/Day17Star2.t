import T::std::Vector
import T::std::HashMap
import T::std::HashSet
import T::std::Vec2
import T::std::Vec3
import T::std::Matrix

val input = "1,330,331,332,109,3524,1102,1182,1,16,1102,1457,1,24,102,1,0,570,1006,570,36,1001,571,0,0,1001,570,-1,570,1001,24,1,24,1105,1,18,1008,571,0,571,1001,16,1,16,1008,16,1457,570,1006,570,14,21101,0,58,0,1106,0,786,1006,332,62,99,21102,1,333,1,21102,1,73,0,1105,1,579,1102,1,0,572,1101,0,0,573,3,574,101,1,573,573,1007,574,65,570,1005,570,151,107,67,574,570,1005,570,151,1001,574,-64,574,1002,574,-1,574,1001,572,1,572,1007,572,11,570,1006,570,165,101,1182,572,127,102,1,574,0,3,574,101,1,573,573,1008,574,10,570,1005,570,189,1008,574,44,570,1006,570,158,1106,0,81,21102,1,340,1,1105,1,177,21102,477,1,1,1105,1,177,21101,0,514,1,21101,0,176,0,1106,0,579,99,21101,184,0,0,1106,0,579,4,574,104,10,99,1007,573,22,570,1006,570,165,1002,572,1,1182,21101,375,0,1,21102,1,211,0,1106,0,579,21101,1182,11,1,21102,1,222,0,1105,1,979,21101,388,0,1,21102,233,1,0,1105,1,579,21101,1182,22,1,21102,1,244,0,1105,1,979,21101,0,401,1,21101,0,255,0,1105,1,579,21101,1182,33,1,21101,0,266,0,1105,1,979,21102,414,1,1,21102,277,1,0,1106,0,579,3,575,1008,575,89,570,1008,575,121,575,1,575,570,575,3,574,1008,574,10,570,1006,570,291,104,10,21101,1182,0,1,21102,313,1,0,1106,0,622,1005,575,327,1102,1,1,575,21101,0,327,0,1105,1,786,4,438,99,0,1,1,6,77,97,105,110,58,10,33,10,69,120,112,101,99,116,101,100,32,102,117,110,99,116,105,111,110,32,110,97,109,101,32,98,117,116,32,103,111,116,58,32,0,12,70,117,110,99,116,105,111,110,32,65,58,10,12,70,117,110,99,116,105,111,110,32,66,58,10,12,70,117,110,99,116,105,111,110,32,67,58,10,23,67,111,110,116,105,110,117,111,117,115,32,118,105,100,101,111,32,102,101,101,100,63,10,0,37,10,69,120,112,101,99,116,101,100,32,82,44,32,76,44,32,111,114,32,100,105,115,116,97,110,99,101,32,98,117,116,32,103,111,116,58,32,36,10,69,120,112,101,99,116,101,100,32,99,111,109,109,97,32,111,114,32,110,101,119,108,105,110,101,32,98,117,116,32,103,111,116,58,32,43,10,68,101,102,105,110,105,116,105,111,110,115,32,109,97,121,32,98,101,32,97,116,32,109,111,115,116,32,50,48,32,99,104,97,114,97,99,116,101,114,115,33,10,94,62,118,60,0,1,0,-1,-1,0,1,0,0,0,0,0,0,1,52,26,0,109,4,1201,-3,0,586,21001,0,0,-1,22101,1,-3,-3,21102,0,1,-2,2208,-2,-1,570,1005,570,617,2201,-3,-2,609,4,0,21201,-2,1,-2,1105,1,597,109,-4,2106,0,0,109,5,1201,-4,0,629,21002,0,1,-2,22101,1,-4,-4,21101,0,0,-3,2208,-3,-2,570,1005,570,781,2201,-4,-3,653,20102,1,0,-1,1208,-1,-4,570,1005,570,709,1208,-1,-5,570,1005,570,734,1207,-1,0,570,1005,570,759,1206,-1,774,1001,578,562,684,1,0,576,576,1001,578,566,692,1,0,577,577,21102,1,702,0,1106,0,786,21201,-1,-1,-1,1105,1,676,1001,578,1,578,1008,578,4,570,1006,570,724,1001,578,-4,578,21102,1,731,0,1105,1,786,1105,1,774,1001,578,-1,578,1008,578,-1,570,1006,570,749,1001,578,4,578,21101,0,756,0,1105,1,786,1105,1,774,21202,-1,-11,1,22101,1182,1,1,21101,0,774,0,1106,0,622,21201,-3,1,-3,1105,1,640,109,-5,2105,1,0,109,7,1005,575,802,20102,1,576,-6,20101,0,577,-5,1106,0,814,21101,0,0,-1,21101,0,0,-5,21101,0,0,-6,20208,-6,576,-2,208,-5,577,570,22002,570,-2,-2,21202,-5,53,-3,22201,-6,-3,-3,22101,1457,-3,-3,1201,-3,0,843,1005,0,863,21202,-2,42,-4,22101,46,-4,-4,1206,-2,924,21101,0,1,-1,1106,0,924,1205,-2,873,21102,1,35,-4,1105,1,924,2102,1,-3,878,1008,0,1,570,1006,570,916,1001,374,1,374,1202,-3,1,895,1101,0,2,0,2102,1,-3,902,1001,438,0,438,2202,-6,-5,570,1,570,374,570,1,570,438,438,1001,578,558,922,20101,0,0,-4,1006,575,959,204,-4,22101,1,-6,-6,1208,-6,53,570,1006,570,814,104,10,22101,1,-5,-5,1208,-5,39,570,1006,570,810,104,10,1206,-1,974,99,1206,-1,974,1101,0,1,575,21102,973,1,0,1106,0,786,99,109,-7,2105,1,0,109,6,21102,1,0,-4,21102,1,0,-3,203,-2,22101,1,-3,-3,21208,-2,82,-1,1205,-1,1030,21208,-2,76,-1,1205,-1,1037,21207,-2,48,-1,1205,-1,1124,22107,57,-2,-1,1205,-1,1124,21201,-2,-48,-2,1106,0,1041,21102,1,-4,-2,1105,1,1041,21102,1,-5,-2,21201,-4,1,-4,21207,-4,11,-1,1206,-1,1138,2201,-5,-4,1059,1202,-2,1,0,203,-2,22101,1,-3,-3,21207,-2,48,-1,1205,-1,1107,22107,57,-2,-1,1205,-1,1107,21201,-2,-48,-2,2201,-5,-4,1090,20102,10,0,-1,22201,-2,-1,-2,2201,-5,-4,1103,2102,1,-2,0,1106,0,1060,21208,-2,10,-1,1205,-1,1162,21208,-2,44,-1,1206,-1,1131,1105,1,989,21101,439,0,1,1106,0,1150,21101,477,0,1,1106,0,1150,21101,0,514,1,21102,1149,1,0,1105,1,579,99,21102,1,1157,0,1105,1,579,204,-2,104,10,99,21207,-3,22,-1,1206,-1,1138,1201,-5,0,1176,2102,1,-4,0,109,-6,2106,0,0,14,11,1,11,30,1,9,1,1,1,9,1,30,1,7,9,5,1,30,1,7,1,1,1,1,1,3,1,5,1,30,1,7,1,1,1,1,1,3,1,5,1,30,1,7,1,1,1,1,1,3,1,5,1,30,1,1,9,1,1,3,1,5,1,30,1,1,1,5,1,3,1,3,1,5,1,30,1,1,1,5,1,3,9,1,1,30,1,1,1,5,1,7,1,3,1,1,1,30,9,7,7,32,1,17,1,34,1,11,9,32,1,11,1,5,1,1,1,32,9,3,1,5,1,1,1,40,1,3,1,5,1,1,1,40,1,3,1,5,1,1,1,40,1,3,1,7,1,40,1,3,1,7,7,34,1,3,1,13,1,30,9,13,1,30,1,3,1,17,1,28,7,17,1,28,1,1,1,21,1,28,1,1,1,21,1,28,1,1,1,21,1,28,1,1,1,21,11,18,1,1,1,32,7,3,9,1,1,32,1,5,1,3,1,9,1,32,1,5,1,3,1,1,9,32,1,5,1,3,1,1,1,40,1,5,1,3,1,1,1,40,1,5,1,3,1,1,1,40,1,5,1,3,1,1,1,40,1,5,1,3,1,1,1,40,1,5,7,40,1,9,1,42,11,42"

class Param =
	Var Base: Long
	Var Value: Long
	Var Mode: Int

	val program: Vector<Long>

	Def new(program: Vector<Long>, base: Long, value: Long, mode: Int) =
		this.program = program
		Base = base
		Value = value
		Mode = mode

	Def Get(): Long = Mode == 0 ? program[Value as Int] :
	                  Mode == 1 ? Value :
	                  program[(Base + Value) as Int]

	Def toString() = Value + "(" + Mode + "): " + Get()

class IntComputer =

	val program: Vector<Long>

	var pc = 0
	var base: Long = 0

	Def new(program: Vector<Long>) =
		this.program = program

	Def new(program: String) =
		this.program = readProgram(program)

	Def [](index: Int)               = program[index]
	Def []=(index: Int, value: Long) = program[index] = value

	Def Execute(input: Long): Long? =
		var output: Long? = null
		for (;;)
			val s = program[pc].toString()
			val opCode = s[-2:].ToInt()
			if(opCode == 99)
				break
			val parameterModes = s[:-2]
			if (opCode == 1)
				val params = getParameters(pc, parameterModes, 3)
				write(params[2], params[0].Get() + params[1].Get())
				pc += 4
			if (opCode == 2)
				val params = getParameters(pc, parameterModes, 3)
				write(params[2], params[0].Get() * params[1].Get())
				pc += 4
			if (opCode == 3)
				val params = getParameters(pc, parameterModes, 1)
				write(params[0], input)
				pc += 2
			if (opCode == 4)
				val params = getParameters(pc, parameterModes, 1)
				output = params[0].Get()
				pc += 2
				break
			if (opCode == 5)
				val params = getParameters(pc, parameterModes, 2)
				pc = params[0].Get() != 0 ? params[1].Get() as Int : pc + 3
			if (opCode == 6)
				val params = getParameters(pc, parameterModes, 2)
				pc = params[0].Get() == 0 ? params[1].Get() as Int : pc + 3
			if (opCode == 7)
				val params = getParameters(pc, parameterModes, 3)
				val v1 = params[0].Get()
				val v2 = params[1].Get()
				write(params[2], v1 < v2 ? 1 : 0)
				pc += 4
			if (opCode == 8)
				val params = getParameters(pc, parameterModes, 3)
				val v1 = params[0].Get()
				val v2 = params[1].Get()
				write(params[2], v1 == v2 ? 1 : 0)
				pc += 4
			if (opCode == 9)
				val params = getParameters(pc, parameterModes, 1)
				base += params[0].Get()
				pc += 2
		output

	Def write(destination: Param, value: Long) =
		var dest = destination.Value
		if(destination.Mode == 2)
			dest += destination.Base
		program[dest as Int] = value

	def getParameters(pc: Int, parameterModes: String, numParameters: Int) =
		val values = new Long[numParameters]
		for(var i = 0; i < numParameters; i++)
			values[i] = program[pc + i + 1]

		val params = new Vector<Param>()
		val n = parameterModes.Size();
		for(var i = 0 ; i < values.Size(); i++)
			val mode = i < n ? parameterModes[n - i - 1].NumericValue() : 0
			params.Add(new Param(program, base, values[i], mode))

		return params

	def readProgram(input: String) =
		val program = new Vector<Long>(input.Size() * 100, 0)

		val split = input.Split(",")
		var i = 0
		for (val x in split)
			program[i++] = x.ToLong()
		program

class Pos =
	Val Pos: Vec2<Int>
	Val Depth: Int

	Def new(pos: Vec2<Int>, depth: Int) =
		Pos = pos
		Depth = depth

	Def #(pos: Pos) = 31 * #pos.Pos + pos.Depth
	Def ==(a: Pos, b: Pos) = a.Pos == b.Pos && a.Depth == b.Depth

	Def toString() = "(" + Pos + ", " + Depth + ")"

class Day13 =

	var intComputer: IntComputer
	val Width = 55
	val Height = 55
	val board = new Char[Height][Width]

	var visited = new HashSet<Pos>()
	var start = new Vec2<Int>(-1, -1)

	val parents = new HashMap<Pos, Pos>()

	Def Run(input: String) =
		intComputer = new IntComputer(input)

		var x = 0
		var y = 0
		while (true)
			val res = intComputer.Execute(0)
			if(!res)
				break

			val char = res as Char
			if (char == '\n')
				x = 0
				y++
				continue

			if(char == '^')
				start = new Vec2<Int>(x, y)
			board[y][x++] = char

		PrintBoard()
		Visit(start, 0)
		for(val entry in parents)
			println(entry)
		val path = MakePath()
		for(val p in path)
			println(p)

	Def Visit(pos: Vec2<Int>, depth: Int): Unit =
		visited.Add(new Pos(pos, depth))

		TryVisit(pos, new Vec2<Int>(pos.X + 1, pos.Y), depth)
		TryVisit(pos, new Vec2<Int>(pos.X - 1, pos.Y), depth)
		TryVisit(pos, new Vec2<Int>(pos.X, pos.Y + 1), depth)
		TryVisit(pos, new Vec2<Int>(pos.X, pos.Y - 1), depth)

	Def TryVisit(from: Vec2<Int>, to: Vec2<Int>, depth: Int): Unit =
		if(!HasScaffold(to.X, to.Y))
		    return
		val p1 = new Pos(from, depth)
		val p2 = new Pos(to, depth + 1)
		if (visited.Contains(p2))
			parents[p1] = p2
			Visit(to, depth + 1)

	Def MakePath() =
		var next: Pos? = new Pos(start, 0)
		val path = new Vector<Vec2<Int>>()
		while(next)
			path.Add(next.Pos)
			println(next)
			next = parents.Get(next)
		path

	Def IsIntersection(x: Int, y: Int) =
		HasScaffold(y, x) &&
		HasScaffold(y + 1, x) &&
		HasScaffold(y - 1, x) &&
		HasScaffold(y, x + 1) &&
		HasScaffold(y, x - 1)

	Def IsInBounds(x: Int, y: Int) = x >= 0 && x < Width && y >= 0 && y < Height
	Def HasScaffold(x: Int, y: Int) = IsInBounds(x, y) && board[y][x] == '#'

	Def PrintBoard() =
		for (var y = 0; y < Height; y++)
			for (var x = 0; x < Width; x++)
				print(board[y][x])
			println()


new Day13().Run(input)