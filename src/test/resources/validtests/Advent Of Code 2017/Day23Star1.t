import java::lang::Math

val input = `set b 79
set c b
jnz a 2
jnz 1 5
mul b 100
sub b -100000
set c b
sub c -17000
set f 1
set d 2
set e 2
set g d
mul g e
sub g b
jnz g 2
set f 0
sub e -1
set g e
sub g b
jnz g -8
sub d -1
set g d
sub g b
jnz g -13
jnz f 2
sub h -1
set g b
sub g c
jnz g 2
jnz 1 3
sub b -17
jnz 1 -23`


trait Value =
	Def Value(): Long
	Def Get(): Long

class RegisterValue: Value =

	var registers: Long[]
	var register: Int

	Def new(registers: Long[], register: Int) =
		this.registers = registers
		this.register = register

	Def Get(): Long = registers[register]
	Def Value(): Long = register
	Def toString() = "Register(" + register + ")"


class Number: Value =
	var v: Long

	Def new(v: Long) =
		this.v = v

	Def Get() = v
	Def Value() = v

	Def toString() = "Number(" + v + ")"


trait Instruction =
	Def Execute(pc: Int): Int

class SET: Instruction =

	var registers: Long[]
	var x: Int
	var y: Value

	Def new(registers: Long[], x: Long, y: Value) =
		this.registers = registers
		this.x = x as Int
		this.y = y


	Def Execute(pc: Int): Int =
		registers[x] = y.Get()
		pc + 1

	Def toString() = "SET(" + x + ", " + y + ")"

class SUB: Instruction =

	var registers: Long[]
	var x: Int
	var y: Value

	Def new(registers: Long[], x: Long, y: Value) =
		this.registers = registers
		this.x = x as Int
		this.y = y

	Def Execute(pc: Int): Int =
		registers[x] -= y.Get()
		pc + 1

	Def toString() = "ADD(" + x + ", " + y + ")"

class MUL: Instruction =

	Var static Count = 0

	var registers: Long[]
	var x: Int
	var y: Value

	Def new(registers: Long[], x: Long, y: Value) =
		this.registers = registers
		this.x = x as Int
		this.y = y

	Def Execute(pc: Int): Int =
		Count++
		registers[x] *= y.Get()
		pc + 1

	Def toString() = "MUL(" + x + ", " + y + ")"

class JNZ: Instruction =

	var x: Value
	var y: Value

	Def new(x: Value, y: Value) =
		this.x = x
		this.y = y

	Def Execute(pc: Int): Int =
		val xVal = x.Get() as Int
		if(xVal == 0)
			return pc + 1

		val yVal = y.Get() as Int
		pc + yVal

	Def toString() = "JGZ(" + x + ", " + y + ")"


Def CreateRegisters(lines: String[]) =
	var numRegisters = 0;
	for(val line in lines)
		val x = line.Split(" ")[1][0] - 'a'
		numRegisters = Math.max(numRegisters, x + 1)

	new Long[numRegisters]


Def ParseInstructions(registers: Long[], lines: String[]) =
	val instructions = new Instruction[lines.Size()]
	for(var i = 0; i < lines.Size(); i++)
		val s = lines[i].Split(" ")
		val instruction = s[0]
		val values = new Value[s.Size() - 1]
		for(var x = 1; x < s.Size(); x++)
		    values[x - 1] = GetValue(registers, s[x])
		instructions[i] = instruction == "set" ? new SET(registers, values[0].Value(), values[1]) :
		                  instruction == "sub" ? new SUB(registers, values[0].Value(), values[1]) :
		                  instruction == "mul" ? new MUL(registers, values[0].Value(), values[1]) :
		                  instruction == "jnz" ? new JNZ(values[0], values[1]) :
		                  null
	instructions


Def GetValue(registers: Long[], s: String) =
	val c = s[0] - 'a'
	val isRegister = s.Size() == 1 && (c >= 0 && c < registers.Size())
	isRegister ? new RegisterValue(registers, c) : new Number(s.ToInt())


/* ----------------------------------------------------------------------------------- */


val lines = input.Split("\r?\n")
val registers = CreateRegisters(lines)
val instructions = ParseInstructions(registers, lines)

var pc = 0
while(pc >= 0 && pc < instructions.Size())
	pc = instructions[pc].Execute(pc)

println(MUL.Count) // res: 5929