import java::lang::Math

val _testInput = `set a 1
add a 2
mul a a
mod a 5
snd a
set a 0
rcv a
jgz a -1
set a 1
jgz a -2`

val input = `set i 31
set a 1
mul p 17
jgz p p
mul a 2
add i -1
jgz i -2
add a -1
set i 127
set p 735
mul p 8505
mod p a
mul p 129749
add p 12345
mod p a
set b p
mod b 10000
snd b
add i -1
jgz i -9
jgz a 3
rcv b
jgz b -1
set f 0
set i 126
rcv a
rcv b
set p a
mul p -1
add p b
jgz p 4
snd a
set a b
jgz 1 3
snd b
set f 1
add i -1
jgz i -11
snd a
jgz f -16
jgz a -19`


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

class SND: Instruction =

	var registers: Long[]
	var x: Int
	Var static Last = 0L

	Def new(registers: Long[], x: Long) =
		this.registers = registers
		this.x = x as Int

	Def Execute(pc: Int): Int =
		Last = registers[x]
		pc + 1

	Def toString() = "SND(" + x + ")"

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

class ADD: Instruction =

	var registers: Long[]
	var x: Int
	var y: Value

	Def new(registers: Long[], x: Long, y: Value) =
		this.registers = registers
		this.x = x as Int
		this.y = y

	Def Execute(pc: Int): Int =
		registers[x] += y.Get()
		pc + 1

	Def toString() = "ADD(" + x + ", " + y + ")"

class MUL: Instruction =

	var registers: Long[]
	var x: Int
	var y: Value

	Def new(registers: Long[], x: Long, y: Value) =
		this.registers = registers
		this.x = x as Int
		this.y = y

	Def Execute(pc: Int): Int =
		registers[x] *= y.Get()
		pc + 1

	Def toString() = "MUL(" + x + ", " + y + ")"

class MOD: Instruction =

	var registers: Long[]
	var x: Int
	var y: Value

	Def new(registers: Long[], x: Long, y: Value) =
		this.registers = registers
		this.x = x as Int
		this.y = y

	Def Execute(pc: Int): Int =
		registers[x] %= y.Get()
		pc + 1

	Def toString() = "MOD(" + x + ", " + y + ")"

class RCV: Instruction =

	Var static RECIEVED = false

	var registers: Long[]
	var x: Int


	Def new(registers: Long[], x: Long) =
		this.registers = registers
		this.x = x as Int

	Def Execute(pc: Int): Int =
		if(registers[x] > 0)
			RECIEVED = true
		pc + 1

	Def toString() = "RCV(" + x + ")"

class JGZ: Instruction =

	var x: Value
	var y: Value

	Def new(x: Value, y: Value) =
		this.x = x
		this.y = y

	Def Execute(pc: Int): Int =
		val xVal = x.Get() as Int
		val yVal = y.Get() as Int

		(xVal > 0 ? pc + yVal : pc + 1)

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
		                  instruction == "add" ? new ADD(registers, values[0].Value(), values[1]) :
		                  instruction == "mul" ? new MUL(registers, values[0].Value(), values[1]) :
		                  instruction == "mod" ? new MOD(registers, values[0].Value(), values[1]) :
		                  instruction == "snd" ? new SND(registers, values[0].Value()) :
		                  instruction == "rcv" ? new RCV(registers, values[0].Value()) :
		                  instruction == "jgz" ? new JGZ(values[0], values[1]) :
		                  new SND(registers, -1)
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
while(pc >= 0 && pc <= instructions.Size())
	pc = instructions[pc].Execute(pc)
	if(RCV.RECIEVED)
		println("RCV: " + SND.Last) // res: RCV: 8600
		break
