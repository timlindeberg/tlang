import java::lang::Math

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

trait Instruction =
	Def Execute(pc: Int): Int


class SND: Instruction =


	var registers: Int[]
	var x: Int
	Var static Last = 0

	Def new(registers: Int[], x: Int) =
		this.registers = registers
		this.x = x

	Def Execute(pc: Int): Int =
		Last = registers[x]
		pc + 1

class SET: Instruction =

	var registers: Int[]
	var x: Int
	var y: Int

	Def new(registers: Int[], x: Int, y: Int) =
		this.registers = registers
		this.x = x
		this.y = y

	Def Execute(pc: Int): Int =
		registers[x] = y
		pc + 1

class ADD: Instruction =

	var registers: Int[]
	var x: Int
	var y: Int

	Def new(registers: Int[], x: Int, y: Int) =
		this.registers = registers
		this.x = x
		this.y = y

	Def Execute(pc: Int): Int =
		registers[x] += y
		pc + 1

class MUL: Instruction =

	var registers: Int[]
	var x: Int
	var y: Int

	Def new(registers: Int[], x: Int, y: Int) =
		this.registers = registers
		this.x = x
		this.y = y

	Def Execute(pc: Int): Int =
		registers[x] *= y
		pc + 1

class MOD: Instruction =

	var registers: Int[]
	var x: Int
	var y: Int

	Def new(registers: Int[], x: Int, y: Int) =
		this.registers = registers
		this.x = x
		this.y = y

	Def Execute(pc: Int): Int =
		registers[x] %= y
		pc + 1

class RCV: Instruction =

	var registers: Int[]
	var x: Int

	Def new(registers: Int[], x: Int) =
		this.registers = registers
		this.x = x

	Def Execute(pc: Int): Int =
		if(registers[x] > 0)
			println("RCV: " + SND.Last)
		pc + 1

class JGZ: Instruction =

	var registers: Int[]
	var x: Int
	var y: Int

	Def new(registers: Int[], x: Int, y: Int) =
		this.registers = registers
		this.x = x
		this.y = y

	Def Execute(pc: Int): Int = registers[x] > 0 ? pc + y : pc + 1

val lines = input.Split("\r?\n")

var numRegisters = 0;
for(val line in lines)
	val x = line.Split(" ")[1][0] - 'a'
	numRegisters = Math.max(numRegisters, x)

val registers = new Int[numRegisters]
val instructions = new Instruction[lines.Size()]
for(var i = 0; i < lines.Size(); i++)
	val s = lines[i].Split(" ")
	val instruction = s[0]
	instructions[i] = instruction == "snd" ? new SND(registers, s[1].ToInt()) :
	                  instruction == "set" ? new SET(registers, s[1][0] - 'a', s[2].ToInt()) :
	                  instruction == "add" ? new ADD(registers, s[1][0] - 'a', s[2].ToInt()) :
	                  instruction == "mul" ? new MUL(registers, s[1][0] - 'a', s[2].ToInt()) :
	                  instruction == "mod" ? new MOD(registers, s[1][0] - 'a', s[2].ToInt()) :
	                  instruction == "rcv" ? new RCV(registers, s[1][0] - 'a') :
	                  instruction == "jgz" ? new JGZ(registers, s[1].ToInt(), s[2].ToInt()) : new SND(registers, -1)

var pc = 0
while(pc >= 0 && pc <= instructions.Size())
	pc = instructions[pc].Execute(pc)

