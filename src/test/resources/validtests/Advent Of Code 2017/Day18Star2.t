// ignore
// This is too slow to be used as a test

import java::util::concurrent::ArrayBlockingQueue
import java::lang::Math
import java::lang::Thread
import java::lang::Runnable
import T::std::Queue
import T::std::LinkedList

val _testInput = `snd 1
snd 2
snd p
rcv a
rcv b
rcv c
rcv d`

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

	var program: Program
	var x: Value
	Var static Last = 0L

	Def new(program: Program, x: Value) =
		this.program = program
		this.x = x

	Def Execute(pc: Int): Int =
		val value = x.Get()
		program.Send(value)
		pc + 1

	Def toString() = "SND(" + x + ")"

class SET: Instruction =

	var program: Program
	var x: Int
	var y: Value

	Def new(program: Program, x: Long, y: Value) =
		this.program = program
		this.x = x as Int
		this.y = y


	Def Execute(pc: Int): Int =
		program.registers[x] = y.Get()
		pc + 1

	Def toString() = "SET(" + x + ", " + y + ")"

class ADD: Instruction =

	var program: Program
	var x: Int
	var y: Value

	Def new(program: Program, x: Long, y: Value) =
		this.program = program
		this.x = x as Int
		this.y = y

	Def Execute(pc: Int): Int =
		program.registers[x] += y.Get()
		pc + 1

	Def toString() = "ADD(" + x + ", " + y + ")"

class MUL: Instruction =

	var program: Program
	var x: Int
	var y: Value

	Def new(program: Program, x: Long, y: Value) =
		this.program = program
		this.x = x as Int
		this.y = y

	Def Execute(pc: Int): Int =
		program.registers[x] *= y.Get()
		pc + 1

	Def toString() = "MUL(" + x + ", " + y + ")"

class MOD: Instruction =

	var program: Program
	var x: Int
	var y: Value

	Def new(program: Program, x: Long, y: Value) =
		this.program = program
		this.x = x as Int
		this.y = y

	Def Execute(pc: Int): Int =
		program.registers[x] %= y.Get()
		pc + 1

	Def toString() = "MOD(" + x + ", " + y + ")"

class RCV: Instruction =

	var program: Program
	var x: Int

	Def new(program: Program, x: Long) =
		this.program = program
		this.x = x as Int

	Def Execute(pc: Int): Int =

		program.Receiving = true
		while(true)
			Thread.sleep(1)
			var value = program.Poll()
			if(value)
				program.registers[x] = value
				break


			if(program.other!!.IsWaiting())
				Program.DEADLOCK = true
				break
		program.Receiving = false
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

class Program =

	Var static DEADLOCK = false

	Var Num: Int
	Var ValuesSent = 0
	Var registers: Long[]
	var instructions: Instruction[]
	var pc = 0
	Var other: Program? =  null
	var queue: ArrayBlockingQueue = new ArrayBlockingQueue(10000)
	Var Receiving = false
	Var Terminated = false

	Def new(num: Int, lines: String[]) =
		this.Num = num
		this.registers = CreateRegisters(lines)
		this.instructions = ParseInstructions(lines)

	Def Execute() =
		var pc = 0
		while(pc >= 0 && pc < instructions.Size())

			pc = instructions[pc].Execute(pc)
			if(DEADLOCK)
				break
		Terminated = true

	Def IsWaiting() = (queue.size() == 0 && Receiving) || Terminated

	Def SetOther(program: Program) = (other = program)

	Def Send(value: Long) =
		other!!.queue.add(value)
		ValuesSent++

	Def Poll(): Long? = (queue.size() == 0 ? null : queue.poll() as Long)

	Def CreateRegisters(lines: String[]) =
		var numRegisters = 0;
		for(val line in lines)
			val x = line.Split(" ")[1][0] - 'a'
			numRegisters = Math.max(numRegisters, x + 1)

		val registers = new Long[numRegisters]
		registers[numRegisters - 1] = Num
		registers

	Def ParseInstructions(lines: String[]) =
		val instructions = new Instruction[lines.Size()]
		for(var i = 0; i < lines.Size(); i++)
			val s = lines[i].Split(" ")
			val instruction = s[0]
			val values = new Value[s.Size() - 1]
			for(var x = 1; x < s.Size(); x++)
			    values[x - 1] = GetValue(registers, s[x])
			instructions[i] = instruction == "set" ? new SET(this, values[0].Value(), values[1]) :
			                  instruction == "add" ? new ADD(this, values[0].Value(), values[1]) :
			                  instruction == "mul" ? new MUL(this, values[0].Value(), values[1]) :
			                  instruction == "mod" ? new MOD(this, values[0].Value(), values[1]) :
			                  instruction == "snd" ? new SND(this, values[0]) :
			                  instruction == "rcv" ? new RCV(this, values[0].Value()) :
			                  instruction == "jgz" ? new JGZ(values[0], values[1]) :
			                  null
		instructions


	Def GetValue(registers: Long[], s: String) =
		val c = s[0] - 'a'
		val isRegister = s.Size() == 1 && (c >= 0 && c < registers.Size())
		isRegister ? new RegisterValue(registers, c) : new Number(s.ToInt())

class ProgramExecution : Runnable =

	var program: Program

	Def new(program: Program) =
		this.program = program

	Def run() = program.Execute()

/* ----------------------------------------------------------------------------------- */


val lines = input.Split("\r?\n")

val program0 = new Program(0, lines)
val program1 = new Program(1, lines)

program0.SetOther(program1)
program1.SetOther(program0)

val t1 = new Thread(new ProgramExecution(program0))
val t2 = new Thread(new ProgramExecution(program1))

t1.start()
t2.start()

t1.join()
t2.join()

println(program1.ValuesSent) // res: 7239
