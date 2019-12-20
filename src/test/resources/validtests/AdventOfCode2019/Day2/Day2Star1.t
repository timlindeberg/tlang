import T::std::Vector
val input = "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,10,1,19,1,19,6,23,2,13,23,27,1,27,13,31,1,9,31,35,1,35,9,39,1,39,5,43,2,6,43,47,1,47,6,51,2,51,9,55,2,55,13,59,1,59,6,63,1,10,63,67,2,67,9,71,2,6,71,75,1,75,5,79,2,79,10,83,1,5,83,87,2,9,87,91,1,5,91,95,2,13,95,99,1,99,10,103,1,103,2,107,1,107,6,0,99,2,14,0,0"

val program = new Vector<Int>()

for(val x in input.Split(","))
	program.Add(x.ToInt())

program[1] = 12
program[2] = 2
var pc = 0
while(true)
	val instruction = program[pc]
	if(instruction == 99)
		break

	val i1 = program[pc + 1]
	val i2 = program[pc + 2]
	val i3 = program[pc + 3]
	if(instruction == 1)
		program[i3] = program[i1] + program[i2]
	else if(instruction == 2)
		program[i3] = program[i1] * program[i2]
	else
		error("Unknown instruction: " + instruction)
	pc += 4

println(program[0]) // res: 2842648
