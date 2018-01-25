import T::std::HashMap
/*
Begin in state A.
Perform a diagnostic checksum after 12481997 steps.

In state A:
  If the current value is 0:
    - Write the value 1.
    - Move one slot to the right.
    - Continue with state B.
  If the current value is 1:
    - Write the value 0.
    - Move one slot to the left.
    - Continue with state C.

In state B:
  If the current value is 0:
    - Write the value 1.
    - Move one slot to the left.
    - Continue with state A.
  If the current value is 1:
    - Write the value 1.
    - Move one slot to the right.
    - Continue with state D.

In state C:
  If the current value is 0:
    - Write the value 0.
    - Move one slot to the left.
    - Continue with state B.
  If the current value is 1:
    - Write the value 0.
    - Move one slot to the left.
    - Continue with state E.

In state D:
  If the current value is 0:
    - Write the value 1.
    - Move one slot to the right.
    - Continue with state A.
  If the current value is 1:
    - Write the value 0.
    - Move one slot to the right.
    - Continue with state B.

In state E:
  If the current value is 0:
    - Write the value 1.
    - Move one slot to the left.
    - Continue with state F.
  If the current value is 1:
    - Write the value 1.
    - Move one slot to the left.
    - Continue with state C.

In state F:
  If the current value is 0:
    - Write the value 1.
    - Move one slot to the right.
    - Continue with state D.
  If the current value is 1:
    - Write the value 1.
    - Move one slot to the right.
    - Continue with state A.
*/

val STEPS = 12_481_997
val TAPE_SIZE = 12_000

class State =

	Var value: Int[]
	Var direction: String[]
	Var nextState: Char[]

	Def new(value: Int[], direction: String[], nextState: Char[]) =
		this.value = value
		this.direction = direction
		this.nextState = nextState

	Def toString() = "State(" + value + ", " + direction + ", " + nextState + ")"

val states: HashMap<Char, State> = [
    ['A',  new State([1, 0], ["right", "left"],  ['B', 'C'])],
    ['B',  new State([1, 1], ["left",  "right"], ['A', 'D'])],
    ['C',  new State([0, 0], ["left",  "left"],  ['B', 'E'])],
    ['D',  new State([1, 0], ["right", "right"], ['A', 'B'])],
    ['E',  new State([1, 1], ["left",  "left"],  ['F', 'C'])],
    ['F',  new State([1, 1], ["right", "right"], ['D', 'A'])],
]


val tape = new Int[TAPE_SIZE]

var pos = TAPE_SIZE / 2
var state = states['A']

for(var i = 0; i < STEPS; i++)
	val currentValue = tape[pos]
	tape[pos] = state.value[currentValue]
	pos += (state.direction[currentValue] == "left" ? -1 : 1)
	state = states[state.nextState[currentValue]]

var checksum = 0
for(var i = 0; i < TAPE_SIZE; i++)
	checksum += tape[i]

println(checksum) // res: 3362
