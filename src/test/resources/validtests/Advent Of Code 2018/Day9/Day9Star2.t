// Ignore
import T::std::Vector
import java::lang::Math
import T::std::HashMap


class Node =
	Var Next: Node? = null
	Var Prev: Node? = null

	Var Value: Int

	Def new(v: Int) = Value = v

	Def toString() = "[" + Prev!!.Value + " <- " + Value + " -> " + Next!!.Value + "]"

class Day9 =

	val numPlayers = 412
	val numMarbles = 71646 * 100

	var player = 1

	val start: Node = new Node(0)
	val scores = new Long[numPlayers]

	Def Run() =
		var n = new Node(1)
		start.Next = n
		start.Prev = n
		n.Next = start
		n.Prev = start


		for(var marble = 2; marble <= numMarbles; marble++)
			if(marble % 23 == 0)
				for(var i = 0; i < 7; i++)
					n = n.Prev
				scores[player] += marble + n.Value
				n.Prev!!.Next = n.Next
				n = n.Next
			else
				val newNode = new Node(marble)

				val next = n.Next!!
				newNode.Next = next.Next
				newNode.Prev = next
				next.Next = newNode
				next.Next!!.Next!!.Prev = newNode
				n = newNode
			player = (player + 1) % numPlayers


		var max = 0L
		for(val v in scores)
			max = Math.max(max, v)
		println(max)

	Def Print(current: Node) =
		print(start.Value + " ")
		var n = start.Next
		while(n != start)
			val v = n!!.Value
			if(n == current) print("(" + v + ") ")
			else             print(v + " ")
			n = n!!.Next
		println()

new Day9().Run() // res: 3562722971
