package T::std

import T::std::List
import T::std::Collection
import T::std::Iterator

class LinkedList<T> : List<T> =

	var first: LinkedListNode<T>? = null
	var last:  LinkedListNode<T>? = null

	var size = 0

	Def new() = ;

	Def new(size: Int, value: T) =
		for(var i = 0; i < size; i++)
			Add(value)

	Def new(elements: Collection<T>) = AddAll(elements)

	Def implicit new(elements: T[]) =
		Clear()
		for(val e in elements)
			Add(e)

	Def Get(index: Int) =
		checkBounds(index)
		find(index).Value

	Def Set(index: Int, value: T) =
		checkBounds(index)
		find(index).Value = value

	Def Size() = size

	Def Clear() =
		first = null
		last = null
		size = 0

	Def Iterator(): Iterator<T> = new LinkedListIterator<T>(first)

	Def Add(index: Int, value: T) =
		if(index != size)
			checkBounds(index)

		val elem = new LinkedListNode<T>(value)
		if(size == 0)
			// Empty list
			first = elem
			last = first
		else if(index == size)
			// Insert at end of the list
			elem.Prev = last
			last!!.Next = elem // last can't be null since size != 0
			last = elem
		else if(index == 0)
			// Insert at beginning of the list
			elem.Next = first
			first!!.Prev = elem // first can't be null since size != 0
			first = elem
		else
			// Insert in the middle of the list
			val node = find(index)
			val prev = node.Prev

			elem.Prev = prev
			elem.Next = node

			prev!!.Next = elem  // prev can't be null since we're inserting in to the middle of the list
			node.Prev = elem
		size++

	Def AddAll(index: Int, elements: Collection<T>) =
		if(elements.IsEmpty())
			return

		checkBounds(index)

		var node = find(index)
		val post = node.Next
		for(val e in elements)
			val n = new LinkedListNode<T>(e)
			n.Prev = node
			node.Next = n
			node = n

		if(post)
			node.Next = post

	Def RemoveIndex(index: Int) =
		checkBounds(index)

		val node = find(index)
		val prev = node.Prev
		val next = node.Next
		if(prev && next)
			// LinkedListNode is in the middle of the list
			prev.Next = next
			next.Prev = prev
		else if(prev && !next)
			// LinkedListNode is last element
			prev.Next = null
			last = prev
		else if(!prev && next)
			// LinkedListNode is first element
			next.Prev = null
			first = next
		else
			// LinkedListNode is the only element
			Clear()
			size = 1

		size--

	Def FirstNode() = first
	Def LastNode() = last

	Def toString() = IsEmpty() ? "[]" : "[ " + MakeString(", ") + " ]"

	def find(index: Int) =
		var tmp: LinkedListNode<T>?
		if(index < (size / 2) + 1)
			tmp = first
			for(var i = 0; i < index; i++)
				tmp = tmp!!.Next
		else
			tmp = last
			for(var i = size - 1; i > index; i--)
				tmp = tmp!!.Prev

		// this is safe since bounds should be checked before calling find
		tmp!!

	def checkBounds(index: Int) =
		if(index < 0 || index >= size)
			outOfBoundsError(index)

	def outOfBoundsError(index: Int) = error("Index out of bounds: " + index + " (size: " + size + ")")

class LinkedListNode<T> =

	Var Value: T
	Var Next: LinkedListNode<T>? = null
	Var Prev: LinkedListNode<T>? = null

	Def new(v: T) = Value = v

class LinkedListIterator<T> : Iterator<T> =
	var current: LinkedListNode<T>
	var hasNext: Bool

	Def new(first: LinkedListNode<T>?) =
		hasNext = first != null
		if(hasNext)
			current = first

	Def HasNext() = hasNext

	Def Next() =
		val res = current.Value

		hasNext = current.Next != null
		if(hasNext)
			current = current.Next
		res