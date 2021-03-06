package T::std

import T::std::Collection
import T::std::Vector

/**
* An ordered collection of elements.
*/
trait List<T>: Collection<T> =

	/**
	* Returns the element specified by the given index.
	* @param index the index of the element to return
	* @return      the element specified by the given index
	*/
	Def Get(index: Int): T

	/**
	* Sets the element specified by the given index.
	* @param index the position to set the value of
	* @param value the value to set
	*/
	Def Set(index: Int, value: T): Unit

	/**
	* Adds the element value at the position specified by the given index.
	* @param index the position to add the new element at
	* @param value the value to add
	*/
	Def Add(index: Int, value: T): Unit

	/**
	* Removes the element at the given index.
	* @param index the position to remove
	*/
	Def RemoveIndex(index: Int): Unit

	/**
	* Returns the first found index containing the given value.
	* @param value the value to find the index of
	* @return      the first found index containing the given value or -1 if the list
	*              does not contain the value
	*/
	Def IndexOf(value: T): Int =
		var i = 0
		for(val e in this)
			if(e == value)
				return i
			i++

		-1

	/**
	* Returns the last found index containing the given value.
	* @param value the value to find the index of
	* @return      the last found index containing the given value or -1 if the list
	*              does not contain the value
	*/
	Def LastIndexOf(value: T): Int =
		var res = -1
		var i = 0
		for(val e in this)
			if(e == value)
				res = i
			i++

		res

	//------------------------------------------------------------
	// Default methods
	//------------------------------------------------------------

	/**
	* Adds the given value to the start of the list.
	* @param value the value to add
	*/
	Def AddFirst(value: T): Unit = Add(0, value)

	/**
	* Adds the given value to the end of the list.
	* @param value the value to add
	*/
	Def Add(value: T): Unit = Add(Size(), value)

	/**
	* Adds all the elements in the given list at the position specified by the given index.
	* @param index the position to add the new elements at
	* @param list  the list of values to add
	*/
	Def AddAll(index: Int, list: Collection<T>): Unit =
		for(val e in list)
			Add(index, e)

	/**
	* Returns the first element of this list.
	* @return the first element of this list
	*/
	Def First(): T = Get(0)

	/**
	* Returns the last element of this list.
	* @return the last element of this list
	*/
	Def Last(): T = Get(Size() - 1)

	/**
	* Returns true if the list contains the given element.
	* @param value the value to check
	* @return      true if the list contains the given element; false otherwise
	*/
	Def Contains(value: T) = IndexOf(value) != -1

	/**
	* Removes the given element from this list.
	* @param value the element to remove
	* @return      true if the element was removed; false otherwise
	*/
	Def Remove(value: T): Bool =
		val index = IndexOf(value)

		if(index == -1)
			return false

		RemoveIndex(index)
		true

	//------------------------------------------------------------
	// Operators
	//------------------------------------------------------------

	/**
	* Returns the element specified by the given index.
	* @param index the index of the element to return
	* @return      the element specified by the given index
	*/
	Def [](index: Int) = Get(index)

	/**
	* Sets the element specified by the given index.
	* @param index the position to set the value of
	* @param value the value to set
	*/
	Def []=(index: Int, value: T) = Set(index, value)

	/**
	* Creates a new list from this list containing every 'step':th
	* element in [start, end).
	* Example:
	* {1, 2, 3, 4, 5, 6}[1:4:2] == {2, 4}
	* @param start the starting position of the slice or null. Will be 0 if null.
	* @param end the end position of the slice or null. Will be 'Size()' if null.
	* @param step the step size of the slice or null. Will be 1 if null.
	*/
	Def [:](start: Int?, end: Int?, step: Int?): List<T> =
		val s  = start ?: 0
		val e  = end   ?: Size()
		val st = step  ?: 1
		val newList = new Vector<T>((e - s + st - 1) / st)
		for(var i = s; i < e; i += st)
			newList[(i - s) / st] = Get(i)

		newList

	Def ==(lhs: List<T>, rhs: List<T>): Bool =
		if(lhs.Size() != rhs.Size())
			return false

		val lhsIt = lhs.Iterator()
		val rhsIt = rhs.Iterator()
		while(lhsIt.HasNext())
			if(lhsIt.Next() != rhsIt.Next())
				return false

		true

	Def !=(lhs: List<T>, rhs: List<T>): Bool = !(lhs == rhs)
