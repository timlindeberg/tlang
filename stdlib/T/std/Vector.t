package T::std

import T::std::List
import T::std::Collection
import T::std::Iterator

class Vector<T>: List<T> {

	val static InitialCapacity = 10
	var size = 0
	var data: T[]

	Def new() = Clear()

	Def new(elements: Collection<T>) = {
		Clear()
		AddAll(elements)
	}

	Def implicit new(array: T[]) = {
		Clear()
		for(val e in array)
		    Add(e)
	}

	Def new(initSize: Int, value: T) = {
		size = initSize
		data = new T[size]
		for(var i = 0; i < size; i++)
			data[i] = value
	}

    Def Get(index: Int) = data[index]
    Def Set(index: Int, value: T) = (data[index] = value)

	Def Size() = size

	Def Clear() = {
		size = 0
		data = new T[InitialCapacity]
	}

	Def Iterator(): Iterator<T> = new VectorIterator<T>(this)

	Def Add(index: Int, value: T) = {
		if(index != size)
			checkBounds(index)

		if(size >= data.Size())
			increaseStorage()

		for(var i = size; i > index; i--)
			data[i] = data[i - 1]

		data[index] = value
		size++
	}

	Def AddAll(index: Int, elements: Collection<T>) = {
		if(index != size)
        	checkBounds(index)

        val numElements = elements.Size()

		val newData = new T[size + numElements]

        val it = elements.Iterator()
		for(var i = 0; i < index; i++)
			newData[i] = data[i]

		for(var i = 0; i < numElements; i++)
			newData[i + index] = it.Next()

		for(var i = 0; i < size - index; i++ )
			newData[i + index + numElements] = data[index + i]

		data = newData
		size += numElements
	}

	Def RemoveIndex(index: Int) = {
		for(var i = index; i < size; i++)
			data[i] = data[i + 1]

		size--
	}

    Def toString() = IsEmpty() ? "[]" : "[ " + MakeString(", ") + " ]"

    /**
    * Increases the size of the underlying storage.
    */
	def increaseStorage() = {
		val newData = new T[(data.Size() * 3) / 2 + 1]

		for(var i = 0; i < data.Size(); i++)
			newData[i] = data[i]
		data = newData
	}

    def checkBounds(index: Int) =
        if(index < 0 || index >= size)
            outOfBoundsError(index)

    def outOfBoundsError(index: Int) = error("Index out of bounds: " + index + " (size: " + size + ")")

}

class VectorIterator<T>: Iterator<T> {
	var current: Int
	var hasNext: Bool
	var vector: Vector<T>

	Def new(list: Vector<T>) = {
		current = 0
		hasNext = list.NonEmpty()
		vector = list
	}

	Def HasNext() = hasNext

	Def Next() = {
		hasNext = current < vector.Size() - 1
		vector[current++]
	}

}