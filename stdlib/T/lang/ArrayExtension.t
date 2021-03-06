package T::lang

import java::lang::Integer
import java::lang::Long
import java::lang::Float
import java::lang::Double
import java::lang::StringBuilder
import java::util::Locale

import T::std::Iterable
import T::std::Iterator

extension<T> T[]: List<T> =

	@CompilerGenerated
	Def Size(): Int = ;
	@CompilerGenerated
	Def [](index: Int): T = ;
	@CompilerGenerated
	Def []=(index: Int, value: T): T = ;

	Def +(arr: T[], v: T): T[] =
		val n = arr.Size()
		val newArr = new T[n + 1]
		for(var i = 0; i < n; i++)
			newArr[i] = arr[i]
		newArr[n] = v
		newArr


	Def +(v: T, arr: T[]): T[] =
		val n = arr.Size()
		val newArr = new T[n + 1]
		newArr[0] = v
		for(var i = 0; i < n; i++)
			newArr[i + 1] = arr[i]
		newArr

	Def ==(lhs: T[], rhs: T[]): Bool =
		if(lhs.Size() != rhs.Size())
			return false

		for(var i = 0; i < lhs.Size(); i++)
			if(lhs[i] != rhs[i])
				return false
		true

	Def !=(lhs: List<T>, rhs: List<T>): Bool = !(lhs == rhs)

	Def #(arr: T[]): Int =
		var res = 0
		for(var i = 0; i < arr.Size(); i++)
			res = 31 * res ^ #arr[i]
		res

	Def [:](start: Int?, end: Int?, step?): T[] =
		val newArr = new T[(end - start + step - 1) / step]
		for(var i = start; i < end; i += step)
		  newArr[(i - start) / step] = this[i]
		newArr

	Def IsEmpty() = Size() == 0
	Def NonEmpty() = Size() > 0

	Def First(): T = this[0]
	Def Last(): T = this[Size() - 1]

	Def IndexOf(value: T): Int =
		for(var i = 0; i < Size(); i++)
			if(this[i] == value)
				return i
		-1

	Def LastIndexOf(value: T): Int =
		var res = -1
		for(var i = 0; i < Size(); i++)
			if(this[i] == value)
				res = i
		res

	Def Contains(value: T): Boolean = IndexOf(value) != -1

	Def ContainsAll(elements: Collection<T>): Bool =
		for(val e in elements)
			if(!Contains(e))
				return false
		true

	Def ToArray(): T[] = this

	Def Iterator(): Iterator<T> = new ArrayIterator<T>()

	Def toString(): String = IsEmpty() ? "[]" : "[ " + MakeString(", ") + " ]"

	Def MakeString(delimiter: String): String =
		var s = ""
		var n = Size()
		for(var i = 0; i < n; i++)
			s += this[i]
			if(i < n - 1)
			   s += delimiter
		s

class ArrayIterator<T> : Iterator<T> =

	var array: T[]
	var i = 0

	Def new(array: T[]) = (this.array = array)

	Def HasNext() = i < array.Size()
	Def Next() = array[i++]
