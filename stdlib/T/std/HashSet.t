package T::std

import T::std::Set
import T::std::HashMap
import T::std::Iterator

class HashSet<T>: Set<T> =

	var map: HashMap<T, Bool>

	Def new()                                         = map = new HashMap<T, Bool>()
	Def new(initialCapacity: Int)                     = map = new HashMap<T, Bool>(initialCapacity)
	Def new(initialCapacity: Int, loadFactor: Double) = map = new HashMap<T, Bool>(initialCapacity, loadFactor)
	Def implicit new(entries: T[]) =
		map = new HashMap<T, Bool>()
		for(val e in entries)
			Add(e)


	Def Size(): Int = map.Size()
	Def Clear(): Unit = map.Clear()

	Def Add(value: T): Unit = map[value] = true
	Def Remove(value: T): Bool = map.Remove(value)

	Def Contains(value: T): Bool = map.Contains(value)
	Def Iterator(): Iterator<T> = map.Keys()

	Def toString(): String = IsEmpty() ? "{}" : "{ " + MakeString(", ") + " }"
