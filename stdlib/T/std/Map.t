package T::std

import T::std::MapEntry
import T::std::Iterator
import T::std::Collection

trait Map<K, V>: Collection<MapEntry<K, V>> =

	Def Add(key: K, value: V): Unit
	Def Get(key: K): V?
	Def Remove(key: K): Bool

	Def Iterator(): Iterator<MapEntry<K, V>>

	Def Keys(): Iterator<K>
	Def Values(): Iterator<V>

	//------------------------------------------------------------
	// Default methods
	//------------------------------------------------------------

	Def Add(entry: MapEntry<K, V>): Unit = Add(entry.Key(), entry.Value())
	Def Remove(entry: MapEntry<K, V>): Bool = Remove(entry.Key())
	Def Contains(entry: MapEntry<K, V>): Bool =
		val v = Get(entry.Key())
		v && v == entry.Value()

	Def Contains(key: K): Bool = Get(key) != null
	Def ContainsValue(value: V): Bool =
		for(val e in this)
			if(e.Value() == value)
				return true
		false

	Def GetOrDefault(key: K, default: V): V =
		val v = Get(key)
		if(v) return v

		Add(key, default)
		default

	//------------------------------------------------------------
	// Operators
	//------------------------------------------------------------

	Def ==(lhs: Map<K, V>, rhs: Map<K, V>): Bool =
		if(lhs.Size() != rhs.Size())
			return false

		for(val entry in lhs)
			val v = rhs.Get(entry.Key())
			if(!v || v != entry.Value())
				return false

		true

	Def !=(lhs: Map<K, V>, rhs: Map<K, V>): Bool = !(lhs == rhs)

	Def []=(key: K, value: V): Unit = Add(key, value)
	Def [](key: K): V =
		val res = Get(key)

		if(!res)
			error("No such key: " + key)

		res!!
