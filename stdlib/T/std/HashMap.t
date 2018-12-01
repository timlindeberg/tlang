package T::std

import T::std::Map
import T::std::MapEntry
import T::std::Iterator
import T::std::Iterable

class HashMap<K, V>: Map<K, V> =

	val static DEFAULT_INITIAL_CAPACITY = 16
	val static DEFAULT_LOAD_FACTOR = 0.75
	val static MAXIMUM_CAPACITY = 1 << 30;

	var entries: HashMapEntry<K, V>?[]
	var size: Int
	var loadFactor: Double

	Def new()                                         = init(DEFAULT_INITIAL_CAPACITY, DEFAULT_LOAD_FACTOR)
	Def new(initialCapacity: Int)                     = init(initialCapacity, DEFAULT_LOAD_FACTOR)
	Def new(initialCapacity: Int, loadFactor: Double) = init(initialCapacity, loadFactor)
	Def implicit new(entries: MapEntry<K, V>[]) =
		init(DEFAULT_INITIAL_CAPACITY, DEFAULT_LOAD_FACTOR)
		for(val e in entries)
			Add(e)

	Def implicit new(entries: Object[][]) =
		init(DEFAULT_INITIAL_CAPACITY, DEFAULT_LOAD_FACTOR)
		for(val e in entries)
			if(e.Size() != 2)
				error("Invalid size of map entry: " + e.Size() + ", expected 2.")

			if(!(e[0] is K))
				error("Entry has wrong key type!")

			if(!(e[1] is V))
				error("Entry has wrong value type!")

			Add(e[0] as K, e[1] as V)

	Def new(map: HashMap<K, V>) =
		init(map.Capacity(), map.loadFactor)
		AddAll(map)

	Def new(map: Map<K, V>) =
		init(DEFAULT_INITIAL_CAPACITY, DEFAULT_LOAD_FACTOR)
		AddAll(map)

	Def Size() = size
	Def Capacity() = entries.Size()
	Def Clear() =
		entries = new HashMapEntry<K, V>?[DEFAULT_INITIAL_CAPACITY]
		size = 0

	Def Add(key: K, value: V) =
		if(size >= loadFactor * Capacity())
			resize()

		val hash = #key
		add(new HashMapEntry<K, V>(key, value, hash))

	Def Get(key: K) =
		val entry = entries[index(#key)]
		if(!entry)
			return null

		for(val e in entry)
			if(e.Key() == key)
				return e.Value()

		null

	Def Remove(key: K): Bool =
		val index = index(#key)
		val entry = entries[index]

		if(!entry)
			return false

		val it = entry.Iterator()
		var previous: HashMapEntry<K, V>? = null
		while(it.HasNext())
			val e = it.Next()
			if(e.Key() == key)
				break

			previous = e

		if(!previous)
			entries[index] = null
		else if(it.HasNext())
			previous.Next = it.Next()
		else
			previous.Next = null

		size--
		true

	Def Iterator() = new EntryIterator<K, V>(entries)
	Def Keys()     = new KeyIterator<K, V>(entries)
	Def Values()   = new ValueIterator<K, V>(entries)

	Def toString() = IsEmpty() ? "[]" : "[ " + MakeString(", ") + " ]"

	def init(initialCapacity: Int, loadFactor: Double) =
		this.loadFactor = loadFactor
		entries = new HashMapEntry<K, V>?[closestPowerOfTwo(initialCapacity)]
		size = 0

	def add(newEntry: HashMapEntry<K, V>) =
		val index = index(newEntry.Hash)
		val startingEntry = entries[index]

		if(!startingEntry)
			entries[index] = newEntry
			size++
			return

		var entry: HashMapEntry<K, V>? = null
		for(val v in startingEntry)
			if(v.Key() == newEntry.Key())
				v.SetValue(newEntry.Value())
				return
			entry = v

		// Will always iterate once since startingEntry was defined, hence entry will be defined
		entry!!.Next = newEntry
		size++

	def resize() =
		val newCapacity = entries.Size() << 1
		val it = new EntryIterator<K, V>(entries)
		entries = new HashMapEntry<K, V>?[newCapacity]
		size = 0
		while(it.HasNext())
			val entry = it.Next() as HashMapEntry<K, V>
			val newEntry = new HashMapEntry<K, V>(entry.Key(), entry.Value(), entry.Hash)
			add(newEntry)

	def index(hashCode: Int) = improvedHash(hashCode) & (Capacity() - 1)

	def improvedHash(hash: Int) = hash ^ (hash >> 16)

	def closestPowerOfTwo(capacity: Int) =
		var n = capacity - 1
		n |= n >> 1
		n |= n >> 2
		n |= n >> 4
		n |= n >> 8
		n |= n >> 16
		return (n < 0) ? 1 : (n >= MAXIMUM_CAPACITY) ? MAXIMUM_CAPACITY : n + 1

class HashMapEntry<K, V>: MapEntry<K, V>, Iterable<HashMapEntry<K, V>> =

	var key: K
	var value: V
	Var Hash: Int
	Var Next: HashMapEntry<K, V>?

	Def new(key: K, value: V)            = init(key, value, 0)
	Def new(key: K, value: V, hash: Int) = init(key, value, hash)

	Def Key() = key
	Def Value() = value

	Def SetKey(key: K)     = (this.key = key)
	Def SetValue(value: V) = (this.value = value)

	Def Iterator() = new EntryListIterator<K, V>(this)

	Def ==(lhs: HashMapEntry<K, V>, rhs: HashMapEntry<K, V>) =
		lhs.key == rhs.key &&
		lhs.value == rhs.value

	Def toString() = "(" + key + " -> " + value + ")"

	def init(key: K, value: V, hash: Int) =
		this.key = key
		this.value = value
		Hash = hash


class EntryListIterator<K, V>: Iterator<HashMapEntry<K, V>> =

	var hasNext: Bool
	var current: HashMapEntry<K, V>

	Def new(startEntry: HashMapEntry<K, V>) =
		hasNext = true
		current = startEntry

	Def HasNext() = hasNext

	Def Next() =
		val res = current
		hasNext = current.Next != null
		if(hasNext)
			current = current.Next!!
		res


class KeyIterator<K, V>: Iterator<K> =

	var it: EntryIterator<K, V>

	Def new(entries: HashMapEntry<K, V>?[]) =
		it = new EntryIterator<K, V>(entries)

	Def HasNext() = it.HasNext()
	Def Next() = it.Next().Key()


class ValueIterator<K, V>: Iterator<V> =

	var it: EntryIterator<K, V>

	Def new(entries: HashMapEntry<K, V>?[]) =
		it = new EntryIterator<K, V>(entries)

	Def HasNext() = it.HasNext()
	Def Next() = it.Next().Value()


class EntryIterator<K, V>: Iterator<MapEntry<K, V>> =

	var hasNext: Bool
	var currentEntry: HashMapEntry<K, V>?
	var currentIndex: Int
	var entries: HashMapEntry<K, V>?[]

	Def new(entries: HashMapEntry<K, V>?[]) =
		this.entries = entries
		currentIndex = 0
		currentEntry = nextDefinedEntry()
		hasNext = currentEntry != null

	Def HasNext() = hasNext

	Def Next() =
		val res = currentEntry!!
		currentEntry = nextEntry()
		hasNext = currentEntry != null
		res

	def nextEntry() = currentEntry?.Next ?: nextDefinedEntry()

	def nextDefinedEntry() =
		while(currentIndex < entries.Size())
			val entry = entries[currentIndex++]
			if(entry)
				return entry

		null
