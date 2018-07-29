package T::std

trait MapEntry<K, V>  =

	Def Key(): K
	Def Value(): V

	Def toString(): String = "(" + Key() + " -> " + Value() + ")"

	Def ==(lhs: MapEntry<K, V>, rhs: MapEntry<K, V>): Bool = (lhs.Key() == rhs.Key() && lhs.Value() == rhs.Value())
	Def #(entry: MapEntry<K, V>): Int = #entry.Key() ^ #entry.Value()
