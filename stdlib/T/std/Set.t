package T::std

import T::std::Collection

/**
* A set of elements.
*/
trait Set<T>: Collection<T> =

	Def [](value: T): Bool = Contains(value)
