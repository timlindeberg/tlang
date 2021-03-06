package T::std
import T::std::List

trait Iterator<T> =

	/**
	* Returns the next element pointed to by the iterator and advances
	* the iterator. It is only valid to call this method if HasNext()
	* returns true.
	* @return the next element of the iterator
	*/
	Def Next(): T

	/**
	* Returns true if the iterator contains another element.
	* @return true if the iterator contains another element; false otherwise.
	*/
	Def HasNext(): Bool
