package T::std

/**
* An object to compare elements.
*/
trait Comparator<T> =

	/**
	* Compares two elements to each other.
	* @param a the first element to compare
	* @param b the second element to compare
	* @return  a negative number if a < b
	*          a positive number if a > b
	*          or zero of a == b
	*/
	Def Compare(a: T, b: T): Int