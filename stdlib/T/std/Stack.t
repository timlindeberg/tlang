package T::std

import T::std::Collection

/**
* An unlimited size stack that supports the operations Enqueue and Dequeue.
*/
trait Stack<T>: Collection<T> =

	/**
	* Puts a value in top of the stack.
	* @param value the value to insert put on the stack
	*/
	Def Push(value: T): Unit

	/**
	* Removes and returns the value in the top of the stack.
	* @return the value in the top of the stack
	* @throws a null pointer exception if the stack is empty
	*/
	Def Pop(): T

	/**
	* Retrieves but does not remove the value from the top of the stack.
	* @return the value in the front of the queue or null if the stack is empty
	*/
	Def Peek(): T?
