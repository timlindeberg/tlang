package T::std

import T::std::Collection

/**
* An unlimited size queue that supports the operations Enqueue, Dequeue and Poll.
*/
trait Queue<T>: Collection<T> =

	/**
	* Puts a value in the back of the queue.
	* @param value the value to insert into the queue
	*/
	Def Enqueue(value: T): Unit

	/**
	* Removes and returns the item in the front of the queue.
	* @return the value in the front of the queue
	* @throws a null pointer exception if the queue is empty
	*/
	Def Dequeue(): T

	/**
	* Retrieves but does not remove the item in the front of the queue.
	* @return the value in the front of the queue
	*/
	Def Poll(): T?
