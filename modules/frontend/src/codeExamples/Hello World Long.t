/*---------------------------------------------------------------------------*/
/*                             Hello World Long                              */
/*---------------------------------------------------------------------------*/

package t::lang

val helloWorld = new HelloWorld<Long>(1300L) +
   new HelloWorld<Long>(37L)

helloWorld.Print() // prints "Hello world!" 1337 times

class HelloWorld<T> =

	var times: T

	Def new(times: T?) =
		this.times = times ?: 1

	Def Print() =
		for(var i = 0; i < times; i++)
			println("Hello world!")

	Def +(lhs: HelloWorld<T>, rhs: HelloWorld<T>) =
		new HelloWorld(lhs.times + rhs.times)
